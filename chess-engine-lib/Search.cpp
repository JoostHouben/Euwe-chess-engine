#include "Search.h"

#include "TTable.h"

#include <algorithm>
#include <atomic>

namespace {

bool gRecordBestMove = false;
Move gBestMove;
std::atomic<bool> gStopSearch;

constexpr int kTTableSizeInBytes   = 64 * 1024 * 1024;
constexpr int kTTableSizeInEntries = kTTableSizeInBytes / sizeof(TTEntry);
TTable gTTable(kTTableSizeInEntries);

SearchStatistics gSearchStatistics;

void updateTTable(
        EvalT bestScore, EvalT alphaOrig, EvalT beta, Move bestMove, int depth, HashT hash) {
    ScoreType scoreType;
    if (bestScore <= alphaOrig) {
        scoreType = ScoreType::UpperBound;
    } else if (bestScore >= beta) {
        scoreType = ScoreType::LowerBound;
    } else {
        scoreType = ScoreType::Exact;
    }

    const TTEntry entry = {
            .hash      = hash,
            .depth     = (std::uint8_t)depth,
            .scoreType = scoreType,
            .score     = bestScore,
            .bestMove  = bestMove,
    };

    gTTable.store(entry);
}

[[nodiscard]] EvalT search(
        GameState& gameState,
        const int depth,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack) {
    if (gStopSearch) {
        return 0;
    }

    ++gSearchStatistics.nodesSearched;

    if (depth == 0) {
        return evaluate(gameState, stack);
    }

    const bool recordBestMove = gRecordBestMove;
    gRecordBestMove           = false;

    const EvalT alphaOrig = alpha;

    if (!recordBestMove && gameState.isForcedDraw(/* repetitionsForDraw = */ 1)) {
        // For a forced draw we can return 0 immediately.
        return 0;
    }

    Move bestMove;
    EvalT bestScore = -kInfiniteEval;

    // Probe the transposition table to update our score based on previous info.
    auto ttHit = gTTable.probe(gameState.getBoardHash());
    if (ttHit) {
        gSearchStatistics.tTableHits++;

        if (!recordBestMove && ttHit->depth >= depth) {
            if (ttHit->scoreType == ScoreType::Exact) {
                return ttHit->score;
            } else if (ttHit->scoreType == ScoreType::LowerBound) {
                alpha = std::max(alpha, ttHit->score);
            } else {
                beta = std::min(beta, ttHit->score);
            }

            // Check if we can return based on tighter bounds from the transposition table.
            if (alpha >= beta) {
                // If alpha was raised by the tt entry this is a lower bound and we want to return
                // that raised alpha (fail-soft: that's the tightest lower bound we have).
                // If beta was lowered by the tt entry this is an upper bound and we want to return
                // that lowered beta (fail-soft: that's the tightest upper bound we have).
                // So either way we return the tt entry score.
                return ttHit->score;
            }
        }

        // Try hash move first.
        // TODO: do we need a legality check here for hash collisions?
        auto unmakeInfo = gameState.makeMove(ttHit->bestMove);

        const EvalT score = -search(gameState, depth - 1, -beta, -alpha, stack);

        gameState.unmakeMove(ttHit->bestMove, unmakeInfo);

        if (gStopSearch) {
            return 0;
        }

        bestScore = score;
        bestMove  = ttHit->bestMove;

        if (bestScore > alpha) {
            alpha = bestScore;

            if (alpha >= beta) {
                updateTTable(bestScore, alphaOrig, beta, bestMove, depth, gameState.getBoardHash());

                if (recordBestMove) {
                    gBestMove = bestMove;
                }

                return bestScore;
            }
        }
    }

    auto moves = gameState.generateMoves(stack);
    if (moves.size() == 0) {
        return evaluateNoLegalMoves(gameState);
    }

    int moveIdx = 0;

    if (ttHit) {
        const auto hashMoveIt = std::find(moves.begin(), moves.end(), ttHit->bestMove);
        if (hashMoveIt != moves.end()) {
            std::iter_swap(moves.begin(), hashMoveIt);
            ++moveIdx;
        }
    }

    while (moveIdx < moves.size()) {
        selectBestMove(moves, moveIdx, gameState);

        const Move move = moves[moveIdx++];

        auto unmakeInfo = gameState.makeMove(move);

        const EvalT score = -search(gameState, depth - 1, -beta, -alpha, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (gStopSearch) {
            return 0;
        }

        if (score > bestScore) {
            bestScore = score;
            bestMove  = move;

            if (score > alpha) {
                alpha = score;

                if (alpha >= beta) {
                    break;
                }
            }
        }
    }

    updateTTable(bestScore, alphaOrig, beta, bestMove, depth, gameState.getBoardHash());

    if (recordBestMove) {
        gBestMove = bestMove;
    }

    return bestScore;
}

}  // namespace

SearchResult searchForBestMove(GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    gRecordBestMove = true;
    gStopSearch     = false;

    const EvalT eval = search(gameState, depth, -kInfiniteEval, kInfiniteEval, stack);

    return {.bestMove = gBestMove, .eval = eval};
}

void stopSearch() {
    gStopSearch = true;
}

SearchStatistics getSearchStatistics() {
    return gSearchStatistics;
}

void resetSearchStatistics() {
    gSearchStatistics = {};
}
