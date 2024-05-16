#include "Search.h"

#include "TTable.h"

#include <atomic>

namespace {

bool gRecordBestMove = false;
Move gBestMove;
std::atomic<bool> gStopSearch;

constexpr int kTTableSizeInBytes   = 64 * 1024 * 1024;
constexpr int kTTableSizeInEntries = kTTableSizeInBytes / sizeof(TTEntry);
TTable gTTable(kTTableSizeInEntries);

SearchStatistics gSearchStatistics;

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

    if (!recordBestMove) {
        // For a forced draw we can return 0 immediately.
        if (gameState.isForcedDraw(/* repetitionsForDraw = */ 1)) {
            return 0;
        }

        // Probe the transposition table to update our score based on previous info.
        auto ttHit = gTTable.probe(gameState.getBoardHash());
        if (ttHit && ttHit->depth >= depth) {
            gSearchStatistics.tTableHits++;

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
    }

    auto moves = gameState.generateMoves(stack);
    if (moves.size() == 0) {
        return evaluateNoLegalMoves(gameState);
    }

    Move bestMove;
    EvalT bestScore = -kInfiniteEval;

    for (Move move : moves) {
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

    ScoreType scoreType;
    if (bestScore <= alphaOrig) {
        scoreType = ScoreType::UpperBound;
    } else if (bestScore >= beta) {
        scoreType = ScoreType::LowerBound;
    } else {
        scoreType = ScoreType::Exact;
    }

    const TTEntry entry = {
            .hash      = gameState.getBoardHash(),
            .depth     = (std::uint8_t)depth,
            .scoreType = scoreType,
            .score     = bestScore,
    };

    gTTable.store(entry);

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
