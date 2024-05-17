#include "Search.h"

#include "Math.h"
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

// If gStopSearch is true, returns std::nullopt
[[nodiscard]] std::optional<EvalT> search(
        GameState& gameState,
        const int depth,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack) {
    if (gStopSearch) {
        return std::nullopt;
    }

    ++gSearchStatistics.nodesSearched;

    if (depth == 0) {
        return evaluate(gameState, stack);
    }

    const bool recordBestMove = gRecordBestMove;
    gRecordBestMove           = false;

    const EvalT alphaOrig = alpha;

    if (!recordBestMove) {
        if (gameState.isRepetition(/* repetitionsForDraw = */ 1)) {
            return 0;
        }

        if (gameState.isFiftyMoves()) {
            const auto moves = gameState.generateMoves(stack);
            if (moves.size() == 0) {
                return evaluateNoLegalMoves(gameState);
            } else {
                return 0;
            }
        }
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

        const auto searchResult = search(gameState, depth - 1, -beta, -alpha, stack);

        gameState.unmakeMove(ttHit->bestMove, unmakeInfo);

        if (!searchResult) {
            return std::nullopt;
        }
        EvalT score = -searchResult.value();
        if (isMate(score)) {
            score -= signum(score);
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

        const auto searchResult = search(gameState, depth - 1, -beta, -alpha, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (!searchResult) {
            return std::nullopt;
        }
        auto score = -searchResult.value();
        if (isMate(score)) {
            score -= signum(score);
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

StackVector<Move> extractPv(GameState gameState, StackOfVectors<Move>& stack, const int depth) {
    // Note: taking copy
    // TODO: would make+unmake be faster here?

    StackVector<Move> pv = stack.makeStackVector();

    pv.push_back(gBestMove);

    (void)gameState.makeMove(gBestMove);

    auto ttHit = gTTable.probe(gameState.getBoardHash());
    while (ttHit && pv.size() < depth) {
        pv.push_back(ttHit->bestMove);

        (void)gameState.makeMove(ttHit->bestMove);

        ttHit = gTTable.probe(gameState.getBoardHash());
    }

    return pv;
}

}  // namespace

SearchResult searchForBestMove(GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    gRecordBestMove = true;
    gStopSearch     = false;

    const auto eval = search(gameState, depth, -kInfiniteEval, kInfiniteEval, stack);

    return {.principalVariation = extractPv(gameState, stack, depth), .eval = eval};
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
