#include "Search.h"

#include "Math.h"
#include "TTable.h"

#include <algorithm>
#include <atomic>
#include <iostream>
#include <print>

namespace {

bool gRecordBestMove = false;
Move gBestMove;
std::atomic<bool> gStopSearch;

constexpr int kTTableSizeInBytes   = 64 * 1024 * 1024;
constexpr int kTTableSizeInEntries = kTTableSizeInBytes / sizeof(SearchTTable::EntryT);
SearchTTable gTTable(kTTableSizeInEntries);

StackOfVectors<MoveEvalT> gMoveScoreStack;

SearchStatistics gSearchStatistics;

[[nodiscard]] Move selectBestMove(
        StackVector<Move>& moves, StackVector<MoveEvalT>& moveScores, int firstMoveIdx) {
    int bestMoveIdx         = -1;
    MoveEvalT bestMoveScore = std::numeric_limits<MoveEvalT>::lowest();

    for (int moveIdx = firstMoveIdx; moveIdx < moveScores.size(); ++moveIdx) {
        if (moveScores[moveIdx] > bestMoveScore) {
            bestMoveScore = moveScores[moveIdx];
            bestMoveIdx   = moveIdx;
        }
    }

    const Move bestMove = moves[bestMoveIdx];

    // Destructive 'swap'
    moves[bestMoveIdx]      = moves[firstMoveIdx];
    moveScores[bestMoveIdx] = moveScores[firstMoveIdx];

    return bestMove;
}

std::optional<EvalT> quiesce(
        GameState& gameState, EvalT alpha, EvalT beta, StackOfVectors<Move>& stack) {
    // TODO:
    //  - Can we use the TTable here?
    //  - Can we prune certain captures? Maybe using SEE or a simpler heuristic?

    if (gStopSearch) {
        return std::nullopt;
    }

    ++gSearchStatistics.qNodesSearched;

    // No need to check for repetitions and 50 move rule: those are impossible when only doing
    // captures.

    const BitBoard enemyControl = gameState.getEnemyControl();
    const bool isInCheck        = gameState.isInCheck(enemyControl);

    EvalT bestScore = -kInfiniteEval;

    if (!isInCheck) {
        // Stand pat
        bestScore = evaluate(gameState, stack, /*checkEndState =*/false);
        if (bestScore >= beta) {
            return bestScore;
        }
        alpha = std::max(alpha, bestScore);
    }

    auto moves = gameState.generateMoves(stack, enemyControl, /*capturesOnly =*/!isInCheck);
    if (moves.size() == 0) {
        if (isInCheck) {
            // We ran full move generation, so no legal moves exist.
            return evaluateNoLegalMoves(gameState);
        }

        // No captures are available.

        // Check if we're in an end state by generating all moves.
        // Note that this ignores repetitions and 50 move rule.
        const auto allMoves = gameState.generateMoves(stack, enemyControl);
        if (allMoves.size() == 0) {
            return evaluateNoLegalMoves(gameState);
        }

        // If we're not in an end state return the stand pat evaluation.
        return bestScore;
    }

    auto moveScores = scoreMoves(moves, gameState, gMoveScoreStack);

    for (int moveIdx = 0; moveIdx < moves.size(); ++moveIdx) {
        const Move move = selectBestMove(moves, moveScores, moveIdx);

        const auto unmakeInfo = gameState.makeMove(move);

        const auto searchResult = quiesce(gameState, -beta, -alpha, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (!searchResult) {
            return std::nullopt;
        }

        EvalT score = -searchResult.value();
        if (isMate(score)) {
            score -= signum(score);
        }

        if (score >= beta) {
            return score;
        }

        alpha     = std::max(alpha, score);
        bestScore = std::max(bestScore, score);
    }

    return bestScore;
}

void updateTTable(
        EvalT bestScore,
        EvalT alphaOrig,
        EvalT beta,
        bool stoppedEarly,
        Move bestMove,
        int depth,
        HashT hash) {
    ScoreType scoreType;
    if (bestScore <= alphaOrig) {
        // Best score is below original feasibility window, so it's an upper bound.
        scoreType = ScoreType::UpperBound;
    } else if (stoppedEarly || bestScore >= beta) {
        // If we stopped early the score is a lower bound. Otherwise...
        // A score above beta was obtained from a subcall that failed high, so that result is an
        // upper bound. This is true regardless of whether the score is above the original beta or
        // a tightened beta.
        scoreType = ScoreType::LowerBound;
    } else {
        // Score is in the feasibility window, so exact.
        scoreType = ScoreType::Exact;
    }

    const SearchTTable::EntryT entry = {
            .hash    = hash,
            .payload = {
                    .depth     = (std::uint8_t)depth,
                    .scoreType = scoreType,
                    .score     = bestScore,
                    .bestMove  = bestMove,
            }};

    auto isMoreValuable = [](const SearchTTPayload& newPayload, const SearchTTPayload& oldPayload) {
        return newPayload.depth >= oldPayload.depth;
    };

    gTTable.store(entry, isMoreValuable);
}

// If returned value s satisfies alpha < s < beta, the value is exact.
// If s <= alpha, the value is an upper bound.
// If beta <= s, the value is a lower bound.
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

    ++gSearchStatistics.normalNodesSearched;

    if (depth == 0) {
        return quiesce(gameState, alpha, beta, stack);
    }

    const bool recordBestMove = gRecordBestMove;
    gRecordBestMove           = false;

    // alphaOrig determines whether the value returned is an upper bound
    const EvalT alphaOrig = alpha;

    if (!recordBestMove) {
        if (gameState.isRepetition(/* repetitionsForDraw = */ 1)) {
            // Exact value
            return 0;
        }

        if (gameState.isFiftyMoves()) {
            const auto moves = gameState.generateMoves(stack);
            if (moves.size() == 0) {
                // Exact value
                return evaluateNoLegalMoves(gameState);
            } else {
                // Exact value
                return 0;
            }
        }
    }

    Move bestMove{};
    EvalT bestScore = -kInfiniteEval;

    // Probe the transposition table to update our score based on previous info.
    const auto ttHit = gTTable.probe(gameState.getBoardHash());
    if (ttHit) {
        const auto& ttInfo = ttHit->payload;

        gSearchStatistics.tTableHits++;

        if (!recordBestMove && ttInfo.depth >= depth) {
            if (ttInfo.scoreType == ScoreType::Exact) {
                // Exact value
                return ttInfo.score;
            } else if (ttInfo.scoreType == ScoreType::LowerBound) {
                // Can safely raise the lower bound for our search window, because the true value
                // is guaranteed to be above this bound.
                alpha = max(alpha, ttInfo.score);
            } else {
                // Can safely lower the upper bound for our search window, because the true value
                // is guaranteed to be below this bound.
                beta = min(beta, ttInfo.score);
            }

            // Check if we can return based on tighter bounds from the transposition table.
            if (alpha >= beta) {
                // Based on information from the ttable, we now know that the true value is outside
                // of the feasibility window.
                // If alpha was raised by the tt entry this is a lower bound and we want to return
                // that raised alpha (fail-soft: that's the tightest lower bound we have).
                // If beta was lowered by the tt entry this is an upper bound and we want to return
                // that lowered beta (fail-soft: that's the tightest upper bound we have).
                // So either way we return the tt entry score.
                return ttInfo.score;
            }
        }

        // Initialize best move to the hash move in case we don't get to complete the search of the
        // hash move.
        if (recordBestMove) {
            gBestMove = ttInfo.bestMove;
        }

        // Try hash move first.
        // TODO: do we need a legality check here for hash collisions?
        auto unmakeInfo = gameState.makeMove(ttInfo.bestMove);

        const auto searchResult = search(gameState, depth - 1, -beta, -alpha, stack);

        gameState.unmakeMove(ttInfo.bestMove, unmakeInfo);

        if (!searchResult) {
            return std::nullopt;
        }
        EvalT score = -searchResult.value();
        if (isMate(score)) {
            score -= signum(score);
        }

        bestScore = score;
        bestMove  = ttInfo.bestMove;

        if (score >= beta) {
            // Fail high

            updateTTable(
                    score,
                    alphaOrig,
                    beta,
                    /* stoppedEarly = */ false,
                    bestMove,
                    depth,
                    gameState.getBoardHash());

            // score was obtained from a subcall that failed high, so it was a lower bound for
            // that position. It is also a lower bound for the overall position because we're
            // maximizing.
            return score;
        }

        // If score is above alpha, it is either exact or a lower bound, so it is safe to raise
        // the lower bound of our feasibility window.
        alpha = std::max(alpha, score);
    }

    auto moves = gameState.generateMoves(stack);
    if (moves.size() == 0) {
        // Exact value
        return evaluateNoLegalMoves(gameState);
    }

    if (ttHit) {
        const auto& ttInfo    = ttHit->payload;
        const auto hashMoveIt = std::find(moves.begin(), moves.end(), ttInfo.bestMove);
        if (hashMoveIt != moves.end()) {
            std::iter_swap(moves.end() - 1, hashMoveIt);
            moves.hide_back();
        }
    }

    auto moveScores = scoreMoves(moves, gameState, gMoveScoreStack);

    for (int moveIdx = 0; moveIdx < moves.size(); ++moveIdx) {
        const Move move = selectBestMove(moves, moveScores, moveIdx);

        auto unmakeInfo = gameState.makeMove(move);

        const auto searchResult = search(gameState, depth - 1, -beta, -alpha, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (!searchResult) {
            if (bestScore > -kInfiniteEval) {
                MY_ASSERT(bestMove.pieceToMove != Piece::Invalid);

                // If we fully evaluated any positions, update the ttable.
                updateTTable(
                        bestScore,
                        alphaOrig,
                        beta,
                        /* stoppedEarly = */ true,
                        bestMove,
                        depth,
                        gameState.getBoardHash());

                if (recordBestMove) {
                    gBestMove = bestMove;
                }
            }
            return std::nullopt;
        }

        EvalT score = -searchResult.value();
        if (isMate(score)) {
            score -= signum(score);
        }

        if (score > bestScore) {
            bestScore = score;
            bestMove  = move;

            if (score >= beta) {
                // Fail high; score is a lower bound for reasons described above
                break;
            }

            // If score is above alpha, it is either exact or a lower bound, so it is safe to raise
            // the lower bound of our feasibility window.
            alpha = std::max(alpha, score);
        }
    }

    updateTTable(
            bestScore,
            alphaOrig,
            beta,
            /* stoppedEarly = */ false,
            bestMove,
            depth,
            gameState.getBoardHash());

    if (recordBestMove) {
        gBestMove = bestMove;
    }

    // If bestScore <= alphaOrig, then all subcalls returned upper bounds and bestScore is the
    // maximum of these upper bounds, so an upper bound on the overall position. This is ok because
    // we're returning a value below the original alpha.
    //
    // Otherwise, alpha >= bestScore > alphaOrig.
    //
    // If alphaOrig < bestScore < alpha, then all subscalls returned upper bounds, but all of these
    // upper bounds were below the lower bound 'promised' by the transposition table. This situation
    // should never occur!! But in practice it may because of 'search instability'.
    //
    // Otherwise, bestScore = alpha.
    //
    // If alpha was never raised after a subcall, then alpha must have been raised from the ttable.
    // In this case all subcalls returned upper bounds, and at least one of those upper bounds was
    // equal to the lower bound 'promised' by the transposition table. In this case the value must
    // be exactly this lower/upper bound.
    //
    // If alpha was raised at least once, but we're not failing high, then bestScore was set from
    // an exact value, and all other subcalls return upper bounds below that. So the value is exact.
    //
    // If we're failing high relative to the ttable beta, but not relative to the original beta,
    // then the ttable beta was incorrect: we found a lower bound greater than the 'promised' upper
    // bound. This should never occur!! But in practice it may because of 'search instability'.
    //
    // If we're failing high relative to the original beta then we found a lower bound outside the
    // feasibility window so we can safely return a lower bound.
    return bestScore;
}

StackVector<Move> extractPv(GameState gameState, StackOfVectors<Move>& stack, const int depth) {
    // Note: taking copy
    // TODO: would make+unmake be faster here?

    StackVector<Move> pv = stack.makeStackVector();

    if (gBestMove.pieceToMove == Piece::Invalid) {
        pv.lock();
        return pv;
    }

    pv.push_back(gBestMove);

    (void)gameState.makeMove(gBestMove);

    auto ttHit = gTTable.probe(gameState.getBoardHash());
    while (ttHit && pv.size() < depth) {
        pv.push_back(ttHit->payload.bestMove);

        (void)gameState.makeMove(ttHit->payload.bestMove);

        ttHit = gTTable.probe(gameState.getBoardHash());
    }

    pv.lock();
    return pv;
}

[[nodiscard]] std::optional<EvalT> searchAndRecordBestMove(
        GameState& gameState,
        const int depth,
        const EvalT alpha,
        const EvalT beta,
        StackOfVectors<Move>& stack) {
    gRecordBestMove = true;
    gBestMove       = {};
    return search(gameState, depth, alpha, beta, stack);
}

[[nodiscard]] std::optional<EvalT> aspirationWindowSearch(
        GameState& gameState,
        const int depth,
        StackOfVectors<Move>& stack,
        const EvalT initialGuess) {
    static constexpr EvalT kInitialTolerance      = 25;
    static constexpr int kToleranceIncreaseFactor = 4;

    EvalT lowerTolerance = kInitialTolerance;
    EvalT upperTolerance = kInitialTolerance;

    EvalT lowerBound = initialGuess - lowerTolerance;
    EvalT upperBound = initialGuess + upperTolerance;

    do {
        const auto searchResult =
                searchAndRecordBestMove(gameState, depth, lowerBound, upperBound, stack);
        if (!searchResult) {
            return std::nullopt;
        }

        const EvalT searchEval = *searchResult;

        if (lowerBound < searchEval && searchEval < upperBound) {
            return searchEval;
        }

        if (searchEval <= lowerBound) {
            if (isMate(searchEval)) {
                lowerBound = -kInfiniteEval;
            } else {
                lowerTolerance *= kToleranceIncreaseFactor;
                lowerBound = min(searchEval - 1, initialGuess - lowerTolerance);
            }
        } else {
            if (isMate(searchEval)) {
                upperBound = kInfiniteEval;
            } else {
                upperTolerance *= kToleranceIncreaseFactor;
                upperBound = max(searchEval + 1, initialGuess + upperTolerance);
            }
        }

        std::print(std::cerr, "Re-searching with window [{}, {}]\n", lowerBound, upperBound);
    } while (true);
}

}  // namespace

SearchResult searchForBestMove(
        GameState& gameState,
        const int depth,
        StackOfVectors<Move>& stack,
        std::optional<EvalT> evalGuess) {
    std::optional<EvalT> eval;
    if (evalGuess) {
        eval = aspirationWindowSearch(gameState, depth, stack, *evalGuess);
    } else {
        eval = searchAndRecordBestMove(gameState, depth, -kInfiniteEval, kInfiniteEval, stack);
    }

    return {.principalVariation = extractPv(gameState, stack, depth), .eval = eval};
}

void prepareForSearch() {
    gMoveScoreStack.reserve(1'000);
    gStopSearch = false;
}

void requestSearchStop() {
    gStopSearch = true;
}

SearchStatistics getSearchStatistics() {
    gSearchStatistics.ttableUtilization = gTTable.getUtilization();

    return gSearchStatistics;
}

void resetSearchStatistics() {
    gSearchStatistics = {};
}
