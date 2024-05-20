#include "Search.h"

#include "Math.h"
#include "TTable.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <iostream>
#include <print>

namespace {

std::atomic<bool> gStopSearch;
bool gWasInterrupted;

constexpr int kTTableSizeInBytes   = 64 * 1024 * 1024;
constexpr int kTTableSizeInEntries = kTTableSizeInBytes / sizeof(SearchTTable::EntryT);
SearchTTable gTTable(kTTableSizeInEntries);

StackOfVectors<MoveEvalT> gMoveScoreStack;

SearchStatistics gSearchStatistics;

constexpr int kMaxDepth = 100;
std::array<std::array<Move, 2>, kMaxDepth> gKillerMoves{};

FORCE_INLINE std::array<Move, 2>& getKillerMoves(const int ply) {
    MY_ASSERT(ply < kMaxDepth);
    return gKillerMoves[ply];
}

FORCE_INLINE void storeKillerMove(const Move& move, const int ply) {
    if (isCapture(move.flags) || isPromotion(move.flags)) {
        // Only store 'quiet' moves as killer moves.
        return;
    }

    auto& plyKillerMoves = getKillerMoves(ply);

    if (move == plyKillerMoves[0]) {
        // Don't store the same move twice.
        return;
    }

    // Shift killer moves down and store the new move at the front.
    plyKillerMoves[1] = plyKillerMoves[0];
    plyKillerMoves[0] = move;
}

// Subroutine for search and quiescence search.
// Select best move based on pre-calculated scores using a simple linear search.
// Then do a 'destructive swap' with the first move in the list and return the best move.
// This basically ends up doing a selection sort when called repeatedly, except that we don't
// actually write the best moves to the front of the list.
[[nodiscard]] FORCE_INLINE Move
selectBestMove(StackVector<Move>& moves, StackVector<MoveEvalT>& moveScores, int firstMoveIdx) {
    int bestMoveIdx         = -1;
    MoveEvalT bestMoveScore = std::numeric_limits<MoveEvalT>::lowest();

    for (int moveIdx = firstMoveIdx; moveIdx < moveScores.size(); ++moveIdx) {
        if (moveScores[moveIdx] > bestMoveScore) {
            bestMoveScore = moveScores[moveIdx];
            bestMoveIdx   = moveIdx;
        }
    }

    const Move bestMove = moves[bestMoveIdx];

    // 'Destructive swap'
    moves[bestMoveIdx]      = moves[firstMoveIdx];
    moveScores[bestMoveIdx] = moveScores[firstMoveIdx];

    return bestMove;
}

// Quiescence search. When in check search all moves, when not in check only search captures.
// Continue until no more capture are available or we get a beta cutoff.
// When not in check use a stand pat evaluation to set alpha and possibly get a beta cutoff.
[[nodiscard]] EvalT quiesce(
        GameState& gameState, EvalT alpha, EvalT beta, int ply, StackOfVectors<Move>& stack) {
    // TODO:
    //  - Can we use the TTable here?
    //  - Can we prune certain captures? Maybe using SEE or a simpler heuristic?

    EvalT bestScore = -kInfiniteEval;

    if (gStopSearch) {
        gWasInterrupted = true;
        return bestScore;
    }

    ++gSearchStatistics.qNodesSearched;

    // No need to check for repetitions and 50 move rule: those are impossible when only doing
    // captures.

    const BitBoard enemyControl = gameState.getEnemyControl();
    const bool isInCheck        = gameState.isInCheck(enemyControl);

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

    auto moveScores = scoreMoves(moves, gameState, getKillerMoves(ply), gMoveScoreStack);

    for (int moveIdx = 0; moveIdx < moves.size(); ++moveIdx) {
        const Move move = selectBestMove(moves, moveScores, moveIdx);

        const auto unmakeInfo = gameState.makeMove(move);

        EvalT score = -quiesce(gameState, -beta, -alpha, ply + 1, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (gWasInterrupted) {
            return bestScore;
        }

        if (isMate(score)) {
            score -= signum(score);
        }

        if (score >= beta) {
            storeKillerMove(move, ply);
            return score;
        }

        alpha     = std::max(alpha, score);
        bestScore = std::max(bestScore, score);
    }

    return bestScore;
}

// Subroutine for search.
// Write updated information to the ttable.
FORCE_INLINE void updateTTable(
        EvalT bestScore,
        EvalT alphaOrig,
        EvalT beta,
        bool stoppedEarly,
        Move bestMove,
        int depth,
        HashT hash) {
    ScoreType scoreType;
    if (stoppedEarly) {
        // Don't trust scores from partial search.
        scoreType = ScoreType::NotSet;
    } else if (bestScore <= alphaOrig) {
        // Best score is below original feasibility window, so it's an upper bound.
        scoreType = ScoreType::UpperBound;
    } else if (bestScore >= beta) {
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

// Forward declaration
[[nodiscard]] EvalT search(
        GameState& gameState,
        const int depth,
        const int ply,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack);

enum class SearchMoveOutcome {
    Continue,
    Cutoff,
    Interrupted,
};

// Subroutine for search.
// Search a single move, updating alpha, bestScore and bestMove as necessary.
[[nodiscard]] FORCE_INLINE SearchMoveOutcome searchMove(
        GameState& gameState,
        const Move& move,
        const int depth,
        const int ply,
        EvalT& alpha,
        const EvalT beta,
        StackOfVectors<Move>& stack,
        EvalT& bestScore,
        Move& bestMove) {

    auto unmakeInfo = gameState.makeMove(move);

    EvalT score = -search(gameState, depth - 1, ply + 1, -beta, -alpha, stack);

    gameState.unmakeMove(move, unmakeInfo);

    if (gWasInterrupted) {
        return SearchMoveOutcome::Interrupted;
    }

    if (isMate(score)) {
        score -= signum(score);
    }

    if (score > bestScore) {
        bestScore = score;
        bestMove  = move;

        if (bestScore >= beta) {
            storeKillerMove(move, ply);

            // Fail high; score is a lower bound.
            return SearchMoveOutcome::Cutoff;
        }

        // If score is above alpha, it is either exact or a lower bound, so it is safe to raise
        // the lower bound of our feasibility window.
        alpha = std::max(alpha, bestScore);
    }

    return SearchMoveOutcome::Continue;
}

// Main search function: alpha-beta search with negamax and transposition table.
//
// If returned value s satisfies alpha < s < beta, the value is exact.
// If s <= alpha, the value is an upper bound.
// If beta <= s, the value is a lower bound.
// If gStopSearch is true, returns std::nullopt
[[nodiscard]] EvalT search(
        GameState& gameState,
        const int depth,
        const int ply,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack) {
    if (gStopSearch) {
        gWasInterrupted = true;
        return -kInfiniteEval;
    }

    ++gSearchStatistics.normalNodesSearched;

    if (depth == 0) {
        return quiesce(gameState, alpha, beta, ply, stack);
    }

    // alphaOrig determines whether the value returned is an upper bound
    const EvalT alphaOrig = alpha;

    if (ply > 0) {
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

    EvalT bestScore = -kInfiniteEval;
    Move bestMove{};

    // Probe the transposition table to update our score based on previous info.
    const auto ttHit = gTTable.probe(gameState.getBoardHash());
    if (ttHit) {
        const auto& ttInfo = ttHit->payload;

        gSearchStatistics.tTableHits++;

        if (ttInfo.depth >= depth) {
            if (ttInfo.scoreType == ScoreType::Exact) {
                // Exact value
                return ttInfo.score;
            } else if (ttInfo.scoreType == ScoreType::LowerBound) {
                // Can safely raise the lower bound for our search window, because the true value
                // is guaranteed to be above this bound.
                alpha = max(alpha, ttInfo.score);
            } else if (ttInfo.scoreType == ScoreType::UpperBound) {
                // Can safely lower the upper bound for our search window, because the true value
                // is guaranteed to be below this bound.
                beta = min(beta, ttInfo.score);
            }
            // Else: score type not set (result from interrupted search).

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

        // Try hash move first.
        // TODO: do we need a legality check here for hash collisions?
        const auto outcome = searchMove(
                gameState, ttInfo.bestMove, depth, ply, alpha, beta, stack, bestScore, bestMove);

        if (outcome == SearchMoveOutcome::Interrupted) {
            return bestScore;
        }

        if (outcome == SearchMoveOutcome::Cutoff) {
            // Fail high

            updateTTable(
                    bestScore,
                    alphaOrig,
                    beta,
                    gWasInterrupted,
                    bestMove,
                    depth,
                    gameState.getBoardHash());

            // Score was obtained from a subcall that failed high, so it was a lower bound for
            // that position. It is also a lower bound for the overall position because we're
            // maximizing.
            return bestScore;
        }
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

    auto moveScores = scoreMoves(moves, gameState, getKillerMoves(ply), gMoveScoreStack);

    for (int moveIdx = 0; moveIdx < moves.size(); ++moveIdx) {
        const Move move = selectBestMove(moves, moveScores, moveIdx);

        const auto outcome =
                searchMove(gameState, move, depth, ply, alpha, beta, stack, bestScore, bestMove);

        if (outcome != SearchMoveOutcome::Continue) {
            break;
        }
    }

    if (bestScore > -kInfiniteEval) {
        // If we fully evaluated any positions, update the ttable.
        MY_ASSERT(bestMove.pieceToMove != Piece::Invalid);
        updateTTable(
                bestScore,
                alphaOrig,
                beta,
                gWasInterrupted,
                bestMove,
                depth,
                gameState.getBoardHash());
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

// Extract the principal variation from the transposition table.
[[nodiscard]] StackVector<Move> extractPv(
        GameState gameState, StackOfVectors<Move>& stack, const int depth) {
    // Note: taking copy of gameState
    // TODO: would make+unmake be faster here?

    StackVector<Move> pv = stack.makeStackVector();

    while (pv.size() < depth) {
        const auto ttHit = gTTable.probe(gameState.getBoardHash());

        if (!ttHit) {
            break;
        }

        pv.push_back(ttHit->payload.bestMove);
        (void)gameState.makeMove(ttHit->payload.bestMove);
    }

    pv.lock();
    return pv;
}

// Perform an aspiration window search.
[[nodiscard]] RootSearchResult aspirationWindowSearch(
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

    bool everFailedLow = false;

    EvalT lastCompletedEval = -kInfiniteEval;

    do {
        const auto searchEval = search(gameState, depth, 0, lowerBound, upperBound, stack);

        const bool noEval = searchEval < -kMateEval;
        if (!noEval) {
            lastCompletedEval = searchEval;

            const bool failedLow = searchEval <= lowerBound;
            everFailedLow |= failedLow;
        }

        if (gWasInterrupted) {
            if (everFailedLow) {
                // Don't trust the best move if we ever failed low and didn't complete the search.
                // TODO: we should probably ask for more search time here.

                std::print(std::cerr, "Partial search with failed low; not returning pv.\n");

                StackVector<Move> principalVariation = stack.makeStackVector();
                principalVariation.lock();
                return {.principalVariation = std::move(principalVariation),
                        .eval               = lastCompletedEval,
                        .wasInterrupted     = true};
            }

            // Return partial result.
            return {.principalVariation = extractPv(gameState, stack, depth),
                    .eval               = lastCompletedEval,
                    .wasInterrupted     = true};
        }

        // If we weren't interrupted we should have a valid eval.
        MY_ASSERT(!noEval);

        if (lowerBound < searchEval && searchEval < upperBound) {
            // Eval is within the aspiration window; return result.
            return {.principalVariation = extractPv(gameState, stack, depth),
                    .eval               = searchEval,
                    .wasInterrupted     = false};
        }

        if (searchEval <= lowerBound) {
            // Failed low
            if (isMate(searchEval)) {
                // If we found a mate, fully open up the window to the mating side.
                // This will allow us to find the earliest mate.
                lowerBound = -kInfiniteEval;
            } else {
                // Exponentially grow the tolerance.
                lowerTolerance *= kToleranceIncreaseFactor;
                // Expand the lower bound based on the increased tolerance or the search result,
                // whichever is lower.
                lowerBound = min(searchEval - 1, initialGuess - lowerTolerance);
            }
        } else {
            // Failed high
            if (isMate(searchEval)) {
                // If we found a mate, fully open up the window to the mating side.
                // This will allow us to find the earliest mate.
                upperBound = kInfiniteEval;
            } else {
                // Exponentially grow the tolerance.
                upperTolerance *= kToleranceIncreaseFactor;
                // Expand the upper bound based on the increased tolerance or the search result,
                // whichever is higher.
                upperBound = max(searchEval + 1, initialGuess + upperTolerance);
            }
        }

        std::print(std::cerr, "Re-searching with window [{}, {}]\n", lowerBound, upperBound);
    } while (true);
}

}  // namespace

// Entry point: perform search and return the principal variation and evaluation.
RootSearchResult searchForBestMove(
        GameState& gameState,
        const int depth,
        StackOfVectors<Move>& stack,
        std::optional<EvalT> evalGuess) {
    if (evalGuess) {
        return aspirationWindowSearch(gameState, depth, stack, *evalGuess);
    } else {
        const auto searchEval = search(gameState, depth, 0, -kInfiniteEval, kInfiniteEval, stack);
        return {.principalVariation = extractPv(gameState, stack, depth),
                .eval               = searchEval,
                .wasInterrupted     = gWasInterrupted};
    }
}

void prepareForSearch() {
    // Set global variables to prepare for search.
    gMoveScoreStack.reserve(1'000);
    gStopSearch     = false;
    gWasInterrupted = false;
}

void requestSearchStop() {
    // Set stop flag to interrupt search.
    gStopSearch = true;
}

SearchStatistics getSearchStatistics() {
    gSearchStatistics.ttableUtilization = gTTable.getUtilization();

    return gSearchStatistics;
}

void resetSearchStatistics() {
    gSearchStatistics = {};
}
