#include "PreProcessing.h"

#include "chess-engine-lib/Eval.h"
#include "chess-engine-lib/Math.h"
#include "chess-engine-lib/MoveOrderer.h"
#include "chess-engine-lib/MoveScorer.h"
#include "chess-engine-lib/RangePatches.h"

#include <algorithm>
#include <execution>
#include <print>
#include <ranges>

namespace {

bool isDraw(const GameState& gameState, StackOfVectors<Move>& stack) {
    if (gameState.isRepetition(/*repetitionThreshold =*/2)) {
        return true;
    }

    if (gameState.isFiftyMoves()) {
        const auto moves = gameState.generateMoves(stack);
        if (moves.size() == 0) {
            return evaluateNoLegalMoves(gameState);
        } else {
            return true;
        }
    }

    if (isInsufficientMaterial(gameState)) {
        return true;
    }

    return false;
}

FORCE_INLINE EvalT updateMateDistanceOut(const EvalT score) {
    if (abs(score) == kInfiniteEval) {
        return score;
    }
    if (isMate(score)) {
        return mateDistancePlus1(score);
    }
    return score;
}

FORCE_INLINE EvalT updateMateDistanceIn(const EvalT score) {
    if (isMate(score)) {
        return mateDistanceMinus1(score);
    }
    return score;
}

std::pair<EvalT, GameState> quiesce(
        GameState& gameState,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack,
        MoveScorer& moveScorer,
        const Evaluator& evaluator) {
    if (isDraw(gameState, stack)) {
        return {0, gameState};
    }

    const BoardControl boardControl = gameState.getBoardControl();
    const bool isInCheck            = gameState.isInCheck(boardControl);

    EvalT standPat = -kInfiniteEval;
    if (!isInCheck) {
        // Stand pat
        standPat = evaluator.evaluate(gameState);
        if (standPat >= beta) {
            return {standPat, gameState};
        }

        alpha = max(alpha, standPat);
    }

    EvalT bestScore     = standPat;
    GameState bestState = gameState;

    auto moveOrderer = moveScorer.getMoveOrdererQuiescence(stack.makeStackVector(), std::nullopt);

    while (const auto maybeMove =
                   moveOrderer.getNextBestMoveQuiescence(gameState, boardControl, isInCheck)) {
        const Move move = *maybeMove;

        const auto unmakeInfo = gameState.makeMove(move);

        auto [score, state] =
                quiesce(gameState,
                        updateMateDistanceIn(-beta),
                        updateMateDistanceIn(-alpha),
                        stack,
                        moveScorer,
                        evaluator);
        score = -score;

        gameState.unmakeMove(move, unmakeInfo);

        alpha = max(alpha, score);
        if (score > bestScore) {
            bestScore = score;
            bestState = state;
        }

        if (alpha >= beta) {
            break;
        }
    }

    if (!moveOrderer.anyLegalMovesQuiescence(gameState, boardControl, isInCheck)) {
        if (isInCheck) {
            return {updateMateDistanceOut(-kMateEval), gameState};
        } else {
            // No legal moves, not in check, so stalemate.
            return {0, gameState};
        }
    }

    return {updateMateDistanceOut(bestScore), bestState};
}

}  // namespace

std::vector<ScoredPosition> quiescePositions(
        const std::vector<AnnotatedPosition>& annotatedPositions) {
    const Evaluator evaluator(EvalParams::getDefaultParams());

    std::vector<std::optional<ScoredPosition>> maybeQuiescedPositions(annotatedPositions.size());

    std::transform(
            std::execution::par_unseq,
            annotatedPositions.begin(),
            annotatedPositions.end(),
            maybeQuiescedPositions.begin(),
            [&evaluator](
                    const AnnotatedPosition& annotatedPosition) -> std::optional<ScoredPosition> {
                const EvalT evalThreshold = 500;

                const EvalT baseEval = evaluator.evaluate(annotatedPosition.gameState);
                if (std::abs(baseEval) >= evalThreshold) {
                    return std::nullopt;
                }

                const EvalT deltaThreshold = 50;
                const EvalT alpha          = baseEval - deltaThreshold - 1;
                const EvalT beta           = baseEval + deltaThreshold + 1;

                StackOfVectors<Move> moveStack;
                MoveScorer moveScorer(evaluator);
                GameState gameState(annotatedPosition.gameState);
                auto [score, state] =
                        quiesce(gameState, alpha, beta, moveStack, moveScorer, evaluator);

                const EvalT evalDelta = (EvalT)std::abs(baseEval - score);
                if (evalDelta >= deltaThreshold || std::abs(score) >= evalThreshold) {
                    return std::nullopt;
                }

                double scoreToUse = annotatedPosition.finalScore;
                if (state.getSideToMove() != annotatedPosition.gameState.getSideToMove()) {
                    scoreToUse = 1 - scoreToUse;
                }

                // Run move generation so that the pin bit board is pre-calculated, speeding up evaluation.
                (void)state.generateMoves(moveStack);

                return ScoredPosition{state, scoreToUse};
            });

    std::vector<ScoredPosition> quiescedPositions =
            maybeQuiescedPositions | std::views::filter([](const auto& maybePosition) {
                return maybePosition.has_value();
            })
            | std::views::transform([](const auto& maybePosition) { return maybePosition.value(); })
            | range_to<std::vector<ScoredPosition>>();

    std::println("Obtained {} quiesced positions", quiescedPositions.size());

    return quiescedPositions;
}
