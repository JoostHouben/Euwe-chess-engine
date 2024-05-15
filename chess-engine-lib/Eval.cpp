#include "Eval.h"

#include <array>
#include <optional>

namespace {

constexpr std::array<EvalT, kNumPieceTypes - 1> kPieceValues = {
        100,  // Pawn
        305,  // Knight
        333,  // Bishop
        563,  // Rook
        950,  // Queen
};

constexpr EvalT kMateEval = (EvalT)30'000;

[[nodiscard]] EvalT evaluateMaterialForSide(const GameState& gameState, const Side side) {
    EvalT material = 0;
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const Piece piece            = (Piece)pieceIdx;
        const BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);

        material += kPieceValues[pieceIdx] * popCount(pieceBitBoard);
    }

    return material;
}

[[nodiscard]] EvalT evaluateMaterial(const GameState& gameState) {
    return evaluateMaterialForSide(gameState, Side::White)
         - evaluateMaterialForSide(gameState, Side::Black);
}

[[nodiscard]] EvalT evaluateForWhite(const GameState& gameState) {
    return evaluateMaterial(gameState);
}

[[nodiscard]] std::optional<EvalT> evaluateEndState(
        const GameState& gameState, StackOfVectors<Move>& stack) {
    // TODO: three-fold repetition.

    if (gameState.getPlySinceCaptureOrPawn() >= 100) {
        // 50 move rule
        return 0;
    }

    auto moves = gameState.generateMoves(stack);

    if (moves.size() > 0) {
        // If there are any legal moves we're not in an end state.
        return std::nullopt;
    }

    if (gameState.isInCheck()) {
        // We're in check and there are no legal moves so we're in checkmate.
        return -kMateEval;
    }

    // We're not in check and there are no legal moves so this is a stalemate.
    return 0;
}

}  // namespace

EvalT evaluate(const GameState& gameState, StackOfVectors<Move>& stack) {
    if (auto maybeEndEval = evaluateEndState(gameState, stack); maybeEndEval) {
        return *maybeEndEval;
    }

    const EvalT whiteEval = evaluateForWhite(gameState);
    return gameState.getSideToMove() == Side::White ? whiteEval : -whiteEval;
}
