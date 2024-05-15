#include "Eval.h"

#include <array>

namespace {

constexpr std::array<EvalT, kNumPieceTypes - 1> kPieceValues = {
        100,  // Pawn
        305,  // Knight
        333,  // Bishop
        563,  // Rook
        950,  // Queen
};

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

}  // namespace

EvalT evaluate(const GameState& gameState) {
    const EvalT whiteEval = evaluateForWhite(gameState);
    return gameState.getSideToMove() == Side::White ? whiteEval : -whiteEval;
}
