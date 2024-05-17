#include "Eval.h"

#include "Math.h"

#include <array>
#include <optional>

namespace {

constexpr std::array<EvalT, kNumPieceTypes> kPieceValues = {
        100,     // Pawn
        305,     // Knight
        333,     // Bishop
        563,     // Rook
        950,     // Queen
        20'000,  // King (for move ordering)
};

// TODO: handle overflows of EvalT
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
    if (gameState.isRepetition()) {
        return 0;
    }

    const auto moves = gameState.generateMoves(stack);

    if (moves.size() > 0) {
        if (gameState.isFiftyMoves()) {
            // If there are legal moves the 50 move rule applies.
            return 0;
        }

        // If there are any legal moves and the 50 move rule doesn't apply we're not in an end state.
        return std::nullopt;
    }

    return evaluateNoLegalMoves(gameState);
}

}  // namespace

EvalT evaluateNoLegalMoves(const GameState& gameState) {
    if (gameState.isInCheck()) {
        // We're in check and there are no legal moves so we're in checkmate.
        return -kMateEval;
    }

    // We're not in check and there are no legal moves so this is a stalemate.
    return 0;
}

EvalT evaluate(const GameState& gameState, StackOfVectors<Move>& stack) {
    if (auto maybeEndEval = evaluateEndState(gameState, stack); maybeEndEval) {
        return *maybeEndEval;
    }

    const EvalT whiteEval = evaluateForWhite(gameState);
    return gameState.getSideToMove() == Side::White ? whiteEval : -whiteEval;
}

void selectBestMove(StackVector<Move>& moves, int firstMoveIdx, const GameState& gameState) {
    EvalT bestMoveScore = -kInfiniteEval;
    int bestMoveIdx     = -1;

    static constexpr EvalT kCaptureBonus   = 2'000;
    static constexpr EvalT kPromotionBonus = 2'000;

    for (int moveIdx = firstMoveIdx; moveIdx < moves.size(); ++moveIdx) {
        const Move& move = moves[moveIdx];

        EvalT moveScore = 0;

        if (isCapture(move.flags)) {
            moveScore += kCaptureBonus;

            Piece capturedPiece;
            if (isEnPassant(move.flags)) {
                capturedPiece = Piece::Pawn;
            } else {
                capturedPiece = getPiece(gameState.getPieceOnSquare(move.to));
            }

            moveScore += kPieceValues[(int)capturedPiece];
            moveScore -= kPieceValues[(int)move.pieceToMove];
        }

        if (auto promotionPiece = getPromotionPiece(move.flags); promotionPiece != Piece::Pawn) {
            moveScore += kPromotionBonus;

            moveScore += kPieceValues[(int)promotionPiece];
            moveScore -= kPieceValues[(int)Piece::Pawn];
        }

        if (moveScore > bestMoveScore) {
            bestMoveScore = moveScore;
            bestMoveIdx   = moveIdx;
        }
    }

    std::swap(moves[firstMoveIdx], moves[bestMoveIdx]);
}

bool isMate(EvalT eval) {
    return std::abs(eval) >= kMateEval - 1000;
}

int getMateDistance(EvalT eval) {
    MY_ASSERT(isMate(eval));

    const int mateInPly   = kMateEval - std::abs(eval);
    const int mateInMoves = (mateInPly + 1) / 2;

    return signum(eval) * mateInMoves;
}
