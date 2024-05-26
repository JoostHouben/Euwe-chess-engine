#include "MoveOrder.h"

namespace {

[[nodiscard]] FORCE_INLINE StackVector<int> scoreMoves(
        const StackVector<Move>& moves,
        const GameState& gameState,
        std::optional<std::array<Move, 2>> killerMoves,
        std::optional<Move> counterMove,
        StackOfVectors<int>& stack) {
    StackVector<int> scores = stack.makeStackVector();

    // Killer and counter bonuses can potentially both apply.
    // Make sure that the combined bonus is less than the capture and promotion bonuses.
    static constexpr int kCaptureBonus     = 3'000;
    static constexpr int kPromotionBonus   = 3'000;
    static constexpr int kKillerMoveBonus  = 1'000;
    static constexpr int kCounterMoveBonus = 500;

    for (const Move& move : moves) {
        int moveScore = 0;

        moveScore -= getPieceSquareValue(move.pieceToMove, move.from, gameState.getSideToMove());

        if (isCapture(move.flags)) {
            moveScore += kCaptureBonus;

            Piece capturedPiece;
            BoardPosition captureTarget = move.to;
            if (isEnPassant(move.flags)) {
                capturedPiece = Piece::Pawn;
                captureTarget = gameState.getEnPassantTarget();
            } else {
                capturedPiece = getPiece(gameState.getPieceOnSquare(move.to));
            }

            // Most valuable victim, least valuable aggressor (MVV-LVA), but disfavoring captures
            // with the king.
            // We achieve this by adding the value of the victim, and then subtracting the value of
            // the aggressor, but divided by a large enough factor so that the victim's value
            // dominates for all but the king.
            moveScore += getPieceValue(capturedPiece);
            moveScore -= (getPieceValue(move.pieceToMove) >> 5);

            moveScore += getPieceSquareValue(
                    capturedPiece, captureTarget, nextSide(gameState.getSideToMove()));
        }

        // If promoting to a queen is not a good move, promoting to a knight, bishop, or rook is
        // probably even worse. So only give an ordering bonus for promoting to a queen.
        if (auto promotionPiece = getPromotionPiece(move.flags); promotionPiece == Piece::Queen) {
            moveScore += kPromotionBonus;

            moveScore += getPieceValue(promotionPiece);
            moveScore -= getPieceValue(Piece::Pawn);

            moveScore += getPieceSquareValue(promotionPiece, move.to, gameState.getSideToMove());
        } else {
            moveScore += getPieceSquareValue(move.pieceToMove, move.to, gameState.getSideToMove());
        }

        if (!isCapture(move.flags) && !isPromotion(move.flags)) {
            if (killerMoves) {
                for (const Move& killerMove : *killerMoves) {
                    if (move == killerMove) {
                        moveScore += kKillerMoveBonus;
                        break;
                    }
                }
            }

            if (counterMove && move == *counterMove) {
                moveScore += kCounterMoveBonus;
            }
        }

        scores.push_back(moveScore);
    }

    scores.lock();
    return scores;
}

}  // namespace

StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const GameState& gameState,
        const std::array<Move, 2>& killerMoves,
        const Move& counterMove,
        StackOfVectors<MoveEvalT>& stack) {
    return scoreMoves(
            moves, gameState, std::optional(killerMoves), std::optional(counterMove), stack);
}

StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const GameState& gameState,
        StackOfVectors<MoveEvalT>& stack) {
    return scoreMoves(moves, gameState, std::nullopt, std::nullopt, stack);
}
