#include "MoveOrder.h"

namespace {

// Killer and counter bonuses can potentially both apply.
// Make sure that the combined bonus is less than the capture and promotion bonuses.
constexpr int kCaptureBonus     = 3'000;
constexpr int kPromotionBonus   = 3'000;
constexpr int kKillerMoveBonus  = 1'000;
constexpr int kCounterMoveBonus = 500;

[[nodiscard]] FORCE_INLINE MoveEvalT scoreCapture(const Move& move, const GameState& gameState) {
    MoveEvalT moveScore = kCaptureBonus;

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

    moveScore +=
            getPieceSquareValue(capturedPiece, captureTarget, nextSide(gameState.getSideToMove()));

    return moveScore;
}

[[nodiscard]] FORCE_INLINE MoveEvalT
scoreQueenPromotion(const Move& move, const GameState& gameState) {
    MoveEvalT moveScore = kPromotionBonus;

    moveScore += getPieceValue(Piece::Queen);
    moveScore -= getPieceValue(Piece::Pawn);

    moveScore += getPieceSquareValue(Piece::Queen, move.to, gameState.getSideToMove());
    moveScore -= getPieceSquareValue(Piece::Pawn, move.to, gameState.getSideToMove());

    return moveScore;
}

}  // namespace

StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        const std::array<Move, 2>& killerMoves,
        const Move& counterMove,
        StackOfVectors<MoveEvalT>& stack) {
    StackVector<MoveEvalT> scores = stack.makeStackVector();

    for (int i = 0; i < firstMoveIdx; ++i) {
        scores.push_back(0);
    }

    for (int moveIdx = firstMoveIdx; moveIdx < moves.size(); ++moveIdx) {
        const Move& move = moves[moveIdx];

        MoveEvalT moveScore = 0;

        moveScore -= getPieceSquareValue(move.pieceToMove, move.from, gameState.getSideToMove());
        moveScore += getPieceSquareValue(move.pieceToMove, move.to, gameState.getSideToMove());

        if (isCapture(move.flags)) {
            moveScore += scoreCapture(move, gameState);
        }

        // If promoting to a queen is not a good move, promoting to a knight, bishop, or rook is
        // probably even worse. So only give an ordering bonus for promoting to a queen.
        if (getPromotionPiece(move.flags) == Piece::Queen) {
            moveScore += scoreQueenPromotion(move, gameState);
        }

        if (!isCapture(move.flags) && !isPromotion(move.flags)) {
            for (const Move& killerMove : killerMoves) {
                if (move == killerMove) {
                    moveScore += kKillerMoveBonus;
                    break;
                }
            }

            if (move == counterMove) {
                moveScore += kCounterMoveBonus;
            }
        }

        scores.push_back(moveScore);
    }

    scores.lock();
    return scores;
}

StackVector<MoveEvalT> scoreMovesQuiesce(
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        StackOfVectors<MoveEvalT>& stack) {
    StackVector<MoveEvalT> scores = stack.makeStackVector();

    for (int i = 0; i < firstMoveIdx; ++i) {
        scores.push_back(0);
    }

    for (int moveIdx = firstMoveIdx; moveIdx < moves.size(); ++moveIdx) {
        const Move& move = moves[moveIdx];

        MoveEvalT moveScore = 0;

        moveScore -= getPieceSquareValue(move.pieceToMove, move.from, gameState.getSideToMove());
        moveScore += getPieceSquareValue(move.pieceToMove, move.to, gameState.getSideToMove());

        if (isCapture(move.flags)) {
            moveScore += scoreCapture(move, gameState);
        }

        // If promoting to a queen is not a good move, promoting to a knight, bishop, or rook is
        // probably even worse. So only give an ordering bonus for promoting to a queen.
        if (getPromotionPiece(move.flags) == Piece::Queen) {
            moveScore += scoreQueenPromotion(move, gameState);
        }

        scores.push_back(moveScore);
    }

    scores.lock();
    return scores;
}
