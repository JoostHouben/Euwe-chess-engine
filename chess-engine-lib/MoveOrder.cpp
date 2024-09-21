#include "MoveOrder.h"

#include "Eval.h"

namespace {

// Killer and counter bonuses can potentially both apply.
// Make sure that the combined bonus is less than the capture and promotion bonuses.
constexpr int kCaptureBonus     = 3'000;
constexpr int kPromotionBonus   = 3'000;
constexpr int kKillerMoveBonus  = 1'000;
constexpr int kCounterMoveBonus = 500;

[[nodiscard]] FORCE_INLINE MoveEvalT
scoreCapture(const Evaluator& evaluator, const Move& move, const GameState& gameState) {
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
    moveScore += getStaticPieceValue(capturedPiece);
    moveScore -= (getStaticPieceValue(move.pieceToMove) >> 5);

    moveScore += evaluator.getPieceSquareValue(
            capturedPiece, captureTarget, nextSide(gameState.getSideToMove()));

    return moveScore;
}

[[nodiscard]] FORCE_INLINE MoveEvalT
scoreQueenPromotion(const Move& move, const GameState& gameState) {
    MoveEvalT moveScore = kPromotionBonus;

    moveScore += getStaticPieceValue(Piece::Queen);
    moveScore -= getStaticPieceValue(Piece::Pawn);

    return moveScore;
}

}  // namespace

StackVector<MoveEvalT> scoreMoves(
        const Evaluator& evaluator,
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        const std::array<Move, 2>& killerMoves,
        const Move& counterMove,
        const std::array<std ::array<unsigned, kSquares>, kNumPieceTypes>& historyCutOffs,
        const std::array<std ::array<unsigned, kSquares>, kNumPieceTypes>& historyUsed,
        StackOfVectors<MoveEvalT>& stack) {
    StackVector<MoveEvalT> scores = stack.makeStackVector();

    for (int i = 0; i < firstMoveIdx; ++i) {
        scores.push_back(0);
    }

    for (int moveIdx = firstMoveIdx; moveIdx < moves.size(); ++moveIdx) {
        const Move& move = moves[moveIdx];

        MoveEvalT moveScore = 0;

        const int cutOffScore = historyCutOffs[(int)move.pieceToMove][(int)move.to];
        const int usedScore   = historyUsed[(int)move.pieceToMove][(int)move.to];

        moveScore += (cutOffScore << 5) / usedScore;

        if (isCapture(move.flags)) {
            moveScore += scoreCapture(evaluator, move, gameState);
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
        const Evaluator& evaluator,
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

        moveScore -= evaluator.getPieceSquareValue(
                move.pieceToMove, move.from, gameState.getSideToMove());
        moveScore +=
                evaluator.getPieceSquareValue(move.pieceToMove, move.to, gameState.getSideToMove());

        if (isCapture(move.flags)) {
            moveScore += scoreCapture(evaluator, move, gameState);
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

// Select best move based on pre-calculated scores using a simple linear search.
// Then do a 'destructive swap' with the first move in the list and return the best move.
// This basically ends up doing a selection sort when called repeatedly, except that we don't
// actually write the best moves to the front of the list.
FORCE_INLINE Move
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
