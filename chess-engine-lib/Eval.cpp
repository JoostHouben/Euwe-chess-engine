#include "Eval.h"

#include "Macros.h"
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

// clang-format off
constexpr std::array<std::array<EvalT, kSquares>, kNumPieceTypes> kPieceSquareTables{
    // Pawns
    std::array<EvalT, kSquares> {
         0,  0,  0,  0,  0,  0,  0,  0,
         5, 10, 10,-20,-20, 10, 10,  5,
         5, -5,-10,  0,  0,-10, -5,  5,
         0,  0,  0, 20, 20,  0,  0,  0,
         5,  5, 10, 25, 25, 10,  5,  5,
        10, 10, 20, 30, 30, 20, 10, 10,
        50, 50, 50, 50, 50, 50, 50, 50,
         0,  0,  0,  0,  0,  0,  0,  0,
    },

    // Knights
    std::array<EvalT, kSquares> {
        -50,-40,-30,-30,-30,-30,-40,-50,
        -40,-20,  0,  5,  5,  0,-20,-40,
        -30,  5, 10, 15, 15, 10,  5,-30,
        -30,  0, 15, 20, 20, 15,  0,-30,
        -30,  5, 15, 20, 20, 15,  5,-30,
        -30,  0, 10, 15, 15, 10,  0,-30,
        -40,-20,  0,  0,  0,  0,-20,-40,
        -50,-40,-30,-30,-30,-30,-40,-50,
    },

    // Bishops 
    std::array<EvalT, kSquares> {
        -20,-10,-10,-10,-10,-10,-10,-20,
        -10,  5,  0,  0,  0,  0,  5,-10,
        -10, 10, 10, 10, 10, 10, 10,-10,
        -10,  0, 10, 10, 10, 10,  0,-10,
        -10,  5,  5, 10, 10,  5,  5,-10,
        -10,  0,  5, 10, 10,  5,  0,-10,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -20,-10,-10,-10,-10,-10,-10,-20,
    },

    // Rooks
    std::array<EvalT, kSquares> {
          0,  0,  0,  5,  5,  0,  0,  0,
         -5,  0,  0,  0,  0,  0,  0, -5,
         -5,  0,  0,  0,  0,  0,  0, -5,
         -5,  0,  0,  0,  0,  0,  0, -5,
         -5,  0,  0,  0,  0,  0,  0, -5,
         -5,  0,  0,  0,  0,  0,  0, -5,
          5, 10, 10, 10, 10, 10, 10,  5,
          0,  0,  0,  0,  0,  0,  0,  0,
    },

    // Queens
    std::array<EvalT, kSquares> {
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -10,  5,  5,  5,  5,  5,  0,-10,
          0,  0,  5,  5,  5,  5,  0, -5,
         -5,  0,  5,  5,  5,  5,  0, -5,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20,
    },

    // King
    // TODO: different piece-square table for end game
    std::array<EvalT, kSquares> {
         20, 30, 10,  0,  0, 10, 30, 20,
         20, 20,  0,  0,  0,  0, 20, 20,
        -10,-20,-20,-20,-20,-20,-20,-10,
        -20,-30,-30,-40,-40,-30,-30,-20,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
    }
};
// clang-format on

[[nodiscard]] FORCE_INLINE int evaluatePiecePositionsForSide(
        const GameState& gameState, const Side side) {
    int piecePosition = 0;
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        const Piece piece      = (Piece)pieceIdx;
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            BoardPosition position = popFirstSetPosition(pieceBitBoard);

            if (side == Side::Black) {
                // Flip the position for black. Note that this flips both the rank and file.
                // (I.e., it's a 180 degree rotation, not a reflection.)
                position = (BoardPosition)(kSquares - (int)position);
            }

            piecePosition += kPieceSquareTables[pieceIdx][(int)position];
            piecePosition += kPieceValues[pieceIdx];
        }
    }

    return piecePosition;
}

[[nodiscard]] FORCE_INLINE EvalT evaluateForWhite(const GameState& gameState) {
    const int pieceSquare = evaluatePiecePositionsForSide(gameState, Side::White)
                          - evaluatePiecePositionsForSide(gameState, Side::Black);

    EvalT eval = (EvalT)clamp(pieceSquare, -kMateEval + 1'000, kMateEval - 1'000);

    return eval;
}

[[nodiscard]] FORCE_INLINE std::optional<EvalT> evaluateEndState(
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

FORCE_INLINE EvalT evaluateNoLegalMoves(const GameState& gameState) {
    if (gameState.isInCheck()) {
        // We're in check and there are no legal moves so we're in checkmate.
        return -kMateEval;
    }

    // We're not in check and there are no legal moves so this is a stalemate.
    return 0;
}

EvalT evaluate(const GameState& gameState, StackOfVectors<Move>& stack, bool checkEndState) {
    if (checkEndState) {
        auto maybeEndEval = evaluateEndState(gameState, stack);
        if (maybeEndEval) {
            return *maybeEndEval;
        }
    }

    const EvalT whiteEval = evaluateForWhite(gameState);
    return gameState.getSideToMove() == Side::White ? whiteEval : -whiteEval;
}

void selectBestMove(StackVector<Move>& moves, int firstMoveIdx, const GameState& gameState) {
    int bestMoveScore = -kInfiniteEval;
    int bestMoveIdx   = -1;

    static constexpr int kCaptureBonus   = 2'000;
    static constexpr int kPromotionBonus = 2'000;

    for (int moveIdx = firstMoveIdx; moveIdx < moves.size(); ++moveIdx) {
        const Move& move = moves[moveIdx];

        int moveScore = 0;

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

            // Most valuable victim, least valuable aggressor (MVV-LVA)
            // We achieve this by adding the value of the victim, and then subtracting the value of
            // the aggressor, but divided by a large enough factor so that the victim's value
            // dominates. Right shifting by 5 is sufficient, since king value / 32 is less than one
            // pawn.
            moveScore += kPieceValues[(int)capturedPiece];
            moveScore -= (kPieceValues[(int)move.pieceToMove] >> 5);

            moveScore += kPieceSquareTables[(int)capturedPiece][(int)captureTarget];
        }

        moveScore -= kPieceSquareTables[(int)move.pieceToMove][(int)move.from];

        if (auto promotionPiece = getPromotionPiece(move.flags); promotionPiece != Piece::Pawn) {
            moveScore += kPromotionBonus;

            moveScore += kPieceValues[(int)promotionPiece];
            moveScore -= kPieceValues[(int)Piece::Pawn];

            moveScore += kPieceSquareTables[(int)promotionPiece][(int)move.to];
        } else {
            moveScore += kPieceSquareTables[(int)move.pieceToMove][(int)move.to];
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
