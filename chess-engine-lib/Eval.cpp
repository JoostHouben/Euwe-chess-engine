#include "Eval.h"

#include "Macros.h"
#include "Math.h"
#include "PawnMasks.h"

#include <array>
#include <optional>

namespace {

constexpr std::array<int, kNumPieceTypes> kPieceValues = {
        100,     // Pawn
        305,     // Knight
        333,     // Bishop
        563,     // Rook
        950,     // Queen
        20'000,  // King (for move ordering)
};

constexpr std::array<int, kNumPieceTypes> kPhaseMaterialValues = {
        0,  // Pawn
        1,  // Knight
        1,  // Bishop
        2,  // Rook
        4,  // Queen
        0,  // King
};

constexpr int kMaxPhaseMaterial = 24;

// clang-format off
constexpr std::array<std::array<int, kSquares>, kNumPieceTypes> kPieceSquareTablesEarly = {
    // Pawns - stand in front of king and promote
    std::array<int, kSquares> {
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
    std::array<int, kSquares> {
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
    std::array<int, kSquares> {
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
    std::array<int, kSquares> {
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
    std::array<int, kSquares> {
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -10,  5,  5,  5,  5,  5,  0,-10,
          0,  0,  5,  5,  5,  5,  0, -5,
         -5,  0,  5,  5,  5,  5,  0, -5,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20,
    },

    // King - encourage hiding behind pawns
    std::array<int, kSquares> {
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

constexpr std::array<std::array<int, kSquares>, kNumPieceTypes> kPieceSquareTablesLate = {
    // Pawns - promote
    std::array<int, kSquares> {
         0,  0,  0,  0,  0,  0,  0,  0,
        10, 10, 10, 10, 10, 10, 10, 10,
        10, 10, 10, 10, 10, 10, 10, 10,
        20, 20, 20, 20, 20, 20, 20, 20,
        30, 30, 30, 30, 30, 30, 30, 30,
        50, 50, 50, 50, 50, 50, 50, 50,
        80, 80, 80, 80, 80, 80, 80, 80,
         0,  0,  0,  0,  0,  0,  0,  0,
    },

    // Knights
    std::array<int, kSquares> {
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
    std::array<int, kSquares> {
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
    std::array<int, kSquares> {
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
    std::array<int, kSquares> {
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -10,  5,  5,  5,  5,  5,  0,-10,
          0,  0,  5,  5,  5,  5,  0, -5,
         -5,  0,  5,  5,  5,  5,  0, -5,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -20,-10,-10, -5, -5,-10,-10,-20,
    },

    // King - encourage center
    std::array<int, kSquares> {
        -50,-30,-30,-30,-30,-30,-30,-50,
        -30,-30,  0,  0,  0,  0,-30,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-20,-10,  0,  0,-10,-20,-30,
        -50,-40,-30,-20,-20,-30,-40,-50,
    }
};
// clang-format on

constexpr std::array kPassedPawnBonus = {0, 90, 60, 40, 25, 15, 15};

constexpr int kDoubledPawnPenalty = 20;

struct PiecePositionEvaluation {
    int material          = 0;
    int phaseMaterial     = 0;
    int earlyGamePosition = 0;
    int endGamePosition   = 0;
};

[[nodiscard]] FORCE_INLINE PiecePositionEvaluation
evaluatePiecePositionsForSide(const GameState& gameState, const Side side) {
    PiecePositionEvaluation result{};

    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        const Piece piece      = (Piece)pieceIdx;
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            BoardPosition position = popFirstSetPosition(pieceBitBoard);

            if (side == Side::Black) {
                position = getVerticalReflection(position);
            }

            result.material += kPieceValues[pieceIdx];
            result.phaseMaterial += kPhaseMaterialValues[pieceIdx];

            result.earlyGamePosition += kPieceSquareTablesEarly[pieceIdx][(int)position];
            result.endGamePosition += kPieceSquareTablesLate[pieceIdx][(int)position];
        }
    }

    return result;
}

[[nodiscard]] FORCE_INLINE PiecePositionEvaluation
evaluatePawnsForSide(const GameState& gameState, const Side side) {
    // TODO: should we hash pawn structure and store pawn eval?

    PiecePositionEvaluation result{};

    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);

    BitBoard pawnBitBoard = ownPawns;

    while (pawnBitBoard != BitBoard::Empty) {
        const BoardPosition position = popFirstSetPosition(pawnBitBoard);

        {
            BoardPosition positionForPieceSquare = position;
            if (side == Side::Black) {
                positionForPieceSquare = getVerticalReflection(positionForPieceSquare);
            }

            result.material += kPieceValues[0];
            result.phaseMaterial += kPhaseMaterialValues[0];

            result.earlyGamePosition += kPieceSquareTablesEarly[0][(int)positionForPieceSquare];
            result.endGamePosition += kPieceSquareTablesLate[0][(int)positionForPieceSquare];
        }

        const BitBoard passedPawnOpponentMask = getPassedPawnOpponentMask(position, side);
        const BitBoard forwardMask            = getPawnForwardMask(position, side);

        const BitBoard opponentBlockers = intersection(enemyPawns, passedPawnOpponentMask);
        const BitBoard ownBlockers      = intersection(ownPawns, forwardMask);

        const bool isDoubledPawn = ownBlockers != BitBoard::Empty;
        const bool isPassedPawn  = !isDoubledPawn && opponentBlockers == BitBoard::Empty;
        if (isDoubledPawn) {
            result.earlyGamePosition -= kDoubledPawnPenalty;
            result.endGamePosition -= kDoubledPawnPenalty;
        } else if (isPassedPawn) {
            const int rank                = rankFromPosition(position);
            const int distanceToPromotion = side == Side::White ? kRanks - 1 - rank : rank;

            result.earlyGamePosition += kPassedPawnBonus[distanceToPromotion];
            result.endGamePosition += kPassedPawnBonus[distanceToPromotion];
        }
    }

    return result;
}

[[nodiscard]] FORCE_INLINE int manhattanDistance(BoardPosition a, BoardPosition b) {
    const auto [aFile, aRank] = fileRankFromPosition(a);
    const auto [bFile, bRank] = fileRankFromPosition(b);

    return std::abs(aFile - bFile) + std::abs(aRank - bRank);
}

[[nodiscard]] FORCE_INLINE int manhattanDistanceToCenter(BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    return std::min({
            std::abs(file - kFiles / 2),
            std::abs(rank - kRanks / 2),
    });
}

[[nodiscard]] FORCE_INLINE int evaluateKingSwarming(
        const GameState& gameState,
        Side swarmingSide,
        int swarmingMaterial,
        int defendingMaterial,
        float endGameFactor) {
    if (defendingMaterial >= swarmingMaterial) {
        return 0;
    }

    static constexpr int rookValue = kPieceValues[(int)Piece::Rook];

    const float materialAdvantageFactor =
            std::min((float)(swarmingMaterial - defendingMaterial) / (float)rookValue, 1.f);

    const BoardPosition swarmingKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(swarmingSide, Piece::King));
    const BoardPosition defendingKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(nextSide(swarmingSide), Piece::King));

    const int defendingKingDistanceToCenter = manhattanDistanceToCenter(defendingKingPosition);

    const int kingDistance = manhattanDistance(swarmingKingPosition, defendingKingPosition);

    const int swarmingValue = defendingKingDistanceToCenter + (14 - kingDistance);

    return (int)(endGameFactor * materialAdvantageFactor * swarmingValue * 10.f);
}

[[nodiscard]] FORCE_INLINE EvalT evaluateForWhite(const GameState& gameState) {
    const auto whitePiecePositionEval = evaluatePiecePositionsForSide(gameState, Side::White);
    const auto blackPiecePositionEval = evaluatePiecePositionsForSide(gameState, Side::Black);

    const auto whitePawnEval = evaluatePawnsForSide(gameState, Side::White);
    const auto blackPawnEval = evaluatePawnsForSide(gameState, Side::Black);

    const int whiteMaterial = whitePiecePositionEval.material + whitePawnEval.material;
    const int blackMaterial = blackPiecePositionEval.material + blackPawnEval.material;

    const int materialEval = whiteMaterial - blackMaterial;

    const int phaseMaterial = std::min(
            whitePiecePositionEval.phaseMaterial + blackPiecePositionEval.phaseMaterial,
            kMaxPhaseMaterial);
    const float endGameFactor = 1.f - (float)phaseMaterial / (float)kMaxPhaseMaterial;

    const int earlyGameWhitePositionEval =
            whitePiecePositionEval.earlyGamePosition + whitePawnEval.earlyGamePosition;
    const int earlyGameBlackPositionEval =
            blackPiecePositionEval.earlyGamePosition + blackPawnEval.earlyGamePosition;

    const int endGameWhitePositionEval =
            whitePiecePositionEval.endGamePosition + whitePawnEval.endGamePosition;
    const int endGameBlackPositionEval =
            blackPiecePositionEval.endGamePosition + blackPawnEval.endGamePosition;

    const int earlyGamePositionEval = earlyGameWhitePositionEval - earlyGameBlackPositionEval;
    const int endGamePositionEval   = endGameWhitePositionEval - endGameBlackPositionEval;
    const int positionEval          = (int)(earlyGamePositionEval * (1.f - endGameFactor)
                                   + endGamePositionEval * endGameFactor);

    const int whiteSwarmingValue = evaluateKingSwarming(
            gameState, Side::White, whiteMaterial, blackMaterial, endGameFactor);
    const int blackSwarmingValue = evaluateKingSwarming(
            gameState, Side::Black, blackMaterial, whiteMaterial, endGameFactor);
    const int swarmingEval = whiteSwarmingValue - blackSwarmingValue;

    const int eval = materialEval + positionEval + swarmingEval;

    return (EvalT)clamp(eval, -kMateEval + 1'000, kMateEval - 1'000);
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

StackVector<int> scoreMoves(
        const StackVector<Move>& moves, const GameState& gameState, StackOfVectors<int>& stack) {
    StackVector<int> scores = stack.makeStackVector();

    static constexpr int kCaptureBonus   = 2'000;
    static constexpr int kPromotionBonus = 2'000;

    for (const Move& move : moves) {
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

            moveScore += kPieceSquareTablesEarly[(int)capturedPiece][(int)captureTarget];
        }

        moveScore -= kPieceSquareTablesEarly[(int)move.pieceToMove][(int)move.from];

        if (auto promotionPiece = getPromotionPiece(move.flags); promotionPiece != Piece::Pawn) {
            moveScore += kPromotionBonus;

            moveScore += kPieceValues[(int)promotionPiece];
            moveScore -= kPieceValues[(int)Piece::Pawn];

            moveScore += kPieceSquareTablesEarly[(int)promotionPiece][(int)move.to];
        } else {
            moveScore += kPieceSquareTablesEarly[(int)move.pieceToMove][(int)move.to];
        }

        scores.push_back(moveScore);
    }

    scores.lock();
    return scores;
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
