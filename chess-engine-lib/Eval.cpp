#include "Eval.h"

#include "Macros.h"
#include "Math.h"
#include "PawnMasks.h"

#include <array>
#include <optional>
#include <utility>

namespace {

constexpr std::array<int, kNumPieceTypes> kPieceValues = {
        100,  // Pawn
        305,  // Knight
        308,  // Bishop
        563,  // Rook
        950,  // Queen
        0,    // King
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

using SquareTable       = std::array<int, kSquares>;
using PieceSquareTables = std::array<SquareTable, kNumPieceTypes>;

// clang-format off
constexpr PieceSquareTables kPieceSquareTablesWhiteEarly = {
    // Pawns - stand in front of king and promote
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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

constexpr PieceSquareTables kPieceSquareTablesWhiteLate = {
    // Pawns - promote
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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
    SquareTable {
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
constexpr int kDoubledPawnPenalty     = 20;
constexpr int kIsolatedPawnPenalty    = 30;

// Penalty for having 0...8 own pawns on the same color as a bishop
constexpr std::array<int, 9> kBadBishopPenalty = {-40, -30, -20, -10, 0, 10, 20, 30, 40};
constexpr int kBishopPairBonus                 = 50;

constexpr int kRookSemiOpenFileBonus = 10;
constexpr int kRookOpenFileBonus     = 20;

constexpr SquareTable getReflectedSquareTable(const SquareTable& table) {
    SquareTable result{};

    for (int i = 0; i < kSquares; ++i) {
        const BoardPosition position          = (BoardPosition)i;
        const BoardPosition reflectedPosition = getVerticalReflection(position);

        result[i] = table[(int)reflectedPosition];
    }

    return result;
}

constexpr PieceSquareTables getReflectedPieceSquareTables(const PieceSquareTables& tables) {
    PieceSquareTables result{};

    for (int i = 0; i < kNumPieceTypes; ++i) {
        result[i] = getReflectedSquareTable(tables[i]);
    }

    return result;
}

constexpr std::array<PieceSquareTables, kNumSides> kPieceSquareTablesEarly = {
        kPieceSquareTablesWhiteEarly,
        getReflectedPieceSquareTables(kPieceSquareTablesWhiteEarly),
};

constexpr std::array<PieceSquareTables, kNumSides> kPieceSquareTablesLate = {
        kPieceSquareTablesWhiteLate,
        getReflectedPieceSquareTables(kPieceSquareTablesWhiteLate),
};

struct PiecePositionEvaluation {
    int material          = 0;
    int phaseMaterial     = 0;
    int earlyGamePosition = 0;
    int endGamePosition   = 0;
};

FORCE_INLINE void updatePiecePositionEvaluation(
        const int pieceIdx,
        BoardPosition position,
        const Side side,
        PiecePositionEvaluation& result) {

    result.material += kPieceValues[pieceIdx];
    result.phaseMaterial += kPhaseMaterialValues[pieceIdx];

    result.earlyGamePosition += kPieceSquareTablesEarly[(int)side][pieceIdx][(int)position];
    result.endGamePosition += kPieceSquareTablesLate[(int)side][pieceIdx][(int)position];
}

[[nodiscard]] FORCE_INLINE PiecePositionEvaluation
evaluatePiecePositionsForSide(const GameState& gameState, const Side side) {
    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);
    const BitBoard anyPawn    = ownPawns | enemyPawns;

    PiecePositionEvaluation result{};

    // Knights
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Knight);

        while (pieceBitBoard != BitBoard::Empty) {
            BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation((int)Piece::Knight, position, side, result);
        }
    }

    // Bishops
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Bishop);

        const std::array<int, 2> ownPawnsPerSquareColor = {
                popCount(ownPawns & kDarkSquareBitBoard),
                popCount(ownPawns & kLightSquareBitBoard),
        };

        std::array<bool, 2> hasBishopOfColor = {false, false};

        while (pieceBitBoard != BitBoard::Empty) {
            BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation((int)Piece::Bishop, position, side, result);

            const int squareColor = getSquareColor(position);

            hasBishopOfColor[squareColor] = true;

            result.material -= kBadBishopPenalty[ownPawnsPerSquareColor[squareColor]];
        }

        if (hasBishopOfColor[0] && hasBishopOfColor[1]) {
            result.material += kBishopPairBonus;
        }
    }

    // Rooks
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Rook);

        while (pieceBitBoard != BitBoard::Empty) {
            BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation((int)Piece::Rook, position, side, result);

            const BitBoard fileBitBoard = getFileBitBoard(position);
            const bool blockedByOwnPawn = (ownPawns & fileBitBoard) != BitBoard::Empty;
            const bool blockedByAnyPawn = (anyPawn & fileBitBoard) != BitBoard::Empty;

            if (!blockedByAnyPawn) {
                result.earlyGamePosition += kRookOpenFileBonus;
                result.endGamePosition += kRookOpenFileBonus;
            } else if (!blockedByOwnPawn) {
                result.earlyGamePosition += kRookSemiOpenFileBonus;
                result.endGamePosition += kRookSemiOpenFileBonus;
            }
        }
    }

    // Queens, King
    for (int pieceIdx = (int)Piece::Queen; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        const Piece piece      = (Piece)pieceIdx;
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation(pieceIdx, position, side, result);
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

        updatePiecePositionEvaluation((int)Piece::Pawn, position, side, result);

        const BitBoard passedPawnOpponentMask = getPassedPawnOpponentMask(position, side);
        const BitBoard forwardMask            = getPawnForwardMask(position, side);
        const BitBoard neighborMask           = getPawnNeighborFileMask(position);

        const BitBoard opponentBlockers = enemyPawns & passedPawnOpponentMask;
        const BitBoard ownBlockers      = ownPawns & forwardMask;
        const BitBoard ownNeighbors     = ownPawns & neighborMask;

        const bool isDoubledPawn = ownBlockers != BitBoard::Empty;
        const bool isPassedPawn  = !isDoubledPawn && opponentBlockers == BitBoard::Empty;
        const bool isIsolated    = ownNeighbors == BitBoard::Empty;

        if (isDoubledPawn) {
            result.earlyGamePosition -= kDoubledPawnPenalty;
            result.endGamePosition -= kDoubledPawnPenalty;
        } else if (isPassedPawn) {
            const int rank                = rankFromPosition(position);
            const int distanceToPromotion = side == Side::White ? kRanks - 1 - rank : rank;

            result.earlyGamePosition += kPassedPawnBonus[distanceToPromotion];
            result.endGamePosition += kPassedPawnBonus[distanceToPromotion];
        }

        if (isIsolated) {
            result.earlyGamePosition -= kIsolatedPawnPenalty;
            result.endGamePosition -= kIsolatedPawnPenalty;
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

    return min(std::abs(file - kFiles / 2), std::abs(rank - kRanks / 2));
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
            min((float)(swarmingMaterial - defendingMaterial) / (float)rookValue, 1.f);

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
    const auto whitePawnEval = evaluatePawnsForSide(gameState, Side::White);
    const auto blackPawnEval = evaluatePawnsForSide(gameState, Side::Black);

    const auto whitePiecePositionEval = evaluatePiecePositionsForSide(gameState, Side::White);
    const auto blackPiecePositionEval = evaluatePiecePositionsForSide(gameState, Side::Black);

    const int whiteMaterial = whitePiecePositionEval.material + whitePawnEval.material;
    const int blackMaterial = blackPiecePositionEval.material + blackPawnEval.material;

    const int materialEval = whiteMaterial - blackMaterial;

    const int phaseMaterial =
            min(whitePiecePositionEval.phaseMaterial + blackPiecePositionEval.phaseMaterial,
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
    if (gameState.isThreeFoldRepetition()) {
        return 0;
    }

    if (isInsufficientMaterial(gameState)) {
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

[[nodiscard]] FORCE_INLINE std::pair<bool, bool> insufficientMaterialForSides(
        const GameState& gameState) {
    bool whiteInsufficientMaterial = false;
    bool blackInsufficientMaterial = false;

    const bool noWhitePawns =
            gameState.getPieceBitBoard(Side::White, Piece::Pawn) == BitBoard::Empty;
    const bool noWhiteRooks =
            gameState.getPieceBitBoard(Side::White, Piece::Rook) == BitBoard::Empty;
    const bool noWhiteQueens =
            gameState.getPieceBitBoard(Side::White, Piece::Queen) == BitBoard::Empty;

    const bool whiteOnlyHasMinorPieces = noWhitePawns && noWhiteRooks && noWhiteQueens;

    const int numWhiteKnights = popCount(gameState.getPieceBitBoard(Side::White, Piece::Knight));
    const int numWhiteBishops = popCount(gameState.getPieceBitBoard(Side::White, Piece::Bishop));

    const bool whiteOnlyHasAKing =
            whiteOnlyHasMinorPieces && numWhiteKnights == 0 && numWhiteBishops == 0;

    const bool noBlackPawns =
            gameState.getPieceBitBoard(Side::Black, Piece::Pawn) == BitBoard::Empty;
    const bool noBlackRooks =
            gameState.getPieceBitBoard(Side::Black, Piece::Rook) == BitBoard::Empty;
    const bool noBlackQueens =
            gameState.getPieceBitBoard(Side::Black, Piece::Queen) == BitBoard::Empty;

    const bool blackOnlyHasMinorPieces = noBlackPawns && noBlackRooks && noBlackQueens;

    const int numBlackKnights = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Knight));
    const int numBlackBishops = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Bishop));

    const bool blackOnlyHasAKing =
            blackOnlyHasMinorPieces && numBlackKnights == 0 && numBlackBishops == 0;

    if (whiteOnlyHasMinorPieces) {
        if (numWhiteKnights == 0 && numWhiteBishops <= 1) {
            whiteInsufficientMaterial = true;
        } else if (numWhiteBishops == 0 && numWhiteKnights <= 1) {
            whiteInsufficientMaterial = true;
        }
    }

    if (blackOnlyHasMinorPieces) {
        if (numBlackKnights == 0 && numBlackBishops <= 1) {
            blackInsufficientMaterial = true;
        } else if (numBlackBishops == 0 && numBlackKnights <= 1) {
            blackInsufficientMaterial = true;
        }
    }

    if (whiteOnlyHasAKing && blackOnlyHasMinorPieces && numBlackBishops == 0
        && numBlackKnights == 2) {
        blackInsufficientMaterial = true;
    }

    if (blackOnlyHasAKing && whiteOnlyHasMinorPieces && numWhiteBishops == 0
        && numWhiteKnights == 2) {
        whiteInsufficientMaterial = true;
    }

    return {whiteInsufficientMaterial, blackInsufficientMaterial};
}

}  // namespace

FORCE_INLINE int getStaticPieceValue(const Piece piece) {
    static constexpr std::array<int, kNumPieceTypes> kStaticPieceValues = {
            100,     // Pawn
            305,     // Knight
            308,     // Bishop
            563,     // Rook
            950,     // Queen
            20'000,  // King
    };

    return kStaticPieceValues[(int)piece];
}

FORCE_INLINE int getPieceSquareValue(const Piece piece, BoardPosition position, const Side side) {
    return kPieceSquareTablesEarly[(int)side][(int)piece][(int)position];
}

FORCE_INLINE bool isInsufficientMaterial(const GameState& gameState) {
    const auto [whiteInsufficientMaterial, blackInsufficientMaterial] =
            insufficientMaterialForSides(gameState);

    return whiteInsufficientMaterial && blackInsufficientMaterial;
}

[[nodiscard]] FORCE_INLINE EvalT evaluateNoLegalMoves(const GameState& gameState) {
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

    EvalT whiteEval = evaluateForWhite(gameState);

    return gameState.getSideToMove() == Side::White ? whiteEval : -whiteEval;
}
