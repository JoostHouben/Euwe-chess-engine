#include "Eval.h"

#include "BitBoard.h"
#include "Macros.h"
#include "Math.h"
#include "PawnMasks.h"
#include "PieceControl.h"

#include <array>
#include <optional>
#include <utility>

namespace {

using EvalCalcT = float;

constexpr std::array<EvalCalcT, kNumPieceTypes> kPieceValues = {
        100,  // Pawn
        325,  // Knight
        335,  // Bishop
        500,  // Rook
        975,  // Queen
        0,    // King
};

constexpr std::array<EvalCalcT, kNumPieceTypes> kPhaseMaterialValues = {
        0,  // Pawn
        1,  // Knight
        1,  // Bishop
        2,  // Rook
        4,  // Queen
        0,  // King
};

constexpr EvalCalcT kMaxPhaseMaterial = 24;

using SquareTable       = std::array<EvalCalcT, kSquares>;
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

constexpr std::array kPassedPawnBonus    = {0, 90, 60, 40, 25, 15, 15};
constexpr EvalCalcT kDoubledPawnPenalty  = 20;
constexpr EvalCalcT kIsolatedPawnPenalty = 30;

// Penalty for having 0...8 own pawns on the same color as a bishop
constexpr std::array<EvalCalcT, 9> kBadBishopPenalty = {-40, -30, -20, -10, 0, 10, 20, 30, 40};

constexpr EvalCalcT kBishopPairBonus   = 30;
constexpr EvalCalcT kKnightPairPenalty = 8;
constexpr EvalCalcT kRookPairPenalty   = 16;

constexpr EvalCalcT kRookSemiOpenFileBonus = 10;
constexpr EvalCalcT kRookOpenFileBonus     = 20;

constexpr std::array<EvalCalcT, 9> kKnightPawnAdjustment = {-20, -16, -12, -8, -4, 0, 4, 8, 12};
constexpr std::array<EvalCalcT, 9> kRookPawnAdjustment   = {15, 12, 9, 6, 3, 0, -3, -6, -9};

constexpr EvalCalcT kKingVirtualMobilityPenalty = 3;

constexpr std::array<EvalCalcT, kNumPieceTypes> kMobilityBonusEarly = {
        0,  // pawns
        1,  // knights
        2,  // bishops
        2,  // rooks
        1,  // queens
        0,  // kings
};

constexpr std::array<EvalCalcT, kNumPieceTypes> kMobilityBonusLate = {
        0,  // pawns
        1,  // knights
        2,  // bishops
        4,  // rooks
        2,  // queens
        0,  // kings
};

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
    EvalCalcT material           = 0;
    EvalCalcT materialAdjustment = 0;
    EvalCalcT phaseMaterial      = 0;
    EvalCalcT earlyGamePosition  = 0;
    EvalCalcT endGamePosition    = 0;
};

FORCE_INLINE void updatePiecePositionEvaluation(
        const int pieceIdx,
        const BoardPosition position,
        const Side side,
        PiecePositionEvaluation& result) {

    result.material += kPieceValues[pieceIdx];
    result.phaseMaterial += kPhaseMaterialValues[pieceIdx];

    result.earlyGamePosition += kPieceSquareTablesEarly[(int)side][pieceIdx][(int)position];
    result.endGamePosition += kPieceSquareTablesLate[(int)side][pieceIdx][(int)position];
}

FORCE_INLINE void updateMobilityEvaluation(
        const Piece piece,
        const BoardPosition position,
        const BitBoard anyPiece,
        const BitBoard ownOccupancy,
        PiecePositionEvaluation& result) {
    const BitBoard control = getPieceControlledSquares(piece, position, anyPiece);
    const int mobility     = popCount(control & ~ownOccupancy);
    result.earlyGamePosition += mobility * kMobilityBonusEarly[(int)piece];
    result.endGamePosition += mobility * kMobilityBonusLate[(int)piece];
}

[[nodiscard]] FORCE_INLINE EvalCalcT
manhattanDistance(const BoardPosition a, const BoardPosition b) {
    const auto [aFile, aRank] = fileRankFromPosition(a);
    const auto [bFile, bRank] = fileRankFromPosition(b);

    return (EvalCalcT)(std::abs(aFile - bFile) + std::abs(aRank - bRank));
}

[[nodiscard]] FORCE_INLINE EvalCalcT manhattanDistanceToCenter(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    return (EvalCalcT)min(std::abs(file - kFiles / 2), std::abs(rank - kRanks / 2));
}

[[nodiscard]] FORCE_INLINE EvalCalcT bishopDistance(const BoardPosition a, const BoardPosition b) {
    const auto [aFile, aRank] = fileRankFromPosition(a);
    const auto [bFile, bRank] = fileRankFromPosition(b);

    const int aDiag = aFile + aRank;
    const int bDiag = bFile + bRank;

    const int aAntiDiag = aFile - aRank;
    const int bAntiDiag = bFile - bRank;

    return (EvalCalcT)(std::abs(aDiag - bDiag) + std::abs(aAntiDiag - bAntiDiag));
}

[[nodiscard]] FORCE_INLINE EvalCalcT queenDistance(const BoardPosition a, const BoardPosition b) {
    const auto [aFile, aRank] = fileRankFromPosition(a);
    const auto [bFile, bRank] = fileRankFromPosition(b);

    return (EvalCalcT)max(std::abs(aFile - bFile), std::abs(aRank - bRank));
}

[[nodiscard]] FORCE_INLINE PiecePositionEvaluation
evaluatePiecePositionsForSide(const GameState& gameState, const Side side) {
    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);
    const BitBoard anyPawn    = ownPawns | enemyPawns;

    const BitBoard ownOccupancy = side == gameState.getSideToMove()
                                        ? gameState.getOccupancy().ownPiece
                                        : gameState.getOccupancy().enemyPiece;

    const BitBoard anyPiece =
            gameState.getOccupancy().ownPiece | gameState.getOccupancy().enemyPiece;

    const int numOwnPawns = popCount(ownPawns);

    const BoardPosition enemyKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(nextSide(side), Piece::King));

    PiecePositionEvaluation result{};

    // Knights
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Knight);

        const int numKnights = popCount(pieceBitBoard);
        if (numKnights >= 2) {
            result.materialAdjustment -= kKnightPairPenalty;
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation((int)Piece::Knight, position, side, result);

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = max((EvalCalcT)0, 7 - kingDistance);
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            result.materialAdjustment += kKnightPawnAdjustment[numOwnPawns];

            updateMobilityEvaluation(Piece::Knight, position, anyPiece, ownOccupancy, result);
        }
    }

    // Bishops
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Bishop);

        const std::array<int, 2> ownPawnsPerSquareColor = {
                popCount(ownPawns & kDarkSquareBitBoard),
                popCount(ownPawns & kLightSquareBitBoard),
        };
        const std::array<int, 2> badBishopIndex = {
                ownPawnsPerSquareColor[0] + (8 - numOwnPawns) / 2,
                ownPawnsPerSquareColor[1] + (8 - numOwnPawns) / 2,
        };

        std::array<bool, 2> hasBishopOfColor = {false, false};

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation((int)Piece::Bishop, position, side, result);

            const int squareColor = getSquareColor(position);

            hasBishopOfColor[squareColor] = true;

            result.materialAdjustment -= kBadBishopPenalty[badBishopIndex[squareColor]];

            const EvalCalcT kingDistance = bishopDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = (14 - kingDistance) / 2;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            updateMobilityEvaluation(Piece::Bishop, position, anyPiece, ownOccupancy, result);
        }

        if (hasBishopOfColor[0] && hasBishopOfColor[1]) {
            result.materialAdjustment += kBishopPairBonus;
        }
    }

    // Rooks
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Rook);

        const int numRooks = popCount(pieceBitBoard);
        if (numRooks >= 2) {
            result.materialAdjustment -= kRookPairPenalty;
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
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

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = 14 - kingDistance;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            result.materialAdjustment += kRookPawnAdjustment[numOwnPawns];

            updateMobilityEvaluation(Piece::Rook, position, anyPiece, ownOccupancy, result);
        }
    }

    // Queens
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Queen);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation((int)Piece::Queen, position, side, result);

            const EvalCalcT kingDistance = queenDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = (7 - kingDistance) * 4;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            updateMobilityEvaluation(Piece::Queen, position, anyPiece, ownOccupancy, result);
        }
    }

    // King
    {
        const BoardPosition kingPosition =
                getFirstSetPosition(gameState.getPieceBitBoard(side, Piece::King));
        updatePiecePositionEvaluation((int)Piece::King, kingPosition, side, result);

        // no mobility bonus for king
    }

    return result;
}

[[nodiscard]] FORCE_INLINE bool isDrawish(
        const GameState& gameState, const EvalCalcT whiteMaterial, const EvalCalcT blackMaterial) {
    const bool whiteIsStronger = whiteMaterial >= blackMaterial;

    const int strongerSidePawns =
            whiteIsStronger ? popCount(gameState.getPieceBitBoard(Side::White, Piece::Pawn))
                            : popCount(gameState.getPieceBitBoard(Side::Black, Piece::Pawn));

    if (strongerSidePawns > 0) {
        return false;
    }

    int strongSideKnights = popCount(gameState.getPieceBitBoard(Side::White, Piece::Knight));
    int strongSideBishops = popCount(gameState.getPieceBitBoard(Side::White, Piece::Bishop));
    int strongSideRooks   = popCount(gameState.getPieceBitBoard(Side::White, Piece::Rook));
    int strongSideQueens  = popCount(gameState.getPieceBitBoard(Side::White, Piece::Queen));

    int weakSideKnights = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Knight));
    int weakSideBishops = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Bishop));
    int weakSideRooks   = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Rook));
    int weakSideQueens  = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Queen));

    if (!whiteIsStronger) {
        std::swap(strongSideKnights, weakSideKnights);
        std::swap(strongSideBishops, weakSideBishops);
        std::swap(strongSideRooks, weakSideRooks);
        std::swap(strongSideQueens, weakSideQueens);
    }

    const int strongSideMinorPieces = strongSideKnights + strongSideBishops;
    const int strongSideMajorPieces = strongSideRooks + strongSideQueens;

    const int weakSideMinorPieces = weakSideKnights + weakSideBishops;
    const int weakSideMajorPieces = weakSideRooks + weakSideQueens;

    // With only a single minor piece you can't reliably deliver mate.
    if (strongSideMajorPieces == 0 && strongSideMinorPieces == 1) {
        return true;
    }

    // Only two knights; this is insufficient material once the weaker side has lost their material.
    if (strongSideMajorPieces == 0 && strongSideKnights == 2 && strongSideBishops == 0) {
        return true;
    }

    // Rook vs a minor piece is drawish.
    if (strongSideRooks == 1 && strongSideQueens == 0 && strongSideMinorPieces == 0
        && (weakSideMinorPieces == 1 && weakSideMajorPieces == 0)) {
        return true;
    }

    // Rook and minor vs rook is drawish.
    if (strongSideRooks == 1 && strongSideQueens == 0 && strongSideMinorPieces == 1
        && weakSideRooks == 1 && weakSideQueens == 0 && weakSideMinorPieces == 0) {
        return true;
    }

    return false;
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

[[nodiscard]] FORCE_INLINE EvalCalcT
evaluateKingSafety(const GameState& gameState, const Side side) {
    const BoardPosition kingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(side, Piece::King));

    const BitBoard ownOccupancy = side == gameState.getSideToMove()
                                        ? gameState.getOccupancy().ownPiece
                                        : gameState.getOccupancy().enemyPiece;

    // Consider all of our own pieces as blockers, but for the enemy pieces we only consider pawns.
    // This is to account for the fact that the other enemy pieces are likely mobile and so should
    // not be relied upon to protect the king from sliding attacks.
    const BitBoard blockers =
            ownOccupancy | gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);

    // We're only interested in squares from which an enemy slider could attack the king, so we
    // exclude the occupied squares themselves.
    const BitBoard virtualKingControl =
            getPieceControlledSquares(Piece::Queen, kingPosition, blockers) & ~blockers;

    const int virtualKingMobility = popCount(virtualKingControl);

    return -kKingVirtualMobilityPenalty * virtualKingMobility;
}

[[nodiscard]] FORCE_INLINE EvalCalcT evaluateKingSwarming(
        const GameState& gameState,
        const Side swarmingSide,
        const EvalCalcT swarmingMaterial,
        const EvalCalcT defendingMaterial,
        const float endGameFactor) {
    if (defendingMaterial >= swarmingMaterial) {
        return 0;
    }

    static constexpr EvalCalcT rookValue = kPieceValues[(int)Piece::Rook];

    const float materialAdvantageFactor =
            min((float)(swarmingMaterial - defendingMaterial) / (float)rookValue, 1.f);

    const BoardPosition swarmingKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(swarmingSide, Piece::King));
    const BoardPosition defendingKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(nextSide(swarmingSide), Piece::King));

    const EvalCalcT defendingKingDistanceToCenter =
            manhattanDistanceToCenter(defendingKingPosition);

    const EvalCalcT kingDistance = manhattanDistance(swarmingKingPosition, defendingKingPosition);

    const EvalCalcT swarmingValue = defendingKingDistanceToCenter + (14 - kingDistance);

    return (EvalCalcT)(endGameFactor * materialAdvantageFactor * swarmingValue * 10.f);
}

[[nodiscard]] FORCE_INLINE EvalT evaluateForWhite(const GameState& gameState) {
    const auto whitePawnEval = evaluatePawnsForSide(gameState, Side::White);
    const auto blackPawnEval = evaluatePawnsForSide(gameState, Side::Black);

    const auto whitePiecePositionEval = evaluatePiecePositionsForSide(gameState, Side::White);
    const auto blackPiecePositionEval = evaluatePiecePositionsForSide(gameState, Side::Black);

    const EvalCalcT whiteMaterial = whitePiecePositionEval.material
                                  + whitePiecePositionEval.materialAdjustment
                                  + whitePawnEval.material + whitePawnEval.materialAdjustment;
    const EvalCalcT blackMaterial = blackPiecePositionEval.material
                                  + blackPiecePositionEval.materialAdjustment
                                  + blackPawnEval.material + blackPawnEval.materialAdjustment;

    EvalCalcT materialEval = whiteMaterial - blackMaterial;

    if (isDrawish(
                gameState,
                whitePiecePositionEval.material + whitePawnEval.material,
                blackPiecePositionEval.material + blackPawnEval.material)) {
        materialEval /= 2;
    }

    const EvalCalcT phaseMaterial =
            min(whitePiecePositionEval.phaseMaterial + blackPiecePositionEval.phaseMaterial,
                kMaxPhaseMaterial);
    const float earlyGameFactor = (float)phaseMaterial / (float)kMaxPhaseMaterial;
    const float endGameFactor   = 1.f - earlyGameFactor;

    const EvalCalcT earlyGameWhitePositionEval =
            whitePiecePositionEval.earlyGamePosition + whitePawnEval.earlyGamePosition;
    const EvalCalcT earlyGameBlackPositionEval =
            blackPiecePositionEval.earlyGamePosition + blackPawnEval.earlyGamePosition;

    const EvalCalcT endGameWhitePositionEval =
            whitePiecePositionEval.endGamePosition + whitePawnEval.endGamePosition;
    const EvalCalcT endGameBlackPositionEval =
            blackPiecePositionEval.endGamePosition + blackPawnEval.endGamePosition;

    const EvalCalcT earlyGamePositionEval = earlyGameWhitePositionEval - earlyGameBlackPositionEval;
    const EvalCalcT endGamePositionEval   = endGameWhitePositionEval - endGameBlackPositionEval;
    const EvalCalcT positionEval          = (EvalCalcT)(earlyGamePositionEval * earlyGameFactor
                                               + endGamePositionEval * endGameFactor);

    const EvalCalcT whiteKingSafety = evaluateKingSafety(gameState, Side::White);
    const EvalCalcT blackKingSafety = evaluateKingSafety(gameState, Side::Black);
    const EvalCalcT kingSafety = (EvalCalcT)((whiteKingSafety - blackKingSafety) * earlyGameFactor);

    const EvalCalcT whiteSwarmingValue = evaluateKingSwarming(
            gameState, Side::White, whiteMaterial, blackMaterial, endGameFactor);
    const EvalCalcT blackSwarmingValue = evaluateKingSwarming(
            gameState, Side::Black, blackMaterial, whiteMaterial, endGameFactor);
    const EvalCalcT swarmingEval = whiteSwarmingValue - blackSwarmingValue;

    const EvalCalcT eval = materialEval + positionEval + kingSafety + swarmingEval;

    const int roundedEval   = (int)(eval + 0.5f);
    const EvalT clampedEval = (EvalT)clamp(roundedEval, -kMateEval + 1'000, kMateEval - 1'000);
    return clampedEval;
}

[[nodiscard]] FORCE_INLINE std::optional<EvalT> evaluateEndState(
        const GameState& gameState, StackOfVectors<Move>& stack) {
    if (gameState.isRepetition()) {
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
    return (int)kPieceSquareTablesEarly[(int)side][(int)piece][(int)position];
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
