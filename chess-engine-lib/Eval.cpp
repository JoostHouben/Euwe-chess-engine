#include "Eval.h"

#include "BitBoard.h"
#include "Macros.h"
#include "Math.h"
#include "PawnMasks.h"
#include "PieceControl.h"

#include <array>
#include <optional>
#include <type_traits>
#include <utility>

namespace {

struct TaperedEvaluation {
    EvalCalcT early = 0;
    EvalCalcT late  = 0;
};

struct PiecePositionEvaluation {
    TaperedEvaluation material{};
    TaperedEvaluation position{};

    EvalCalcT phaseMaterial = 0;
};

VectorT zeros() {
    return std::valarray<double>((EvalCalcT)0, kNumEvalParams);
}

template <bool CalcJacobians>
using ParamGradient = std::conditional_t<CalcJacobians, VectorT, std::monostate>;

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE ParamGradient<CalcJacobians> zeroGradient() {
    if constexpr (CalcJacobians) {
        return zeros();
    } else {
        return std::monostate{};
    }
}

template <bool CalcJacobians>
struct TaperedEvaluationJacobians;

template <>
struct TaperedEvaluationJacobians<false> {};

template <>
struct TaperedEvaluationJacobians<true> {
    ParamGradient<true> early = zeroGradient<true>();
    ParamGradient<true> late  = zeroGradient<true>();
};

template <bool CalcJacobians>
struct PiecePositionEvaluationJacobians {
    TaperedEvaluationJacobians<CalcJacobians> material{};
    TaperedEvaluationJacobians<CalcJacobians> position{};

    ParamGradient<CalcJacobians> phaseMaterialJacobians = zeroGradient<CalcJacobians>();
};

SquareTable getReflectedSquareTable(const SquareTable& table) {
    SquareTable result{};

    for (int i = 0; i < kSquares; ++i) {
        const BoardPosition position          = (BoardPosition)i;
        const BoardPosition reflectedPosition = getVerticalReflection(position);

        result[i] = table[(int)reflectedPosition];
    }

    return result;
}

PieceSquareTables getReflectedPieceSquareTables(const PieceSquareTables& tables) {
    PieceSquareTables result{};

    for (int i = 0; i < kNumPieceTypes; ++i) {
        result[i] = getReflectedSquareTable(tables[i]);
    }

    return result;
}

FORCE_INLINE std::size_t getParamIndex(const EvalParams& params, const EvalCalcT& param) {
    return (std::size_t)((std::byte*)&param - (std::byte*)&params) / sizeof(EvalCalcT);
}

template <bool CalcJacobians>
FORCE_INLINE void updateTaperedTerm(
        const Evaluator::EvalCalcParams& params,
        const TaperedTerm& term,
        TaperedEvaluation& eval,
        TaperedEvaluationJacobians<CalcJacobians>& jacobians,
        const EvalCalcT weight) {
    eval.early += term.early * weight;
    eval.late += term.late * weight;

    if constexpr (CalcJacobians) {
        jacobians.early[getParamIndex(params, term.early)] += weight;
        jacobians.late[getParamIndex(params, term.late)] += weight;
    }
}

template <bool CalcJacobians>
FORCE_INLINE void updatePiecePositionEvaluation(
        const Evaluator::EvalCalcParams& params,
        const int pieceIdx,
        const BoardPosition position,
        const Side side,
        PiecePositionEvaluation& result,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {
    result.phaseMaterial += params.phaseMaterialValues[pieceIdx];
    if constexpr (CalcJacobians) {
        jacobians.phaseMaterialJacobians[getParamIndex(
                params, params.phaseMaterialValues[pieceIdx])] += 1;
    }

    updateTaperedTerm(params, params.pieceValues[pieceIdx], result.material, jacobians.material, 1);

    // Manual update for position because of piece square table index mapping.

    result.position.early += params.pieceSquareTables[(int)side][pieceIdx][(int)position].early;
    result.position.late += params.pieceSquareTables[(int)side][pieceIdx][(int)position].late;

    if constexpr (CalcJacobians) {
        int positionForPieceSquare = (int)position;
        if (side == Side::Black) {
            positionForPieceSquare = (int)getVerticalReflection(position);
        }

        jacobians.position.early[getParamIndex(
                params, params.pieceSquareTablesWhite[pieceIdx][positionForPieceSquare].early)] +=
                1;
        jacobians.position.late[getParamIndex(
                params, params.pieceSquareTablesWhite[pieceIdx][positionForPieceSquare].late)] += 1;
    }
}

template <bool CalcJacobians>
FORCE_INLINE void updateMobilityEvaluation(
        const Evaluator::EvalCalcParams& params,
        const Piece piece,
        const BoardPosition position,
        const BitBoard anyPiece,
        const BitBoard ownOccupancy,
        PiecePositionEvaluation& result,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {
    const BitBoard control = getPieceControlledSquares(piece, position, anyPiece);
    const int mobility     = popCount(control & ~ownOccupancy);

    updateTaperedTerm(
            params,
            params.mobilityBonus[(int)piece],
            result.position,
            jacobians.position,
            mobility);
}

template <bool CalcJacobians>
FORCE_INLINE void updateForVirtualKingMobility(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const Side side,
        const BoardPosition kingPosition,
        PiecePositionEvaluation& result,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {

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

    updateTaperedTerm(
            params,
            params.kingVirtualMobilityPenalty,
            result.position,
            jacobians.position,
            -virtualKingMobility);
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

template <bool CalcJacobians>
FORCE_INLINE void evaluatePiecePositionsForSide(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const Side side,
        PiecePositionEvaluation& result,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {
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

    // Knights
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Knight);

        const int numKnights = popCount(pieceBitBoard);
        if (numKnights >= 2) {
            updateTaperedTerm(
                    params, params.knightPairBonus, result.material, jacobians.material, 1);
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Knight, position, side, result, jacobians);

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropism      = max((EvalCalcT)0, 7 - kingDistance);

            updateTaperedTerm(
                    params,
                    params.kingTropismBonus[(int)Piece::Knight],
                    result.position,
                    jacobians.position,
                    tropism);

            updateTaperedTerm(
                    params,
                    params.knightPawnAdjustment[numOwnPawns],
                    result.material,
                    jacobians.material,
                    1);

            updateMobilityEvaluation<CalcJacobians>(
                    params, Piece::Knight, position, anyPiece, ownOccupancy, result, jacobians);
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
            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Bishop, position, side, result, jacobians);

            const int squareColor = getSquareColor(position);

            hasBishopOfColor[squareColor] = true;

            updateTaperedTerm(
                    params,
                    params.bishopPawnSameColorBonus[badBishopIndex[squareColor]],
                    result.position,
                    jacobians.position,
                    1);

            const EvalCalcT kingDistance = bishopDistance(position, enemyKingPosition);
            const EvalCalcT tropism      = 14 - kingDistance;

            updateTaperedTerm(
                    params,
                    params.kingTropismBonus[(int)Piece::Bishop],
                    result.position,
                    jacobians.position,
                    tropism);

            updateMobilityEvaluation<CalcJacobians>(
                    params, Piece::Bishop, position, anyPiece, ownOccupancy, result, jacobians);
        }

        if (hasBishopOfColor[0] && hasBishopOfColor[1]) {
            updateTaperedTerm(
                    params, params.bishopPairBonus, result.material, jacobians.material, 1);
        }
    }

    // Rooks
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Rook);

        const int numRooks = popCount(pieceBitBoard);
        if (numRooks >= 2) {
            updateTaperedTerm(params, params.rookPairBonus, result.material, jacobians.material, 1);
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Rook, position, side, result, jacobians);

            const BitBoard fileBitBoard = getFileBitBoard(position);
            const bool blockedByOwnPawn = (ownPawns & fileBitBoard) != BitBoard::Empty;
            const bool blockedByAnyPawn = (anyPawn & fileBitBoard) != BitBoard::Empty;

            if (!blockedByAnyPawn) {
                updateTaperedTerm(
                        params, params.rookOpenFileBonus, result.position, jacobians.position, 1);
            } else if (!blockedByOwnPawn) {
                updateTaperedTerm(
                        params,
                        params.rookSemiOpenFileBonus,
                        result.position,
                        jacobians.position,
                        1);
            }

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropism      = 14 - kingDistance;

            updateTaperedTerm(
                    params,
                    params.kingTropismBonus[(int)Piece::Rook],
                    result.position,
                    jacobians.position,
                    tropism);

            updateTaperedTerm(
                    params,
                    params.rookPawnAdjustment[numOwnPawns],
                    result.material,
                    jacobians.material,
                    1);

            updateMobilityEvaluation<CalcJacobians>(
                    params, Piece::Rook, position, anyPiece, ownOccupancy, result, jacobians);
        }
    }

    // Queens
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Queen);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Queen, position, side, result, jacobians);

            const EvalCalcT kingDistance = queenDistance(position, enemyKingPosition);
            const EvalCalcT tropism      = 7 - kingDistance;

            updateTaperedTerm(
                    params,
                    params.kingTropismBonus[(int)Piece::Queen],
                    result.position,
                    jacobians.position,
                    tropism);

            updateMobilityEvaluation<CalcJacobians>(
                    params, Piece::Queen, position, anyPiece, ownOccupancy, result, jacobians);
        }
    }

    // King
    {
        const BoardPosition kingPosition =
                getFirstSetPosition(gameState.getPieceBitBoard(side, Piece::King));
        updatePiecePositionEvaluation<CalcJacobians>(
                params, (int)Piece::King, kingPosition, side, result, jacobians);

        // no mobility bonus for king

        updateForVirtualKingMobility<CalcJacobians>(
                params, gameState, side, kingPosition, result, jacobians);
    }
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

template <bool CalcJacobians>
FORCE_INLINE void evaluatePawnsForSide(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const Side side,
        PiecePositionEvaluation& result,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {
    // TODO: should we hash pawn structure and store pawn eval?

    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);

    BitBoard pawnBitBoard = ownPawns;

    while (pawnBitBoard != BitBoard::Empty) {
        const BoardPosition position = popFirstSetPosition(pawnBitBoard);

        updatePiecePositionEvaluation<CalcJacobians>(
                params, (int)Piece::Pawn, position, side, result, jacobians);

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
            updateTaperedTerm(
                    params, params.doubledPawnPenalty, result.position, jacobians.position, -1);
        } else if (isPassedPawn) {
            const int rank                = rankFromPosition(position);
            const int distanceToPromotion = side == Side::White ? kRanks - 1 - rank : rank;

            updateTaperedTerm(
                    params,
                    params.passedPawnBonus[distanceToPromotion],
                    result.position,
                    jacobians.position,
                    1);
        }

        if (isIsolated) {
            updateTaperedTerm(
                    params, params.isolatedPawnPenalty, result.position, jacobians.position, -1);
        }
    }
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE EvalCalcT evaluateKingSwarming(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const Side swarmingSide,
        const EvalCalcT swarmingMaterial,
        const EvalCalcT defendingMaterial,
        const ParamGradient<CalcJacobians>& swarmingMaterialGradient,
        const ParamGradient<CalcJacobians>& defendingMaterialGradient,
        ParamGradient<CalcJacobians>& gradient) {
    if (defendingMaterial >= swarmingMaterial) {
        return 0;
    }

    const EvalCalcT rookValue = params.pieceValues[(int)Piece::Rook].late;

    const float materialAdvantageFactor =
            min((float)(swarmingMaterial - defendingMaterial) / (float)rookValue, 1.f);

    ParamGradient<CalcJacobians> materialAdvantageFactorGradient;
    if constexpr (CalcJacobians) {
        if (materialAdvantageFactor < 1.f) {
            const ParamGradient<CalcJacobians> numGradient =
                    swarmingMaterialGradient - defendingMaterialGradient;
            ParamGradient<CalcJacobians> denGradient = zeroGradient<CalcJacobians>();
            denGradient[getParamIndex(params, params.pieceValues[(int)Piece::Rook].late)] = 1;

            const EvalCalcT num = swarmingMaterial - defendingMaterial;
            const EvalCalcT den = rookValue;

            materialAdvantageFactorGradient = (numGradient * den - num * denGradient) / (den * den);
        } else {
            materialAdvantageFactorGradient = zeroGradient<CalcJacobians>();
        }
    }

    const BoardPosition swarmingKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(swarmingSide, Piece::King));
    const BoardPosition defendingKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(nextSide(swarmingSide), Piece::King));

    const EvalCalcT defendingKingDistanceToCenter =
            manhattanDistanceToCenter(defendingKingPosition);

    const EvalCalcT kingDistance = manhattanDistance(swarmingKingPosition, defendingKingPosition);

    const EvalCalcT swarmingValue = defendingKingDistanceToCenter + (14 - kingDistance);

    if constexpr (CalcJacobians) {
        gradient += materialAdvantageFactorGradient * swarmingValue * 10.f;
    }

    return (EvalCalcT)(materialAdvantageFactor * swarmingValue * 10.f);
}

[[nodiscard]] FORCE_INLINE ParamGradient<true> getMaxPhaseMaterialGradient(
        const EvalParams& params) {
    /*
    maxPhaseMaterial = 2 * 8 * evalParams.phaseMaterialValues[(int)Piece::Pawn]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Knight]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Bishop]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Rook]
                     + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::Queen]
                     + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::King];
    */
    ParamGradient<true> gradient = zeroGradient<true>();
    gradient[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Pawn])] += 2 * 8;
    gradient[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Knight])] += 2 * 2;
    gradient[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Bishop])] += 2 * 2;
    gradient[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Rook])] += 2 * 2;
    gradient[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Queen])] += 2 * 1;
    gradient[getParamIndex(params, params.phaseMaterialValues[(int)Piece::King])] += 2 * 1;
    return gradient;
}

[[nodiscard]] FORCE_INLINE EvalCalcT calcTaperedValue(
        const EvalCalcT earlyValue,
        const EvalCalcT lateValue,
        const EvalCalcT earlyFactor,
        const EvalCalcT lateFactor) {
    return earlyValue * earlyFactor + lateValue * lateFactor;
}

[[nodiscard]] ParamGradient<true> calcTaperedGradient(
        const VectorT& earlyGradient,
        const VectorT& lateGradient,
        const VectorT& earlyFactorGradient,
        const EvalCalcT earlyValue,
        const EvalCalcT lateValue,
        const EvalCalcT earlyFactor,
        const EvalCalcT lateFactor) {
    return earlyGradient * earlyFactor + earlyFactorGradient * earlyValue
         + lateGradient * lateFactor - earlyFactorGradient * lateValue;
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE std::tuple<float, float, ParamGradient<CalcJacobians>> calcTaperParams(
        const Evaluator::EvalCalcParams& params,
        const PiecePositionEvaluation& whitePiecePositionEval,
        const PiecePositionEvaluation& blackPiecePositionEval,
        const PiecePositionEvaluationJacobians<CalcJacobians>& whitePiecePositionJacobians,
        const PiecePositionEvaluationJacobians<CalcJacobians>& blackPiecePositionJacobians) {

    const EvalCalcT phaseMaterial =
            whitePiecePositionEval.phaseMaterial + blackPiecePositionEval.phaseMaterial;
    const float earlyFactor = (float)phaseMaterial / (float)params.maxPhaseMaterial;
    const float lateFactor  = 1.f - earlyFactor;

    ParamGradient<CalcJacobians> earlyFactorGradient;
    if constexpr (CalcJacobians) {
        const ParamGradient<CalcJacobians> phaseMaterialGradient =
                whitePiecePositionJacobians.phaseMaterialJacobians
                + blackPiecePositionJacobians.phaseMaterialJacobians;

        const ParamGradient<CalcJacobians> maxPhaseMaterialGradient =
                getMaxPhaseMaterialGradient(params);

        earlyFactorGradient = (phaseMaterialGradient * params.maxPhaseMaterial
                               - phaseMaterial * maxPhaseMaterialGradient)
                            / (params.maxPhaseMaterial * params.maxPhaseMaterial);
    }

    return {earlyFactor, lateFactor, earlyFactorGradient};
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE std::tuple<EvalCalcT, ParamGradient<CalcJacobians>> calcMaterialEval(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const PiecePositionEvaluation& whitePiecePositionEval,
        const PiecePositionEvaluation& blackPiecePositionEval,
        const PiecePositionEvaluationJacobians<CalcJacobians>& whitePiecePositionJacobians,
        const PiecePositionEvaluationJacobians<CalcJacobians>& blackPiecePositionJacobians,
        const float earlyFactor,
        const float lateFactor,
        const ParamGradient<CalcJacobians>& earlyFactorGradient) {
    const EvalCalcT earlyMaterial =
            whitePiecePositionEval.material.early - blackPiecePositionEval.material.early;
    const EvalCalcT lateMaterial =
            whitePiecePositionEval.material.late - blackPiecePositionEval.material.late;

    EvalCalcT materialEval = calcTaperedValue(earlyMaterial, lateMaterial, earlyFactor, lateFactor);

    ParamGradient<CalcJacobians> materialGradient;
    if constexpr (CalcJacobians) {
        const ParamGradient<CalcJacobians> earlyMaterialGradient =
                whitePiecePositionJacobians.material.early
                - blackPiecePositionJacobians.material.early;

        const ParamGradient<CalcJacobians> lateMaterialGradient =
                whitePiecePositionJacobians.material.late
                - blackPiecePositionJacobians.material.late;

        materialGradient = calcTaperedGradient(
                earlyMaterialGradient,
                lateMaterialGradient,
                earlyFactorGradient,
                earlyMaterial,
                lateMaterial,
                earlyFactor,
                lateFactor);
    }

    if (isDrawish(
                gameState,
                whitePiecePositionEval.material.late,
                blackPiecePositionEval.material.late)) {
        materialEval /= 2;

        if constexpr (CalcJacobians) {
            materialGradient /= 2;
        }
    }

    return {materialEval, materialGradient};
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE std::tuple<EvalCalcT, ParamGradient<CalcJacobians>> calcPositionEval(
        const PiecePositionEvaluation& whitePiecePositionEval,
        const PiecePositionEvaluation& blackPiecePositionEval,
        const PiecePositionEvaluationJacobians<CalcJacobians>& whitePiecePositionJacobians,
        const PiecePositionEvaluationJacobians<CalcJacobians>& blackPiecePositionJacobians,
        const float earlyFactor,
        const float lateFactor,
        const ParamGradient<CalcJacobians>& earlyFactorGradient) {
    const EvalCalcT earlyPositionEval =
            whitePiecePositionEval.position.early - blackPiecePositionEval.position.early;
    const EvalCalcT latePositionEval =
            whitePiecePositionEval.position.late - blackPiecePositionEval.position.late;
    const EvalCalcT positionEval =
            (EvalCalcT)(earlyPositionEval * earlyFactor + latePositionEval * lateFactor);

    ParamGradient<CalcJacobians> positionGradient;
    if constexpr (CalcJacobians) {
        const ParamGradient<CalcJacobians> earlyPositionGradient =
                whitePiecePositionJacobians.position.early
                - blackPiecePositionJacobians.position.early;

        const ParamGradient<CalcJacobians> latePositionGradient =
                whitePiecePositionJacobians.position.late
                - blackPiecePositionJacobians.position.late;

        positionGradient = calcTaperedGradient(
                earlyPositionGradient,
                latePositionGradient,
                earlyFactorGradient,
                earlyPositionEval,
                latePositionEval,
                earlyFactor,
                lateFactor);
    }

    return {positionEval, positionGradient};
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE std::tuple<EvalCalcT, ParamGradient<CalcJacobians>>
evalAndCalcKingSwarming(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const PiecePositionEvaluation& whitePiecePositionEval,
        const PiecePositionEvaluation& blackPiecePositionEval,
        const PiecePositionEvaluationJacobians<CalcJacobians>& whitePiecePositionJacobians,
        const PiecePositionEvaluationJacobians<CalcJacobians>& blackPiecePositionJacobians,
        const float lateFactor,
        const ParamGradient<CalcJacobians>& earlyFactorGradient) {
    ParamGradient<CalcJacobians> whiteLateMaterialGradient;
    ParamGradient<CalcJacobians> blackLateMaterialGradient;
    if constexpr (CalcJacobians) {
        whiteLateMaterialGradient = whitePiecePositionJacobians.material.late;
        blackLateMaterialGradient = blackPiecePositionJacobians.material.late;
    }

    ParamGradient<CalcJacobians> whiteSwarmingGradient = zeroGradient<CalcJacobians>();
    ParamGradient<CalcJacobians> blackSwarmingGradient = zeroGradient<CalcJacobians>();

    const EvalCalcT whiteSwarmingValue = evaluateKingSwarming<CalcJacobians>(
            params,
            gameState,
            Side::White,
            whitePiecePositionEval.material.late,
            blackPiecePositionEval.material.late,
            whiteLateMaterialGradient,
            blackLateMaterialGradient,
            whiteSwarmingGradient);
    const EvalCalcT blackSwarmingValue = evaluateKingSwarming<CalcJacobians>(
            params,
            gameState,
            Side::Black,
            blackPiecePositionEval.material.late,
            whitePiecePositionEval.material.late,
            blackLateMaterialGradient,
            whiteLateMaterialGradient,
            blackSwarmingGradient);
    const EvalCalcT swarmingEval = lateFactor * (whiteSwarmingValue - blackSwarmingValue);

    ParamGradient<CalcJacobians> swarmingGradient;
    if constexpr (CalcJacobians) {
        const EvalCalcT endSwarmingValue = whiteSwarmingValue - blackSwarmingValue;
        const ParamGradient<CalcJacobians> endSwarmingGradient =
                whiteSwarmingGradient - blackSwarmingGradient;

        swarmingGradient =
                endSwarmingGradient * lateFactor - earlyFactorGradient * endSwarmingValue;
    }

    return {swarmingEval, swarmingGradient};
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE EvalCalcT evaluateForWhite(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        ParamGradient<CalcJacobians>& whiteEvalGradient) {
    PiecePositionEvaluation whitePiecePositionEval{};
    PiecePositionEvaluation blackPiecePositionEval{};

    PiecePositionEvaluationJacobians<CalcJacobians> whitePiecePositionJacobians;
    PiecePositionEvaluationJacobians<CalcJacobians> blackPiecePositionJacobians;

    evaluatePawnsForSide(
            params, gameState, Side::White, whitePiecePositionEval, whitePiecePositionJacobians);
    evaluatePawnsForSide(
            params, gameState, Side::Black, blackPiecePositionEval, blackPiecePositionJacobians);

    evaluatePiecePositionsForSide(
            params, gameState, Side::White, whitePiecePositionEval, whitePiecePositionJacobians);
    evaluatePiecePositionsForSide(
            params, gameState, Side::Black, blackPiecePositionEval, blackPiecePositionJacobians);

    const auto [earlyFactor, lateFactor, earlyFactorGradient] = calcTaperParams(
            params,
            whitePiecePositionEval,
            blackPiecePositionEval,
            whitePiecePositionJacobians,
            blackPiecePositionJacobians);

    const auto [materialEval, materialGradient] = calcMaterialEval(
            params,
            gameState,
            whitePiecePositionEval,
            blackPiecePositionEval,
            whitePiecePositionJacobians,
            blackPiecePositionJacobians,
            earlyFactor,
            lateFactor,
            earlyFactorGradient);

    const auto [positionEval, positionGradient] = calcPositionEval(
            whitePiecePositionEval,
            blackPiecePositionEval,
            whitePiecePositionJacobians,
            blackPiecePositionJacobians,
            earlyFactor,
            lateFactor,
            earlyFactorGradient);

    const auto [swarmingEval, swarmingGradient] = evalAndCalcKingSwarming(
            params,
            gameState,
            whitePiecePositionEval,
            blackPiecePositionEval,
            whitePiecePositionJacobians,
            blackPiecePositionJacobians,
            lateFactor,
            earlyFactorGradient);

    const EvalCalcT eval = materialEval + positionEval + swarmingEval;

    if constexpr (CalcJacobians) {
        whiteEvalGradient = materialGradient + positionGradient + swarmingGradient;
    }

    return eval;
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

Evaluator::EvalCalcParams::EvalCalcParams(const EvalParams& evalParams) : EvalParams(evalParams) {
    maxPhaseMaterial = 2 * 8 * evalParams.phaseMaterialValues[(int)Piece::Pawn]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Knight]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Bishop]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Rook]
                     + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::Queen]
                     + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::King];

    pieceSquareTables = {
            evalParams.pieceSquareTablesWhite,
            getReflectedPieceSquareTables(evalParams.pieceSquareTablesWhite)};
}

Evaluator::Evaluator() : Evaluator(EvalParams::getDefaultParams()) {}

Evaluator::Evaluator(const EvalParams& params) : params_(params) {}

FORCE_INLINE int Evaluator::getPieceSquareValue(
        const Piece piece, BoardPosition position, const Side side) const {
    return (int)params_.pieceSquareTables[(int)side][(int)piece][(int)position].early;
}

EvalCalcT Evaluator::evaluateRaw(const GameState& gameState) const {
    ParamGradient<false> gradient;
    const EvalCalcT rawEvalWhite = evaluateForWhite<false>(params_, gameState, gradient);

    return gameState.getSideToMove() == Side::White ? rawEvalWhite : -rawEvalWhite;
}

EvalWithGradient Evaluator::evaluateWithGradient(const GameState& gameState) const {
    ParamGradient<true> gradient = zeroGradient<true>();
    const EvalCalcT rawEvalWhite = evaluateForWhite<true>(params_, gameState, gradient);

    const EvalCalcT colorFactor = gameState.getSideToMove() == Side::White ? 1 : -1;

    return {.eval = colorFactor * rawEvalWhite, .gradient = colorFactor * gradient};
}

EvalT Evaluator::evaluate(const GameState& gameState) const {
    ParamGradient<false> gradient;
    const EvalCalcT rawEvalWhite = evaluateForWhite<false>(params_, gameState, gradient);

    const EvalT clampedEvalWhite =
            (EvalT)clamp((int)rawEvalWhite, -kMateEval + 1'000, kMateEval - 1'000);

    return gameState.getSideToMove() == Side::White ? clampedEvalWhite : -clampedEvalWhite;
}

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

FORCE_INLINE bool isInsufficientMaterial(const GameState& gameState) {
    const auto [whiteInsufficientMaterial, blackInsufficientMaterial] =
            insufficientMaterialForSides(gameState);

    return whiteInsufficientMaterial && blackInsufficientMaterial;
}

FORCE_INLINE EvalT evaluateNoLegalMoves(const GameState& gameState) {
    if (gameState.isInCheck()) {
        // We're in check and there are no legal moves so we're in checkmate.
        return -kMateEval;
    }

    // We're not in check and there are no legal moves so this is a stalemate.
    return 0;
}
