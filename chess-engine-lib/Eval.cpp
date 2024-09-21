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

struct PiecePositionEvaluation {
    EvalCalcT material           = 0;
    EvalCalcT materialAdjustment = 0;
    EvalCalcT phaseMaterial      = 0;
    EvalCalcT earlyGamePosition  = 0;
    EvalCalcT endGamePosition    = 0;
};

template <bool CalcJacobians>
struct PiecePositionEvaluationJacobians;

template <>
struct PiecePositionEvaluationJacobians<false> {};

template <>
struct PiecePositionEvaluationJacobians<true> {
    Eigen::VectorXf materialJacobians           = Eigen::VectorXf::Zero(kNumEvalParams);
    Eigen::VectorXf materialAdjustmentJacobians = Eigen::VectorXf::Zero(kNumEvalParams);
    Eigen::VectorXf phaseMaterialJacobians      = Eigen::VectorXf::Zero(kNumEvalParams);
    Eigen::VectorXf earlyGamePositionJacobians  = Eigen::VectorXf::Zero(kNumEvalParams);
    Eigen::VectorXf endGamePositionJacobians    = Eigen::VectorXf::Zero(kNumEvalParams);
};

template <bool CalcJacobians>
struct ParamGradient;

template <>
struct ParamGradient<false> {};

template <>
struct ParamGradient<true> {
    Eigen::VectorXf grad = Eigen::VectorXf::Zero(kNumEvalParams);
};

FORCE_INLINE std::size_t getParamIndex(const EvalParams& params, const EvalCalcT& param) {
    return (std::size_t)((std::byte*)&param - (std::byte*)&params) / sizeof(EvalCalcT);
}

template <bool CalcJacobians>
FORCE_INLINE void updatePiecePositionEvaluation(
        const EvalParams& params,
        const int pieceIdx,
        const BoardPosition position,
        const Side side,
        PiecePositionEvaluation& result,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {

    result.material += params.pieceValues[pieceIdx];
    result.phaseMaterial += params.phaseMaterialValues[pieceIdx];

    int positionForPieceSquare = (int)position;
    if (side == Side::Black) {
        positionForPieceSquare = (int)getVerticalReflection(position);
    }

    result.earlyGamePosition +=
            params.pieceSquareTablesWhiteEarly[pieceIdx][positionForPieceSquare];
    result.endGamePosition += params.pieceSquareTablesWhiteLate[pieceIdx][positionForPieceSquare];

    if constexpr (CalcJacobians) {
        jacobians.materialJacobians[getParamIndex(params, params.pieceValues[pieceIdx])] += 1;
        jacobians.phaseMaterialJacobians[getParamIndex(
                params, params.phaseMaterialValues[pieceIdx])] += 1;
        jacobians.earlyGamePositionJacobians[getParamIndex(
                params, params.pieceSquareTablesWhiteEarly[pieceIdx][positionForPieceSquare])] += 1;
        jacobians.endGamePositionJacobians[getParamIndex(
                params, params.pieceSquareTablesWhiteLate[pieceIdx][positionForPieceSquare])] += 1;
    }
}

template <bool CalcJacobians>
FORCE_INLINE void updateMobilityEvaluation(
        const EvalParams& params,
        const Piece piece,
        const BoardPosition position,
        const BitBoard anyPiece,
        const BitBoard ownOccupancy,
        PiecePositionEvaluation& result,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {
    const BitBoard control = getPieceControlledSquares(piece, position, anyPiece);
    const int mobility     = popCount(control & ~ownOccupancy);
    result.earlyGamePosition += mobility * params.mobilityBonusEarly[(int)piece];
    result.endGamePosition += mobility * params.mobilityBonusLate[(int)piece];

    if constexpr (CalcJacobians) {
        jacobians.earlyGamePositionJacobians[getParamIndex(
                params, params.mobilityBonusEarly[(int)piece])] += mobility;
        jacobians.endGamePositionJacobians[getParamIndex(
                params, params.mobilityBonusLate[(int)piece])] += mobility;
    }
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
[[nodiscard]] FORCE_INLINE PiecePositionEvaluation evaluatePiecePositionsForSide(
        const EvalParams& params,
        const GameState& gameState,
        const Side side,
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

    PiecePositionEvaluation result{};

    // Knights
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Knight);

        const int numKnights = popCount(pieceBitBoard);
        if (numKnights >= 2) {
            result.materialAdjustment -= params.knightPairPenalty;

            if constexpr (CalcJacobians) {
                jacobians.materialAdjustmentJacobians[getParamIndex(
                        params, params.knightPairPenalty)] -= 1;
            }
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Knight, position, side, result, jacobians);

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = max((EvalCalcT)0, 7 - kingDistance);
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            result.materialAdjustment += params.knightPawnAdjustment[numOwnPawns];

            if constexpr (CalcJacobians) {
                jacobians.materialAdjustmentJacobians[getParamIndex(
                        params, params.knightPawnAdjustment[numOwnPawns])] += 1;
            }

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

            result.materialAdjustment -= params.badBishopPenalty[badBishopIndex[squareColor]];

            if constexpr (CalcJacobians) {
                jacobians.materialAdjustmentJacobians[getParamIndex(
                        params, params.badBishopPenalty[badBishopIndex[squareColor]])] -= 1;
            }

            const EvalCalcT kingDistance = bishopDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = (14 - kingDistance) / 2;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            updateMobilityEvaluation<CalcJacobians>(
                    params, Piece::Bishop, position, anyPiece, ownOccupancy, result, jacobians);
        }

        if (hasBishopOfColor[0] && hasBishopOfColor[1]) {
            result.materialAdjustment += params.bishopPairBonus;

            if constexpr (CalcJacobians) {
                jacobians.materialAdjustmentJacobians[getParamIndex(
                        params, params.bishopPairBonus)] += 1;
            }
        }
    }

    // Rooks
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Rook);

        const int numRooks = popCount(pieceBitBoard);
        if (numRooks >= 2) {
            result.materialAdjustment -= params.rookPairPenalty;

            if constexpr (CalcJacobians) {
                jacobians.materialAdjustmentJacobians[getParamIndex(
                        params, params.rookPairPenalty)] -= 1;
            }
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Rook, position, side, result, jacobians);

            const BitBoard fileBitBoard = getFileBitBoard(position);
            const bool blockedByOwnPawn = (ownPawns & fileBitBoard) != BitBoard::Empty;
            const bool blockedByAnyPawn = (anyPawn & fileBitBoard) != BitBoard::Empty;

            if (!blockedByAnyPawn) {
                result.earlyGamePosition += params.rookOpenFileBonus;
                result.endGamePosition += params.rookOpenFileBonus;

                if constexpr (CalcJacobians) {
                    jacobians.earlyGamePositionJacobians[getParamIndex(
                            params, params.rookOpenFileBonus)] += 1;
                    jacobians.endGamePositionJacobians[getParamIndex(
                            params, params.rookOpenFileBonus)] += 1;
                }
            } else if (!blockedByOwnPawn) {
                result.earlyGamePosition += params.rookSemiOpenFileBonus;
                result.endGamePosition += params.rookSemiOpenFileBonus;

                if constexpr (CalcJacobians) {
                    jacobians.earlyGamePositionJacobians[getParamIndex(
                            params, params.rookSemiOpenFileBonus)] += 1;
                    jacobians.endGamePositionJacobians[getParamIndex(
                            params, params.rookSemiOpenFileBonus)] += 1;
                }
            }

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = 14 - kingDistance;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            result.materialAdjustment += params.rookPawnAdjustment[numOwnPawns];

            if constexpr (CalcJacobians) {
                jacobians.materialAdjustmentJacobians[getParamIndex(
                        params, params.rookPawnAdjustment[numOwnPawns])] += 1;
            }

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
            const EvalCalcT tropismBonus = (7 - kingDistance) * 4;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

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

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE PiecePositionEvaluation evaluatePawnsForSide(
        const EvalParams& params,
        const GameState& gameState,
        const Side side,
        PiecePositionEvaluationJacobians<CalcJacobians>& jacobians) {
    // TODO: should we hash pawn structure and store pawn eval?

    PiecePositionEvaluation result{};

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
            result.earlyGamePosition -= params.doubledPawnPenalty;
            result.endGamePosition -= params.doubledPawnPenalty;

            if constexpr (CalcJacobians) {
                jacobians.earlyGamePositionJacobians[getParamIndex(
                        params, params.doubledPawnPenalty)] -= 1;
                jacobians.endGamePositionJacobians[getParamIndex(
                        params, params.doubledPawnPenalty)] -= 1;
            }
        } else if (isPassedPawn) {
            const int rank                = rankFromPosition(position);
            const int distanceToPromotion = side == Side::White ? kRanks - 1 - rank : rank;

            result.earlyGamePosition += params.passedPawnBonus[distanceToPromotion];
            result.endGamePosition += params.passedPawnBonus[distanceToPromotion];

            if constexpr (CalcJacobians) {
                jacobians.earlyGamePositionJacobians[getParamIndex(
                        params, params.passedPawnBonus[distanceToPromotion])] += 1;
                jacobians.endGamePositionJacobians[getParamIndex(
                        params, params.passedPawnBonus[distanceToPromotion])] += 1;
            }
        }

        if (isIsolated) {
            result.earlyGamePosition -= params.isolatedPawnPenalty;
            result.endGamePosition -= params.isolatedPawnPenalty;

            if constexpr (CalcJacobians) {
                jacobians.earlyGamePositionJacobians[getParamIndex(
                        params, params.isolatedPawnPenalty)] -= 1;
                jacobians.endGamePositionJacobians[getParamIndex(
                        params, params.isolatedPawnPenalty)] -= 1;
            }
        }
    }

    return result;
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE EvalCalcT evaluateKingSafety(
        const EvalParams& params,
        const GameState& gameState,
        const Side side,
        ParamGradient<CalcJacobians>& gradient) {
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

    if constexpr (CalcJacobians) {
        gradient.grad[getParamIndex(params, params.kingVirtualMobilityPenalty)] +=
                -virtualKingMobility;
    }

    return -params.kingVirtualMobilityPenalty * virtualKingMobility;
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE EvalCalcT evaluateKingSwarming(
        const EvalParams& params,
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

    const EvalCalcT rookValue = params.pieceValues[(int)Piece::Rook];

    const float materialAdvantageFactor =
            min((float)(swarmingMaterial - defendingMaterial) / (float)rookValue, 1.f);

    ParamGradient<CalcJacobians> materialAdvantageFactorGradient;
    if constexpr (CalcJacobians) {
        if (materialAdvantageFactor < 1.f) {
            ParamGradient<CalcJacobians> numGradient;
            numGradient.grad = swarmingMaterialGradient.grad - defendingMaterialGradient.grad;
            ParamGradient<CalcJacobians> denGradient;
            denGradient.grad[getParamIndex(params, params.pieceValues[(int)Piece::Rook])] = 1;

            const EvalCalcT num = swarmingMaterial - defendingMaterial;
            const EvalCalcT den = rookValue;

            materialAdvantageFactorGradient.grad =
                    (numGradient.grad * den - num * denGradient.grad) / (den * den);
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
        gradient.grad += materialAdvantageFactorGradient.grad * swarmingValue * 10.f;
    }

    return (EvalCalcT)(materialAdvantageFactor * swarmingValue * 10.f);
}

[[nodiscard]] FORCE_INLINE ParamGradient<true> getMaxPhaseMaterialGradient(
        const EvalParams& params) {
    /*
    maxPhaseMaterial_ = 2 * 8 * params_.phaseMaterialValues[(int)Piece::Pawn]
                      + 2 * 2 * params_.phaseMaterialValues[(int)Piece::Knight]
                      + 2 * 2 * params_.phaseMaterialValues[(int)Piece::Bishop]
                      + 2 * 2 * params_.phaseMaterialValues[(int)Piece::Rook]
                      + 2 * 1 * params_.phaseMaterialValues[(int)Piece::Queen]
                      + 2 * 1 * params_.phaseMaterialValues[(int)Piece::King];
    */
    ParamGradient<true> gradient;
    gradient.grad[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Pawn])] += 2 * 8;
    gradient.grad[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Knight])] += 2 * 2;
    gradient.grad[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Bishop])] += 2 * 2;
    gradient.grad[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Rook])] += 2 * 2;
    gradient.grad[getParamIndex(params, params.phaseMaterialValues[(int)Piece::Queen])] += 2 * 1;
    gradient.grad[getParamIndex(params, params.phaseMaterialValues[(int)Piece::King])] += 2 * 1;
    return gradient;
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE EvalT evaluateForWhite(
        const EvalParams& params,
        const EvalCalcT maxPhaseMaterial,
        const GameState& gameState,
        ParamGradient<CalcJacobians>& whiteEvalGradient) {
    PiecePositionEvaluationJacobians<CalcJacobians> whitePiecePositionJacobians;
    PiecePositionEvaluationJacobians<CalcJacobians> blackPiecePositionJacobians;

    const auto whitePawnEval = evaluatePawnsForSide<CalcJacobians>(
            params, gameState, Side::White, whitePiecePositionJacobians);
    const auto blackPawnEval = evaluatePawnsForSide<CalcJacobians>(
            params, gameState, Side::Black, blackPiecePositionJacobians);

    const auto whitePiecePositionEval = evaluatePiecePositionsForSide<CalcJacobians>(
            params, gameState, Side::White, whitePiecePositionJacobians);
    const auto blackPiecePositionEval = evaluatePiecePositionsForSide<CalcJacobians>(
            params, gameState, Side::Black, blackPiecePositionJacobians);

    const EvalCalcT whiteMaterial = whitePiecePositionEval.material
                                  + whitePiecePositionEval.materialAdjustment
                                  + whitePawnEval.material + whitePawnEval.materialAdjustment;
    const EvalCalcT blackMaterial = blackPiecePositionEval.material
                                  + blackPiecePositionEval.materialAdjustment
                                  + blackPawnEval.material + blackPawnEval.materialAdjustment;

    EvalCalcT materialEval = whiteMaterial - blackMaterial;

    ParamGradient<CalcJacobians> whiteMaterialGradient;
    ParamGradient<CalcJacobians> blackMaterialGradient;
    ParamGradient<CalcJacobians> materialGradient;
    if constexpr (CalcJacobians) {
        whiteMaterialGradient.grad = whitePiecePositionJacobians.materialJacobians
                                   + whitePiecePositionJacobians.materialAdjustmentJacobians;
        blackMaterialGradient.grad = blackPiecePositionJacobians.materialJacobians
                                   + blackPiecePositionJacobians.materialAdjustmentJacobians;
        materialGradient.grad = whiteMaterialGradient.grad - blackMaterialGradient.grad;
    }

    if (isDrawish(
                gameState,
                whitePiecePositionEval.material + whitePawnEval.material,
                blackPiecePositionEval.material + blackPawnEval.material)) {
        materialEval /= 2;

        if constexpr (CalcJacobians) {
            materialGradient.grad /= 2;
        }
    }

    const EvalCalcT phaseMaterial =
            whitePiecePositionEval.phaseMaterial + whitePawnEval.phaseMaterial
            + blackPiecePositionEval.phaseMaterial + blackPawnEval.phaseMaterial;
    const float earlyGameFactor = (float)phaseMaterial / (float)maxPhaseMaterial;
    const float endGameFactor   = 1.f - earlyGameFactor;

    ParamGradient<CalcJacobians> earlyGameFactorGradient;
    if constexpr (CalcJacobians) {
        ParamGradient<CalcJacobians> phaseMaterialGradient;
        phaseMaterialGradient.grad = whitePiecePositionJacobians.phaseMaterialJacobians
                                   + blackPiecePositionJacobians.phaseMaterialJacobians;

        ParamGradient<CalcJacobians> maxPhaseMaterialGradient = getMaxPhaseMaterialGradient(params);

        earlyGameFactorGradient.grad = (phaseMaterialGradient.grad * maxPhaseMaterial
                                        - phaseMaterial * maxPhaseMaterialGradient.grad)
                                     / (maxPhaseMaterial * maxPhaseMaterial);
    }

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

    ParamGradient<CalcJacobians> positionGradient;
    if constexpr (CalcJacobians) {
        ParamGradient<CalcJacobians> earlyGamePositionGradient;
        earlyGamePositionGradient.grad = whitePiecePositionJacobians.earlyGamePositionJacobians
                                       - blackPiecePositionJacobians.earlyGamePositionJacobians;

        ParamGradient<CalcJacobians> endGamePositionGradient;
        endGamePositionGradient.grad = whitePiecePositionJacobians.endGamePositionJacobians
                                     - blackPiecePositionJacobians.endGamePositionJacobians;

        positionGradient.grad = earlyGamePositionGradient.grad * earlyGameFactor
                              + earlyGameFactorGradient.grad * earlyGamePositionEval
                              + endGamePositionGradient.grad * endGameFactor
                              - earlyGameFactorGradient.grad * endGamePositionEval;
    }

    ParamGradient<CalcJacobians> whiteKingSafetyGradient;
    ParamGradient<CalcJacobians> blackKingSafetyGradient;

    const EvalCalcT whiteKingSafety = evaluateKingSafety<CalcJacobians>(
            params, gameState, Side::White, whiteKingSafetyGradient);
    const EvalCalcT blackKingSafety = evaluateKingSafety<CalcJacobians>(
            params, gameState, Side::Black, blackKingSafetyGradient);
    const EvalCalcT kingSafety = (EvalCalcT)((whiteKingSafety - blackKingSafety) * earlyGameFactor);

    ParamGradient<CalcJacobians> kingSafetyGradient;
    if constexpr (CalcJacobians) {
        const EvalCalcT earlyGameKingSafety = whiteKingSafety - blackKingSafety;
        ParamGradient<CalcJacobians> earlyGameKingSafetyGradient;
        earlyGameKingSafetyGradient.grad =
                whiteKingSafetyGradient.grad - blackKingSafetyGradient.grad;

        kingSafetyGradient.grad = earlyGameKingSafetyGradient.grad * earlyGameFactor
                                + earlyGameKingSafety * earlyGameFactorGradient.grad;
    }

    ParamGradient<CalcJacobians> whiteSwarmingGradient;
    ParamGradient<CalcJacobians> blackSwarmingGradient;

    const EvalCalcT whiteSwarmingValue = evaluateKingSwarming<CalcJacobians>(
            params,
            gameState,
            Side::White,
            whiteMaterial,
            blackMaterial,
            whiteMaterialGradient,
            blackMaterialGradient,
            whiteSwarmingGradient);
    const EvalCalcT blackSwarmingValue = evaluateKingSwarming<CalcJacobians>(
            params,
            gameState,
            Side::Black,
            blackMaterial,
            whiteMaterial,
            blackMaterialGradient,
            whiteMaterialGradient,
            blackSwarmingGradient);
    const EvalCalcT swarmingEval = endGameFactor * (whiteSwarmingValue - blackSwarmingValue);

    ParamGradient<CalcJacobians> swarmingGradient;
    if constexpr (CalcJacobians) {
        const EvalCalcT endSwarmingValue = whiteSwarmingValue - blackSwarmingValue;
        ParamGradient<CalcJacobians> endSwarmingGradient;
        endSwarmingGradient.grad = whiteSwarmingGradient.grad - blackSwarmingGradient.grad;

        swarmingGradient.grad = endSwarmingGradient.grad * endGameFactor
                              - earlyGameFactorGradient.grad * endSwarmingValue;
    }

    const EvalCalcT eval = materialEval + positionEval + kingSafety + swarmingEval;

    if constexpr (CalcJacobians) {
        whiteEvalGradient.grad = materialGradient.grad + positionGradient.grad
                               + kingSafetyGradient.grad + swarmingGradient.grad;
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

Evaluator::Evaluator() : Evaluator(EvalParams::getDefaultParams()) {}

Evaluator::Evaluator(const EvalParams& params) : params_(params) {
    maxPhaseMaterial_ = 2 * 8 * params_.phaseMaterialValues[(int)Piece::Pawn]
                      + 2 * 2 * params_.phaseMaterialValues[(int)Piece::Knight]
                      + 2 * 2 * params_.phaseMaterialValues[(int)Piece::Bishop]
                      + 2 * 2 * params_.phaseMaterialValues[(int)Piece::Rook]
                      + 2 * 1 * params_.phaseMaterialValues[(int)Piece::Queen]
                      + 2 * 1 * params_.phaseMaterialValues[(int)Piece::King];
}

FORCE_INLINE int Evaluator::getPieceSquareValue(
        const Piece piece, BoardPosition position, const Side side) const {
    int positionForPieceSquare = (int)position;
    if (side == Side::Black) {
        positionForPieceSquare = (int)getVerticalReflection(position);
    }
    return (int)params_.pieceSquareTablesWhiteEarly[(int)piece][positionForPieceSquare];
}

EvalCalcT Evaluator::evaluateRaw(const GameState& gameState) const {
    ParamGradient<false> gradient;
    const EvalCalcT rawEvalWhite =
            evaluateForWhite(params_, maxPhaseMaterial_, gameState, gradient);

    return gameState.getSideToMove() == Side::White ? rawEvalWhite : -rawEvalWhite;
}

EvalWithGradient Evaluator::evaluateWithGradient(const GameState& gameState) const {
    ParamGradient<true> gradient;
    const EvalCalcT rawEvalWhite =
            evaluateForWhite(params_, maxPhaseMaterial_, gameState, gradient);

    const EvalCalcT colorFactor = gameState.getSideToMove() == Side::White ? 1 : -1;

    return {.eval = colorFactor * rawEvalWhite, .gradient = colorFactor * gradient.grad};
}

EvalT Evaluator::evaluate(const GameState& gameState) const {
    ParamGradient<false> gradient;
    const EvalT rawEvalWhite = evaluateForWhite(params_, maxPhaseMaterial_, gameState, gradient);

    const int roundedEvalWhite = (int)(rawEvalWhite + 0.5f);
    const EvalT clampedEvalWhite =
            (EvalT)clamp(roundedEvalWhite, -kMateEval + 1'000, kMateEval - 1'000);

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
