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

FORCE_INLINE void updatePiecePositionEvaluation(
        const EvalParams& params,
        const int pieceIdx,
        const BoardPosition position,
        const Side side,
        PiecePositionEvaluation& result) {

    result.material += params.pieceValues[pieceIdx];
    result.phaseMaterial += params.phaseMaterialValues[pieceIdx];

    int positionForPieceSquare = (int)position;
    if (side == Side::Black) {
        positionForPieceSquare = (int)getVerticalReflection(position);
    }

    result.earlyGamePosition +=
            params.pieceSquareTablesWhiteEarly[pieceIdx][positionForPieceSquare];
    result.endGamePosition += params.pieceSquareTablesWhiteLate[pieceIdx][positionForPieceSquare];
}

FORCE_INLINE void updateMobilityEvaluation(
        const EvalParams& params,
        const Piece piece,
        const BoardPosition position,
        const BitBoard anyPiece,
        const BitBoard ownOccupancy,
        PiecePositionEvaluation& result) {
    const BitBoard control = getPieceControlledSquares(piece, position, anyPiece);
    const int mobility     = popCount(control & ~ownOccupancy);
    result.earlyGamePosition += mobility * params.mobilityBonusEarly[(int)piece];
    result.endGamePosition += mobility * params.mobilityBonusLate[(int)piece];
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

[[nodiscard]] FORCE_INLINE PiecePositionEvaluation evaluatePiecePositionsForSide(
        const EvalParams& params, const GameState& gameState, const Side side) {
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
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation(params, (int)Piece::Knight, position, side, result);

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = max((EvalCalcT)0, 7 - kingDistance);
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            result.materialAdjustment += params.knightPawnAdjustment[numOwnPawns];

            updateMobilityEvaluation(
                    params, Piece::Knight, position, anyPiece, ownOccupancy, result);
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
            updatePiecePositionEvaluation(params, (int)Piece::Bishop, position, side, result);

            const int squareColor = getSquareColor(position);

            hasBishopOfColor[squareColor] = true;

            result.materialAdjustment -= params.badBishopPenalty[badBishopIndex[squareColor]];

            const EvalCalcT kingDistance = bishopDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = (14 - kingDistance) / 2;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            updateMobilityEvaluation(
                    params, Piece::Bishop, position, anyPiece, ownOccupancy, result);
        }

        if (hasBishopOfColor[0] && hasBishopOfColor[1]) {
            result.materialAdjustment += params.bishopPairBonus;
        }
    }

    // Rooks
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Rook);

        const int numRooks = popCount(pieceBitBoard);
        if (numRooks >= 2) {
            result.materialAdjustment -= params.rookPairPenalty;
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation(params, (int)Piece::Rook, position, side, result);

            const BitBoard fileBitBoard = getFileBitBoard(position);
            const bool blockedByOwnPawn = (ownPawns & fileBitBoard) != BitBoard::Empty;
            const bool blockedByAnyPawn = (anyPawn & fileBitBoard) != BitBoard::Empty;

            if (!blockedByAnyPawn) {
                result.earlyGamePosition += params.rookOpenFileBonus;
                result.endGamePosition += params.rookOpenFileBonus;
            } else if (!blockedByOwnPawn) {
                result.earlyGamePosition += params.rookSemiOpenFileBonus;
                result.endGamePosition += params.rookSemiOpenFileBonus;
            }

            const EvalCalcT kingDistance = manhattanDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = 14 - kingDistance;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            result.materialAdjustment += params.rookPawnAdjustment[numOwnPawns];

            updateMobilityEvaluation(params, Piece::Rook, position, anyPiece, ownOccupancy, result);
        }
    }

    // Queens
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Queen);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);
            updatePiecePositionEvaluation(params, (int)Piece::Queen, position, side, result);

            const EvalCalcT kingDistance = queenDistance(position, enemyKingPosition);
            const EvalCalcT tropismBonus = (7 - kingDistance) * 4;
            result.earlyGamePosition += tropismBonus;
            result.endGamePosition += tropismBonus;

            updateMobilityEvaluation(
                    params, Piece::Queen, position, anyPiece, ownOccupancy, result);
        }
    }

    // King
    {
        const BoardPosition kingPosition =
                getFirstSetPosition(gameState.getPieceBitBoard(side, Piece::King));
        updatePiecePositionEvaluation(params, (int)Piece::King, kingPosition, side, result);

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
evaluatePawnsForSide(const EvalParams& params, const GameState& gameState, const Side side) {
    // TODO: should we hash pawn structure and store pawn eval?

    PiecePositionEvaluation result{};

    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);

    BitBoard pawnBitBoard = ownPawns;

    while (pawnBitBoard != BitBoard::Empty) {
        const BoardPosition position = popFirstSetPosition(pawnBitBoard);

        updatePiecePositionEvaluation(params, (int)Piece::Pawn, position, side, result);

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
        } else if (isPassedPawn) {
            const int rank                = rankFromPosition(position);
            const int distanceToPromotion = side == Side::White ? kRanks - 1 - rank : rank;

            result.earlyGamePosition += params.passedPawnBonus[distanceToPromotion];
            result.endGamePosition += params.passedPawnBonus[distanceToPromotion];
        }

        if (isIsolated) {
            result.earlyGamePosition -= params.isolatedPawnPenalty;
            result.endGamePosition -= params.isolatedPawnPenalty;
        }
    }

    return result;
}

[[nodiscard]] FORCE_INLINE EvalCalcT
evaluateKingSafety(const EvalParams& params, const GameState& gameState, const Side side) {
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

    return -params.kingVirtualMobilityPenalty * virtualKingMobility;
}

[[nodiscard]] FORCE_INLINE EvalCalcT evaluateKingSwarming(
        const EvalParams& params,
        const GameState& gameState,
        const Side swarmingSide,
        const EvalCalcT swarmingMaterial,
        const EvalCalcT defendingMaterial,
        const float endGameFactor) {
    if (defendingMaterial >= swarmingMaterial) {
        return 0;
    }

    const EvalCalcT rookValue = params.pieceValues[(int)Piece::Rook];

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

[[nodiscard]] FORCE_INLINE EvalT evaluateForWhite(
        const EvalParams& params, const EvalCalcT maxPhaseMaterial, const GameState& gameState) {
    const auto whitePawnEval = evaluatePawnsForSide(params, gameState, Side::White);
    const auto blackPawnEval = evaluatePawnsForSide(params, gameState, Side::Black);

    const auto whitePiecePositionEval =
            evaluatePiecePositionsForSide(params, gameState, Side::White);
    const auto blackPiecePositionEval =
            evaluatePiecePositionsForSide(params, gameState, Side::Black);

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
            whitePiecePositionEval.phaseMaterial + whitePawnEval.phaseMaterial
            + blackPiecePositionEval.phaseMaterial + blackPawnEval.phaseMaterial;
    const float earlyGameFactor = (float)phaseMaterial / (float)maxPhaseMaterial;
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

    const EvalCalcT whiteKingSafety = evaluateKingSafety(params, gameState, Side::White);
    const EvalCalcT blackKingSafety = evaluateKingSafety(params, gameState, Side::Black);
    const EvalCalcT kingSafety = (EvalCalcT)((whiteKingSafety - blackKingSafety) * earlyGameFactor);

    const EvalCalcT whiteSwarmingValue = evaluateKingSwarming(
            params, gameState, Side::White, whiteMaterial, blackMaterial, endGameFactor);
    const EvalCalcT blackSwarmingValue = evaluateKingSwarming(
            params, gameState, Side::Black, blackMaterial, whiteMaterial, endGameFactor);
    const EvalCalcT swarmingEval = whiteSwarmingValue - blackSwarmingValue;

    const EvalCalcT eval = materialEval + positionEval + kingSafety + swarmingEval;

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

[[nodiscard]] FORCE_INLINE EvalT evaluateNoLegalMoves(const GameState& gameState) {
    if (gameState.isInCheck()) {
        // We're in check and there are no legal moves so we're in checkmate.
        return -kMateEval;
    }

    // We're not in check and there are no legal moves so this is a stalemate.
    return 0;
}

Evaluator::Evaluator() : Evaluator(EvalParams{}) {}

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
    const EvalCalcT rawEvalWhite = evaluateForWhite(params_, maxPhaseMaterial_, gameState);

    return gameState.getSideToMove() == Side::White ? rawEvalWhite : -rawEvalWhite;
}

EvalT Evaluator::evaluate(const GameState& gameState) const {
    const EvalT rawEvalWhite = evaluateForWhite(params_, maxPhaseMaterial_, gameState);

    const int roundedEvalWhite = (int)(rawEvalWhite + 0.5f);
    const EvalT clampedEvalWhite =
            (EvalT)clamp(roundedEvalWhite, -kMateEval + 1'000, kMateEval - 1'000);

    return gameState.getSideToMove() == Side::White ? clampedEvalWhite : -clampedEvalWhite;
}
