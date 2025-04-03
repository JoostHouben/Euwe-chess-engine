#include "PostProcessing.h"

#include "Utilities.h"

#include "chess-engine-lib/Eval.h"
#include "chess-engine-lib/PawnMasks.h"

namespace {

double calculateMaxPhaseMaterial(const EvalParams& evalParams) {
    return 2 * 8 * evalParams.phaseMaterialValues[(int)Piece::Pawn]
         + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Knight]
         + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Bishop]
         + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Rook]
         + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::Queen]
         + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::King];
}

double calculateGamePhase(
        const EvalParams& evalParams, const double maxPhaseMaterial, const GameState& gameState) {
    double phaseMaterial = 0.0;
    for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
        const Side side = (Side)sideIdx;
        for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
            const Piece piece            = (Piece)pieceIdx;
            const BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);
            const int numOfPiece         = popCount(pieceBitBoard);

            phaseMaterial += numOfPiece * evalParams.phaseMaterialValues[pieceIdx];
        }
    }
    return phaseMaterial / maxPhaseMaterial;
}

[[nodiscard]] bool isPassedPawn(
        const GameState& gameState, const Side side, const BoardPosition position) {
    const Side enemySide = nextSide(side);

    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(enemySide, Piece::Pawn);

    const BitBoard passedPawnOpponentMask = getPassedPawnOpponentMask(position, side);
    const BitBoard forwardMask            = getPawnForwardMask(position, side);

    const BitBoard opponentBlockers = enemyPawns & passedPawnOpponentMask;
    const BitBoard ownBlockers      = ownPawns & forwardMask;

    const bool isDoubledPawn = ownBlockers != BitBoard::Empty;
    const bool isPassedPawn  = !isDoubledPawn && opponentBlockers == BitBoard::Empty;

    return isPassedPawn && !isDoubledPawn;
}

void extractPieceSquareValues(
        const EvalParams& evalParams,
        const GameState& gameState,
        const double maxPhaseMaterial,
        std::array<double, kNumPieceTypes>& summedPieceSquareValuesEarly,
        std::array<double, kNumPieceTypes>& summedPieceSquareValuesLate,
        std::array<double, kNumPieceTypes>& numPieceOccurrencesEarly,
        std::array<double, kNumPieceTypes>& numPieceOccurrencesLate) {
    const double earlyFactor = calculateGamePhase(evalParams, maxPhaseMaterial, gameState);
    const double lateFactor  = 1.0 - earlyFactor;

    std::array<const PstMapping*, 2> normalMappingBySide{};
    std::array<const PstMapping*, 2> kingMappingBySide{};

    getPstMapping(
            gameState,
            normalMappingBySide[0],
            kingMappingBySide[0],
            normalMappingBySide[1],
            kingMappingBySide[1]);

    for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
        const Side side = (Side)sideIdx;
        for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
            const Piece piece      = (Piece)pieceIdx;
            BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);

            const PstMapping* mapping = pieceIdx == (int)Piece::King ? kingMappingBySide[sideIdx]
                                                                     : normalMappingBySide[sideIdx];

            while (pieceBitBoard != BitBoard::Empty) {
                const BoardPosition position = popFirstSetPosition(pieceBitBoard);

                if (piece == Piece::Pawn && isPassedPawn(gameState, side, position)) {
                    continue;
                }

                const int pstSqIdx = (*mapping)[(int)position];

                const EvalCalcT pieceSquareValueEarly =
                        evalParams.pieceSquareTables[pieceIdx][pstSqIdx].early;
                const EvalCalcT pieceSquareValueLate =
                        evalParams.pieceSquareTables[pieceIdx][pstSqIdx].late;

                summedPieceSquareValuesEarly[pieceIdx] += earlyFactor * pieceSquareValueEarly;
                numPieceOccurrencesEarly[pieceIdx] += earlyFactor;

                summedPieceSquareValuesLate[pieceIdx] += lateFactor * pieceSquareValueLate;
                numPieceOccurrencesLate[pieceIdx] += lateFactor;
            }
        }
    }
}

void setPieceValuesFromPieceSquare(
        EvalParams& evalParams, const std::vector<ScoredPosition>& scoredPositions) {
    // Calculate the piece values as the average piece square value across all positions.
    // These piece values can then be used for search heuristics. In particular, the difference
    // between piece square values and the piece values is a good representation of the 'goodnes' of
    // that square for that piece.
    // We calculate the average across all positions instead of a naive average across all squares,
    // since pieces are not uniformly distributed across the board over the course of a game.

    std::array<double, kNumPieceTypes> summedPieceSquareValuesEarly{};
    std::array<double, kNumPieceTypes> summedPieceSquareValuesLate{};
    std::array<double, kNumPieceTypes> numPieceOccurrencesEarly{};
    std::array<double, kNumPieceTypes> numPieceOccurrencesLate{};

    const double maxPhaseMaterial = calculateMaxPhaseMaterial(evalParams);

    for (const ScoredPosition& scoredPosition : scoredPositions) {
        const GameState& gameState = scoredPosition.gameState;

        extractPieceSquareValues(
                evalParams,
                gameState,
                maxPhaseMaterial,
                summedPieceSquareValuesEarly,
                summedPieceSquareValuesLate,
                numPieceOccurrencesEarly,
                numPieceOccurrencesLate);
    }

    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        double pieceSquareValueEarly;
        if (numPieceOccurrencesEarly[pieceIdx] == 0.0) {
            pieceSquareValueEarly = 0.0;
        } else {
            pieceSquareValueEarly =
                    summedPieceSquareValuesEarly[pieceIdx] / numPieceOccurrencesEarly[pieceIdx];
        }

        double pieceSquareValueLate;
        if (numPieceOccurrencesLate[pieceIdx] == 0.0) {
            pieceSquareValueLate = 0.0;
        } else {
            pieceSquareValueLate =
                    summedPieceSquareValuesLate[pieceIdx] / numPieceOccurrencesLate[pieceIdx];
        }

        evalParams.pieceValues[pieceIdx].early = (EvalCalcT)pieceSquareValueEarly;
        evalParams.pieceValues[pieceIdx].late  = (EvalCalcT)pieceSquareValueLate;
    }
}

void zeroOutKingValues(EvalParams& evalParams) {
    evalParams.pieceValues[(int)Piece::King].early = 0.0;
    evalParams.pieceValues[(int)Piece::King].late  = 0.0;
}

void zeroOutUnreachablePawnSquares(EvalParams& evalParams) {
    for (int file = 0; file < kFiles; ++file) {
        const int rank1Position = (int)positionFromFileRank(file, 0);
        const int rank7Position = (int)positionFromFileRank(file, kRanks - 2);
        const int rank8Position = (int)positionFromFileRank(file, kRanks - 1);

        evalParams.pieceSquareTables[(int)Piece::Pawn][rank1Position].early = 0.0;
        evalParams.pieceSquareTables[(int)Piece::Pawn][rank7Position].early = 0.0;
        evalParams.pieceSquareTables[(int)Piece::Pawn][rank8Position].early = 0.0;

        evalParams.pieceSquareTables[(int)Piece::Pawn][rank1Position].late = 0.0;
        evalParams.pieceSquareTables[(int)Piece::Pawn][rank7Position].late = 0.0;
        evalParams.pieceSquareTables[(int)Piece::Pawn][rank8Position].late = 0.0;

        evalParams.pieceSquareTables[EvalParams::kPassedPawnPstIdx][rank1Position].early = 0.0;
        evalParams.pieceSquareTables[EvalParams::kPassedPawnPstIdx][rank8Position].early = 0.0;

        evalParams.pieceSquareTables[EvalParams::kPassedPawnPstIdx][rank1Position].late = 0.0;
        evalParams.pieceSquareTables[EvalParams::kPassedPawnPstIdx][rank8Position].late = 0.0;
    }
}

}  // namespace

void postProcess(
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions) {
    EvalParams evalParams = evalParamsFromDoubles(paramsDouble);

    setPieceValuesFromPieceSquare(evalParams, scoredPositions);
    zeroOutKingValues(evalParams);
    zeroOutUnreachablePawnSquares(evalParams);

    paramsDouble = evalParamsToDoubles(evalParams);
}
