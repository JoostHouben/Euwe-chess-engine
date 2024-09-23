#include "PostProcessing.h"

#include "Utilities.h"

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

    for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
        const Side side = (Side)sideIdx;
        for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
            const Piece piece      = (Piece)pieceIdx;
            BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);

            while (pieceBitBoard != BitBoard::Empty) {
                BoardPosition position = popFirstSetPosition(pieceBitBoard);

                if (side == Side::Black) {
                    position = getVerticalReflection(position);
                }

                const EvalCalcT pieceSquareValueEarly =
                        evalParams.pieceSquareTablesWhite[pieceIdx][(int)position].early;
                const EvalCalcT pieceSquareValueLate =
                        evalParams.pieceSquareTablesWhite[pieceIdx][(int)position].late;

                summedPieceSquareValuesEarly[pieceIdx] += earlyFactor * pieceSquareValueEarly;
                numPieceOccurrencesEarly[pieceIdx] += earlyFactor;

                summedPieceSquareValuesLate[pieceIdx] += lateFactor * pieceSquareValueLate;
                numPieceOccurrencesLate[pieceIdx] += lateFactor;
            }
        }
    }
}

void updatePieceValuesFromPieceSquare(
        EvalParams& evalParams, const std::vector<ScoredPosition>& scoredPositions) {
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

        evalParams.pieceValues[pieceIdx].early += pieceSquareValueEarly;
        evalParams.pieceValues[pieceIdx].late += pieceSquareValueLate;

        for (int squareIdx = 0; squareIdx < kSquares; ++squareIdx) {
            evalParams.pieceSquareTablesWhite[pieceIdx][squareIdx].early -= pieceSquareValueEarly;
            evalParams.pieceSquareTablesWhite[pieceIdx][squareIdx].late -= pieceSquareValueLate;
        }
    }
}

void zeroOutKingValues(EvalParams& evalParams) {
    evalParams.pieceValues[(int)Piece::King].early = 0.0;
    evalParams.pieceValues[(int)Piece::King].late  = 0.0;
}

void zeroOutUnreachablePawnSquares(EvalParams& evalParams) {
    for (int file = 0; file < kFiles; ++file) {
        const int rank1Position = (int)positionFromFileRank(file, 0);
        const int rank8Position = (int)positionFromFileRank(file, kRanks - 1);

        evalParams.pieceSquareTablesWhite[(int)Piece::Pawn][rank1Position].early = 0.0;
        evalParams.pieceSquareTablesWhite[(int)Piece::Pawn][rank8Position].early = 0.0;

        evalParams.pieceSquareTablesWhite[(int)Piece::Pawn][rank1Position].late = 0.0;
        evalParams.pieceSquareTablesWhite[(int)Piece::Pawn][rank8Position].late = 0.0;
    }
}

template <typename T>
void interlaceValues(T& valuesEarly, T& valuesLate) {
    constexpr std::size_t kNumElements = sizeof(T) / sizeof(EvalCalcT);
    std::array<EvalCalcT, kNumElements * 2> valuesInterlaced;
    for (std::size_t i = 0; i < kNumElements; ++i) {
        valuesInterlaced[2 * i]     = (reinterpret_cast<EvalCalcT*>(&valuesEarly))[i];
        valuesInterlaced[2 * i + 1] = (reinterpret_cast<EvalCalcT*>(&valuesLate))[i];
    }

    std::memcpy(&valuesEarly, valuesInterlaced.data(), sizeof(valuesInterlaced));
}

}  // namespace

void postProcess(
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions) {
    EvalParams evalParams = evalParamsFromDoubles(paramsDouble);

    updatePieceValuesFromPieceSquare(evalParams, scoredPositions);
    zeroOutKingValues(evalParams);
    zeroOutUnreachablePawnSquares(evalParams);

    paramsDouble = evalParamsToDoubles(evalParams);
}
