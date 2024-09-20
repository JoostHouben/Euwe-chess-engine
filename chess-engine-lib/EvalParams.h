#pragma once

#include "BoardConstants.h"

#include <array>

using EvalCalcT         = float;
using SquareTable       = std::array<EvalCalcT, kSquares>;
using PieceSquareTables = std::array<SquareTable, kNumPieceTypes>;

struct EvalParams {
    [[nodiscard]] static EvalParams getEmptyParams();
    [[nodiscard]] static EvalParams getDefaultParams();

    std::array<EvalCalcT, kNumPieceTypes> pieceValues;

    std::array<EvalCalcT, kNumPieceTypes> phaseMaterialValues;

    PieceSquareTables pieceSquareTablesWhiteEarly;

    PieceSquareTables pieceSquareTablesWhiteLate;

    std::array<EvalCalcT, 7> passedPawnBonus;
    EvalCalcT doubledPawnPenalty;
    EvalCalcT isolatedPawnPenalty;

    // Penalty for having 0...8 own pawns on the same color as a bishop
    std::array<EvalCalcT, 9> badBishopPenalty;

    EvalCalcT bishopPairBonus;
    EvalCalcT knightPairPenalty;
    EvalCalcT rookPairPenalty;

    EvalCalcT rookSemiOpenFileBonus;
    EvalCalcT rookOpenFileBonus;

    std::array<EvalCalcT, 9> knightPawnAdjustment;
    std::array<EvalCalcT, 9> rookPawnAdjustment;

    EvalCalcT kingVirtualMobilityPenalty;

    std::array<EvalCalcT, kNumPieceTypes> mobilityBonusEarly;

    std::array<EvalCalcT, kNumPieceTypes> mobilityBonusLate;

  private:
    EvalParams() = default;
};

using EvalParamArray = std::array<EvalCalcT, sizeof(EvalParams) / sizeof(EvalCalcT)>;
static_assert(sizeof(EvalParams) == sizeof(EvalParamArray));

[[nodiscard]] EvalParamArray evalParamsToArray(const EvalParams& params);

[[nodiscard]] EvalParams evalParamsFromArray(const EvalParamArray& array);
