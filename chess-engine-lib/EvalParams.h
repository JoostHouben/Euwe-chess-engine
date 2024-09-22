#pragma once

#include "BoardConstants.h"

#include <array>
#include <string>

using EvalCalcT         = float;
using SquareTable       = std::array<EvalCalcT, kSquares>;
using PieceSquareTables = std::array<SquareTable, kNumPieceTypes>;

struct EvalParams {
    [[nodiscard]] static EvalParams getEmptyParams();
    [[nodiscard]] static EvalParams getDefaultParams();

    std::array<EvalCalcT, kNumPieceTypes> pieceValuesEarly;
    std::array<EvalCalcT, kNumPieceTypes> pieceValuesLate;

    std::array<EvalCalcT, kNumPieceTypes> phaseMaterialValues;

    PieceSquareTables pieceSquareTablesWhiteEarly;
    PieceSquareTables pieceSquareTablesWhiteLate;

    std::array<EvalCalcT, 7> passedPawnBonusEarly;
    std::array<EvalCalcT, 7> passedPawnBonusLate;

    EvalCalcT doubledPawnPenaltyEarly;
    EvalCalcT doubledPawnPenaltyLate;

    EvalCalcT isolatedPawnPenaltyEarly;
    EvalCalcT isolatedPawnPenaltyLate;

    std::array<EvalCalcT, 9> bishopPawnSameColorBonusEarly;
    std::array<EvalCalcT, 9> bishopPawnSameColorBonusLate;

    EvalCalcT bishopPairBonusEarly;
    EvalCalcT bishopPairBonusLate;

    EvalCalcT knightPairBonusEarly;
    EvalCalcT knightPairBonusLate;

    EvalCalcT rookPairBonusEarly;
    EvalCalcT rookPairBonusLate;

    EvalCalcT rookSemiOpenFileBonusEarly;
    EvalCalcT rookSemiOpenFileBonusLate;

    EvalCalcT rookOpenFileBonusEarly;
    EvalCalcT rookOpenFileBonusLate;

    std::array<EvalCalcT, 9> knightPawnAdjustmentEarly;
    std::array<EvalCalcT, 9> knightPawnAdjustmentLate;

    std::array<EvalCalcT, 9> rookPawnAdjustmentEarly;
    std::array<EvalCalcT, 9> rookPawnAdjustmentLate;

    EvalCalcT kingVirtualMobilityPenaltyEarly;
    EvalCalcT kingVirtualMobilityPenaltyLate;

    std::array<EvalCalcT, kNumPieceTypes> mobilityBonusEarly;
    std::array<EvalCalcT, kNumPieceTypes> mobilityBonusLate;

    std::array<EvalCalcT, kNumPieceTypes> kingTropismBonusEarly;
    std::array<EvalCalcT, kNumPieceTypes> kingTropismBonusLate;

  private:
    EvalParams() = default;
};

static constexpr std::size_t kNumEvalParams = sizeof(EvalParams) / sizeof(EvalCalcT);

using EvalParamArray = std::array<EvalCalcT, kNumEvalParams>;
static_assert(sizeof(EvalParams) == sizeof(EvalParamArray));

[[nodiscard]] EvalParamArray evalParamsToArray(const EvalParams& params);

[[nodiscard]] EvalParams evalParamsFromArray(const EvalParamArray& array);

[[nodiscard]] std::string evalParamsToString(const EvalParams& params);
