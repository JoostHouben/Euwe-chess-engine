#pragma once

#include "BoardConstants.h"

#include <array>
#include <string>

using EvalCalcT = float;

struct TaperedTerm {
    EvalCalcT early;
    EvalCalcT late;
};

using SquareTable       = std::array<TaperedTerm, kSquares>;
using PieceSquareTables = std::array<SquareTable, kNumPieceTypes>;

struct EvalParams {
    [[nodiscard]] static EvalParams getEmptyParams();
    [[nodiscard]] static EvalParams getDefaultParams();

    std::array<EvalCalcT, kNumPieceTypes> phaseMaterialValues;

    std::array<TaperedTerm, kNumPieceTypes> pieceValues;
    PieceSquareTables pieceSquareTablesWhite;

    std::array<TaperedTerm, 7> passedPawnBonus;
    TaperedTerm doubledPawnPenalty;
    TaperedTerm isolatedPawnPenalty;

    std::array<TaperedTerm, 9> bishopPawnSameColorBonus;

    TaperedTerm bishopPairBonus;
    TaperedTerm knightPairBonus;
    TaperedTerm rookPairBonus;

    TaperedTerm rookSemiOpenFileBonus;
    TaperedTerm rookOpenFileBonus;

    std::array<TaperedTerm, 9> knightPawnAdjustment;
    std::array<TaperedTerm, 9> rookPawnAdjustment;

    TaperedTerm kingVirtualMobilityPenalty;

    std::array<TaperedTerm, kNumPieceTypes> mobilityBonus;

    std::array<TaperedTerm, kNumPieceTypes> kingTropismBonus;

    TaperedTerm tempoBonus;

    std::array<std::array<TaperedTerm, 3>, kNumPieceTypes - 1> attackDefendAdjustment;

  private:
    EvalParams() = default;
};

static constexpr std::size_t kNumEvalParams = sizeof(EvalParams) / sizeof(EvalCalcT);

using EvalParamArray = std::array<EvalCalcT, kNumEvalParams>;
static_assert(sizeof(EvalParams) == sizeof(EvalParamArray));

[[nodiscard]] EvalParamArray evalParamsToArray(const EvalParams& params);

[[nodiscard]] EvalParams evalParamsFromArray(const EvalParamArray& array);

[[nodiscard]] std::string evalParamsToString(const EvalParams& params);
