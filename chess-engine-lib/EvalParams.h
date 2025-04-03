#pragma once

#include "BoardConstants.h"
#include "Macros.h"
#include "MyAssert.h"

#include <array>
#include <string>

#include <cstddef>

using EvalCalcT = float;

struct TaperedTerm {
    EvalCalcT early;
    EvalCalcT late;
};

struct EvalParams {
    [[nodiscard]] static EvalParams getEmptyParams();
    [[nodiscard]] static EvalParams getDefaultParams();

    static constexpr std::size_t kPassedPawnPstIdx = kNumPieceTypes;
    static constexpr std::size_t kNumPstPieceTypes = kNumPieceTypes + 1;

    std::array<EvalCalcT, kNumPstPieceTypes> phaseMaterialValues;

    std::array<EvalCalcT, 5> oppositeColoredBishopFactor;
    EvalCalcT singleMinorFactor;
    EvalCalcT twoKnightsFactor;
    EvalCalcT rookVsMinorFactor;
    EvalCalcT rookAndMinorVsRookFactor;

    EvalCalcT hasUnstoppablePawn;

    // From here on out, every term is a TaperedTerm.

    // Note: piece values are unused by eval; only used for heuristics in search.
    std::array<TaperedTerm, kNumPieceTypes> pieceValues;

    using SquareTable       = std::array<TaperedTerm, kSquares>;
    using PieceSquareTables = std::array<SquareTable, kNumPstPieceTypes>;

    PieceSquareTables pieceSquareTables;

    TaperedTerm connectedPassedPawnBonus;
    TaperedTerm doubledPawnPenalty;
    TaperedTerm isolatedPawnPenalty;

    std::array<TaperedTerm, 9> bishopPawnSameColorAdjustment;
    std::array<TaperedTerm, 9> bishopEnemyPawnSameColorAdjustment;

    TaperedTerm bishopPairBonus;
    TaperedTerm knightPairBonus;
    TaperedTerm rookPairBonus;

    TaperedTerm rookSemiOpenFileBonus;
    TaperedTerm rookOpenFileBonus;

    std::array<TaperedTerm, 9> knightPawnAdjustment;
    std::array<TaperedTerm, 9> rookPawnAdjustment;
    std::array<TaperedTerm, 9> queenPawnAdjustment;

    TaperedTerm kingVirtualMobilityPenalty;

    std::array<TaperedTerm, kNumPieceTypes> mobilityBonus;

    static constexpr int kDoubledPawnTropismIdx        = kNumPieceTypes - 1;
    static constexpr int kIsolatedPawnTropismIdx       = kNumPieceTypes;
    static constexpr int kPassedPawnTropismIdx         = kNumPieceTypes + 1;
    static constexpr std::size_t kNumTropismPieceTypes = kNumPieceTypes + 2;

    std::array<TaperedTerm, kNumTropismPieceTypes> ownKingTropism;
    std::array<TaperedTerm, kNumTropismPieceTypes> enemyKingTropism;

    TaperedTerm tempoBonus;

    std::array<std::array<TaperedTerm, 3>, kNumPieceTypes - 1> attackDefendAdjustment;

    std::array<TaperedTerm, 10> controlNearEnemyKing;
    std::array<TaperedTerm, kNumPieceTypes> kingAttackWeight;
    std::array<TaperedTerm, 6> numKingAttackersAdjustment;

    std::array<TaperedTerm, kNumPieceTypes - 1> piecePinnedAdjustment;

    TaperedTerm kingOpenFileAdjustment;
    TaperedTerm kingFlankOpenFileAdjustment;

    TaperedTerm passedPawnOutsideKingSquare;

    [[nodiscard]] FORCE_INLINE std::size_t getParamIndex(const EvalCalcT& param) const {
        const std::byte* thisByte   = (const std::byte*)this;
        const std::byte* paramByte  = (const std::byte*)&param;
        const std::ptrdiff_t offset = paramByte - thisByte;

        MY_ASSERT(offset >= 0 && (std::size_t)offset < sizeof(*this));
        MY_ASSERT(offset % sizeof(EvalCalcT) == 0);

        return (std::size_t)(offset / sizeof(EvalCalcT));
    }

    [[nodiscard]] std::size_t getFirstTaperedTermIndex() const {
        return getParamIndex(pieceValues[0].early);
    }

  private:
    EvalParams() = default;
};

static constexpr std::size_t kNumEvalParams = sizeof(EvalParams) / sizeof(EvalCalcT);

using EvalParamArray = std::array<EvalCalcT, kNumEvalParams>;
static_assert(sizeof(EvalParams) == sizeof(EvalParamArray));

[[nodiscard]] EvalParamArray evalParamsToArray(const EvalParams& params);

[[nodiscard]] EvalParams evalParamsFromArray(const EvalParamArray& array);

[[nodiscard]] std::string evalParamsToString(const EvalParams& params);
