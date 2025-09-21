#pragma once

#include <limits>

#include <cstdint>

using EvalT = std::int16_t;

enum ScoreType : std::uint8_t {
    NotSet     = 0,
    Exact      = 1,
    LowerBound = 2,
    UpperBound = 3,
    EGTB       = 4,
};

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();
inline constexpr EvalT kMateEval     = (EvalT)30'000;

[[nodiscard]] bool isMate(EvalT eval);
[[nodiscard]] bool isValid(EvalT eval);

[[nodiscard]] int getMateDistanceInPly(EvalT eval);

[[nodiscard]] EvalT mateDistancePlus1(EvalT eval);

[[nodiscard]] EvalT mateDistanceMinus1(EvalT eval);

[[nodiscard]] EvalT mateIn(int mateDistance);

[[nodiscard]] EvalT clampNonMateEval(int eval);
