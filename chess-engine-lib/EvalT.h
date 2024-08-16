#pragma once

#include <limits>

#include <cstdint>

using EvalT = std::int16_t;

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();
inline constexpr EvalT kMateEval     = (EvalT)30'000;

[[nodiscard]] bool isMate(EvalT eval);
[[nodiscard]] bool isValid(EvalT eval);

[[nodiscard]] int getMateDistanceInPly(EvalT eval);

[[nodiscard]] EvalT mateDistancePlus1(EvalT eval);
