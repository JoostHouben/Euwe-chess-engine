#pragma once

#include "EvalT.h"

#include "Macros.h"
#include "Math.h"
#include "MyAssert.h"

#include <limits>

#include <cstdint>

using EvalT = std::int16_t;

enum class ScoreType : std::uint8_t {
    NotSet     = 0,
    Exact      = 1,
    LowerBound = 2,
    UpperBound = 3,
    EGTB       = 4,
};

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();
inline constexpr EvalT kMateEval     = (EvalT)30'000;

[[nodiscard]] FORCE_INLINE constexpr bool isMate(const EvalT eval) {
    return abs(eval) > kMateEval - 1000;
}

[[nodiscard]] FORCE_INLINE constexpr bool isValid(const EvalT eval) {
    return -kMateEval <= eval && eval <= kMateEval;
}

[[nodiscard]] FORCE_INLINE constexpr int getMateDistanceInPly(const EvalT eval) {
    MY_ASSERT(isMate(eval));

    return kMateEval - abs(eval);
}

[[nodiscard]] FORCE_INLINE constexpr EvalT mateDistancePlus1(const EvalT eval) {
    MY_ASSERT(isMate(eval));

    return (EvalT)(eval - signum(eval));
}

[[nodiscard]] FORCE_INLINE constexpr EvalT mateDistanceMinus1(const EvalT eval) {
    MY_ASSERT(isMate(eval));

    return (EvalT)(eval + signum(eval));
}

[[nodiscard]] FORCE_INLINE constexpr EvalT mateIn(const int mateDistance) {
    MY_ASSERT(mateDistance >= 0);

    return (EvalT)(kMateEval - mateDistance);
}

[[nodiscard]] FORCE_INLINE constexpr EvalT clampNonMateEval(const int eval) {
    return (EvalT)clamp((int)eval, -kMateEval + 1'000, kMateEval - 1'000);
}
