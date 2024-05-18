#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Macros.h"

FORCE_INLINE constexpr int signum(const int x) {
    return (x > 0) - (x < 0);
}

template <typename T>
FORCE_INLINE constexpr T clamp(const T x, const T min, const T max) {
    return x < min ? min : x > max ? max : x;
}
