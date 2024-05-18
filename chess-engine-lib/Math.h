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

// std::min and std::max don't seem to inline reliably in MSVC.
// Possibly because they're marked noexcept.
// Define our own versions to ensure inlining.

template <typename T>
FORCE_INLINE constexpr T min(const T& a, const T& b) {
    return a < b ? a : b;
}

template <typename T>
FORCE_INLINE constexpr T max(const T& a, const T& b) {
    return a > b ? a : b;
}
