#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Macros.h"

FORCE_INLINE constexpr int signum(const int x) {
    return (x > 0) - (x < 0);
}
