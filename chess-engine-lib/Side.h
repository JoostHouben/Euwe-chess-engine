#include "ClangDiagnosticIgnore.h"

#pragma once

#include "MyAssert.h"

#include <cstdint>

enum class Side : std::uint8_t { White, Black };

[[nodiscard]] constexpr Side nextSide(Side side) {
    switch (side) {
        case Side::White:
            return Side::Black;
        case Side::Black:
            return Side::White;
    }
    UNREACHABLE;
}