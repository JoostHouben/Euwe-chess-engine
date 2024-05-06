#include "ClangDiagnosticIgnore.h"

#pragma once

#include "immintrin.h"

#include <cstdint>

inline std::uint64_t pdep(std::uint64_t src, std::uint64_t mask) {
    return _pdep_u64(src, mask);
}

inline std::uint64_t pext(std::uint64_t src, std::uint64_t mask) {
    return _pext_u64(src, mask);
}
