#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Macros.h"

#include "immintrin.h"

#include <cstdint>

FORCE_INLINE inline std::uint64_t pdep(std::uint64_t src, std::uint64_t mask) {
    return _pdep_u64(src, mask);
}

FORCE_INLINE inline std::uint64_t pext(std::uint64_t src, std::uint64_t mask) {
    return _pext_u64(src, mask);
}

template <typename T>
FORCE_INLINE void prefetch(const T* ptr) {
    _mm_prefetch(reinterpret_cast<const char*>(ptr), _MM_HINT_T0);
}
