#pragma once

#ifndef NDEBUG
#define FORCE_INLINE
#else

#if defined(__clang__)
// [[clang::always_inline]] doesn't work on lambdas.
#define FORCE_INLINE __attribute__((always_inline))
#elif defined(_MSC_VER)
// NOTE: need to use attribute syntax; __forceinline doesn't work on lambdas.
// This might be non-standard behavior since attributes on lambdas are supposed to apply to the
// lambda type, not the function call operator.
#define FORCE_INLINE [[msvc::forceinline]]
#else
// GCC warns: ‘always_inline’ function might not be inlinable unless also declared ‘inline’ [-Wattributes]
// However, using the attribute still significantly improves performance.
// Making FORCE_INLINE imply 'inline' doesn't work because it's also used outside of headers.
#pragma GCC diagnostic ignored "-Wattributes"
// [[gnu::always_inline]] doesn't work on lambdas.
#define FORCE_INLINE __attribute__((always_inline))
#endif

#endif  // NDEBUG
