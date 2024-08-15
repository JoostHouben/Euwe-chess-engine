#pragma once

#ifndef NDEBUG
#define FORCE_INLINE
#else

#if defined(__clang__)
// [[clang::always_inline]] doesn't work on lambdas.
#define FORCE_INLINE __attribute__((always_inline))
#elif defined(_MSC_VER)
// NOTE: need to use attribute syntax; __forceinline doesn't work on lambdas.
// This might be non-standard behavior since attributes on lambdas are supposed to apply top the
// lambda type, not the function call operator.
#define FORCE_INLINE [[msvc::forceinline]]
#else
// [[gnu::always_inline]] doesn't work on lambdas.
#define FORCE_INLINE __attribute__((always_inline))
#endif

#endif  // NDEBUG
