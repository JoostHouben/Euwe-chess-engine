#include "ClangDiagnosticIgnore.h"

#pragma once

#ifndef NDEBUG
#define FORCE_INLINE
#else

#ifdef _MSC_VER
#define FORCE_INLINE [[msvc::forceinline]]
#elif defined(__clang__)
#define FORCE_INLINE [[clang::always_inline]]
#else
#define FORCE_INLINE [[gnu::always_inline]]
#endif

#endif  // NDEBUG
