#include "ClangDiagnosticIgnore.h"

#pragma once

#include <utility>

#include <cassert>
#include <cstdlib>

#ifdef _MSC_VER
#define ENSURE_ASSERT_BREAKS _set_error_mode(_OUT_TO_MSGBOX)
#else
#define ENSURE_ASSERT_BREAKS (void)0
#endif

#define ASSUME(condition)       \
    do {                        \
        if (!(condition)) {     \
            std::unreachable(); \
        }                       \
    } while (0)

#ifndef NDEBUG
#define MY_ASSERT(condition)  \
    do {                      \
        ENSURE_ASSERT_BREAKS; \
        assert(condition);    \
    } while (0)
#else
#define MY_ASSERT(condition) ASSUME(condition)
#endif

#ifndef NDEBUG
#define UNREACHABLE                            \
    do {                                       \
        constexpr bool IS_UNREACHABLE = false; \
        MY_ASSERT(IS_UNREACHABLE);             \
        std::abort();                          \
    } while (0)
#else
#define UNREACHABLE std::unreachable()
#endif
