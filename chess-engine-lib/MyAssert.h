#pragma once

#include <utility>

#include <cassert>

#ifdef _MSC_VER
#define ENSURE_ASSERT_BREAKS _set_error_mode(_OUT_TO_MSGBOX)
#else
#define ENSURE_ASSERT_BREAKS (void)0
#endif

#ifdef _MSC_VER
#define ASSUME(condition) __assume(condition)
#else
#define ASSUME(condition)            \
    do {                             \
        if (!(condition)) {          \
            __builtin_unreachable(); \
        }                            \
    } while (0)
#endif

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
        std::unreachable();                    \
    } while (0)
#else
#define UNREACHABLE std::unreachable()
#endif
