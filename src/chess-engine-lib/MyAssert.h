#pragma once

#include <stdexcept>
#include <utility>

#include <cassert>
#include <cstdlib>

#ifdef _MSC_VER
#define ENSURE_ASSERT_BREAKS _set_error_mode(_OUT_TO_MSGBOX)
#else
#define ENSURE_ASSERT_BREAKS (void)0
#endif

// NOLINTNEXTLINE(clang-analyzer-core.NullDereference)
#define FAIL_IN_CONSTEVAL *static_cast<int*>(nullptr) = 0

#define ASSUME(condition)           \
    do {                            \
        if consteval {              \
            if (!(condition)) {     \
                FAIL_IN_CONSTEVAL;  \
            }                       \
        } else {                    \
            if (!(condition)) {     \
                std::unreachable(); \
            }                       \
        }                           \
    } while (0)

#ifndef NDEBUG
#define MY_ASSERT(condition)       \
    do {                           \
        if consteval {             \
            if (!(condition)) {    \
                FAIL_IN_CONSTEVAL; \
            }                      \
        } else {                   \
            ENSURE_ASSERT_BREAKS;  \
            assert(condition);     \
        }                          \
        if (!(condition)) {        \
            std::abort();          \
        }                          \
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

#ifndef NDEBUG
#define MY_ASSERT_DEBUG(condition) MY_ASSERT(condition)
#else
#define MY_ASSERT_DEBUG(condition) (void)0
#endif
