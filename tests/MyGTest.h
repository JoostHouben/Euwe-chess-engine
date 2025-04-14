#pragma once

#ifdef _MSC_VER
#pragma warning(push)
// uninitialized variables inside gtest code
#pragma warning(disable : 26495)
#endif

#include "gtest/gtest.h"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "chess-engine-lib/MyAssert.h"

// ASSERT_TRUE(result.has_value()) should be enough to prevent the test
// code that follows it to run, but clang-tidy doesn't seem to recognize
// that macro expansion as protective. We add UNREACHABLE to fix this.
#define ENFORCE_TRUE(condition)     \
    do {                            \
        if (!(condition)) {         \
            ASSERT_TRUE(condition); \
            UNREACHABLE;            \
        }                           \
    } while (0)
