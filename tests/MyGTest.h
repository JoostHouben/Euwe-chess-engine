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
