#include "EvalT.h"

#include "Macros.h"
#include "Math.h"
#include "MyAssert.h"

#include <cmath>

FORCE_INLINE bool isMate(const EvalT eval) {
    return std::abs(eval) > kMateEval - 1000;
}

FORCE_INLINE bool isValid(const EvalT eval) {
    return -kMateEval <= eval && eval <= kMateEval;
}

FORCE_INLINE int getMateDistanceInPly(const EvalT eval) {
    MY_ASSERT(isMate(eval));

    return kMateEval - std::abs(eval);
}

FORCE_INLINE EvalT mateDistancePlus1(const EvalT eval) {
    MY_ASSERT(isMate(eval));

    return eval - signum(eval);
}
