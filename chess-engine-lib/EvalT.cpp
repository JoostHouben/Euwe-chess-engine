#include "EvalT.h"

#include "Math.h"
#include "MyAssert.h"

#include <cmath>

bool isMate(const EvalT eval) {
    return std::abs(eval) > kMateEval - 1000;
}

bool isValid(const EvalT eval) {
    return -kMateEval <= eval && eval <= kMateEval;
}

int getMateDistanceInPly(const EvalT eval) {
    MY_ASSERT(isMate(eval));

    return kMateEval - std::abs(eval);
}

EvalT mateDistancePlus1(const EvalT eval) {
    MY_ASSERT(isMate(eval));

    return eval - signum(eval);
}
