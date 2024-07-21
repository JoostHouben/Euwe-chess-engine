#include "EvalT.h"

#include "MyAssert.h"

#include <numeric>

bool isMate(const EvalT eval) {
    return std::abs(eval) >= kMateEval - 1000;
}

bool isValid(const EvalT eval) {
    return -kMateEval <= eval && eval <= kMateEval;
}

int getMateDistanceInPly(EvalT eval) {
    MY_ASSERT(isMate(eval));

    return kMateEval - std::abs(eval);
}
