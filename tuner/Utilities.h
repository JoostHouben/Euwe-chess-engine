#include "chess-engine-lib/EvalParams.h"

template <typename DoubleArrayT>
EvalParams evalParamsFromDoubles(const DoubleArrayT& doubles) {
    EvalParamArray paramsArray;
    for (int i = 0; i < paramsArray.size(); ++i) {
        paramsArray[i] = static_cast<EvalCalcT>(doubles[i]);
    }

    return evalParamsFromArray(paramsArray);
}
