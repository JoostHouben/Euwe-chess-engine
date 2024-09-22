#include "chess-engine-lib/EvalParams.h"

#include <array>

template <typename DoubleArrayT>
EvalParams evalParamsFromDoubles(const DoubleArrayT& doubles) {
    EvalParamArray paramsArray;
    for (int i = 0; i < paramsArray.size(); ++i) {
        paramsArray[i] = static_cast<EvalCalcT>(doubles[i]);
    }

    return evalParamsFromArray(paramsArray);
}

inline std::array<double, kNumEvalParams> evalParamsToDoubles(const EvalParams& params) {
    std::array<double, kNumEvalParams> paramsDouble;
    const EvalParamArray paramsArray = evalParamsToArray(params);
    for (int i = 0; i < paramsDouble.size(); ++i) {
        paramsDouble[i] = static_cast<double>(paramsArray[i]);
    }

    return paramsDouble;
}
