#include "chess-engine-lib/EvalParams.h"

#include <array>

template <typename DoubleArrayT>
EvalParams evalParamsFromDoubles(const DoubleArrayT& doubles) {
    EvalParamArray paramsArray;
    for (std::size_t i = 0; i < paramsArray.size(); ++i) {
        paramsArray[i] = static_cast<EvalCalcT>(doubles[i]);
    }

    return evalParamsFromArray(paramsArray);
}

inline std::array<double, kNumEvalParams> evalParamsToDoubles(const EvalParams& params) {
    std::array<double, kNumEvalParams> paramsDouble;
    const EvalParamArray paramsArray = evalParamsToArray(params);
    for (std::size_t i = 0; i < paramsDouble.size(); ++i) {
        paramsDouble[i] = static_cast<double>(paramsArray[i]);
    }

    return paramsDouble;
}
