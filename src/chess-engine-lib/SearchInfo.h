#pragma once

#include "EvalT.h"
#include "Move.h"
#include "SearchStatistics.h"

#include <vector>

#include <cstdint>

struct SearchInfo {
    std::vector<Move> principalVariation{};
    EvalT score{};
    int depth{};

    SearchStatistics statistics{};
};
