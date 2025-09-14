#pragma once

#include "EvalT.h"
#include "Move.h"
#include "SearchStatistics.h"

#include <vector>

#include <cstdint>

struct SearchInfo {
    std::vector<Move> principalVariation{};
    EvalT score{};
    ScoreType scoreType = ScoreType::NotSet;
    int depth{};

    SearchStatistics statistics{};
};
