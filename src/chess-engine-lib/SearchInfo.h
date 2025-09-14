#pragma once

#include "EvalT.h"
#include "Move.h"
#include "SearchStatistics.h"

#include <vector>

#include <cstdint>

struct SearchResult {
    std::vector<Move> principalVariation;
    EvalT eval;
    ScoreType scoreType;
    bool wasInterrupted = false;
};

struct SearchInfo {
    SearchResult result{};

    int depth{};

    SearchStatistics statistics{};
};
