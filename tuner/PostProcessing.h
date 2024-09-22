#pragma once

#include "ScoredPosition.h"

#include "chess-engine-lib/EvalParams.h"

#include <array>
#include <vector>

void postProcess(
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions);
