#pragma once

#include "ScoredPosition.h"

#include <string>
#include <vector>

void loadScoredPositions(
        const std::string& annotatedFensPath,
        int dropoutRate,
        std::vector<ScoredPosition>& scoredPositions);
