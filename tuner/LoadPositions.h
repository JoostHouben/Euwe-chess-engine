#pragma once

#include "ScoredPosition.h"

#include <filesystem>
#include <vector>

void loadScoredPositions(
        const std::filesystem::path& annotatedFensPath,
        int dropoutRate,
        std::vector<ScoredPosition>& scoredPositions);
