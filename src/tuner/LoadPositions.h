#pragma once

#include "ScoredPosition.h"

#include <filesystem>
#include <optional>
#include <ostream>
#include <vector>

std::vector<ScoredPosition> loadScoredPositions(
        std::vector<std::pair<std::filesystem::path, int>> pathsAndDropoutRates,
        int additionalDropoutRate = 1,
        std::ostream* logOutput   = nullptr);
