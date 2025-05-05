#pragma once

#include "chess-engine-lib/GameState.h"

#include <cstdint>

struct ScoredPosition {
    GameState gameState;
    std::uint64_t gameId{};
    int plyCount{};
    double finalScore{};
    int searchEvalCp{};
};
