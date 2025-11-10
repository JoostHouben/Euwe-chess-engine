#pragma once

#include "chess-engine-lib/GameState.h"

#include <cstdint>

struct AnnotatedPosition {
    GameState gameState;
    std::uint64_t gameId{};
    int plyCount{};
    double finalScore{};
    int searchEvalCp{};
    bool moveIsCapture{};
};

struct ScoredPosition {
    GameState gameState;
    double score{};
};
