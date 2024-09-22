#pragma once

#include "chess-engine-lib/GameState.h"

struct ScoredPosition {
    GameState gameState;
    double score;
};
