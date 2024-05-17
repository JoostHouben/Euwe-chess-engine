#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

#include <vector>

struct SearchInfo {
    std::vector<Move> principalVariation;
    EvalT score;
    int depth;
    int timeMs;
    int numNodes;
    int nodesPerSecond;
};

[[nodiscard]] SearchInfo findMove(const GameState& gameState);
