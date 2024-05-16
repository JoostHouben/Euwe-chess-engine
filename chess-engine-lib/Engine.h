#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

struct SearchInfo {
    Move bestMove;
    EvalT score;
    int depth;
    int timeMs;
    int numNodes;
    int nodesPerSecond;
};

[[nodiscard]] SearchInfo findMove(const GameState& gameState);
