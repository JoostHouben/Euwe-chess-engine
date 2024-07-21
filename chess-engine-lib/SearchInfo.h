#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "Move.h"

#include <vector>

struct SearchInfo {
    std::vector<Move> principalVariation;
    EvalT score;
    int depth;
    int timeMs;
    int numNodes;
    int nodesPerSecond;
};
