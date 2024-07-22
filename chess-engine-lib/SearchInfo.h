#include "ClangDiagnosticIgnore.h"

#pragma once

#include "EvalT.h"
#include "Move.h"

#include <vector>

struct SearchInfo {
    std::vector<Move> principalVariation;
    EvalT score;
    int depth;
    int selectiveDepth;
    int timeMs;
    int numNodes;
    int nodesPerSecond;
};
