#include "ClangDiagnosticIgnore.h"

#pragma once

#include "EvalT.h"
#include "Move.h"

#include <vector>

#include <cstdint>

struct SearchInfo {
    std::vector<Move> principalVariation{};
    EvalT score{};
    int depth{};
    int timeMs{};
    std::uint64_t numNodes{};
    float nodesPerSecond{};
};
