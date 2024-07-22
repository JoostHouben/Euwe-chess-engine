#include "ClangDiagnosticIgnore.h"

#pragma once

struct SearchStatistics {
    int normalNodesSearched = 0;
    int qNodesSearched      = 0;
    int tTableHits          = 0;
    float ttableUtilization = 0.0f;
    int selectiveDepth      = 0;
};
