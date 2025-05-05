#pragma once

#include "ScoredPosition.h"

#include <vector>

std::vector<ScoredPosition> quiescePositions(
        const std::vector<AnnotatedPosition>& annotatedPositions);
