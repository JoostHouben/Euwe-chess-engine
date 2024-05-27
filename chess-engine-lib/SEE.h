#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

// Let s be the real SEE value; let t be the threshold.
// If s >= t, returns a lower bound b such that s >= b >= t.
// If s < t, returns an upper bound b such that s <= b < t.
// Note that (s >= t) == (b >= t).
int staticExchangeEvaluationBound(const GameState& gameState, const Move& move, int threshold);
