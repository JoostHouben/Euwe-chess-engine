#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

// Returns exact SEE value.
[[nodiscard]] int staticExchangeEvaluation(const GameState& gameState, const Move& move);

// Let s be the exact SEE value; let t be the threshold. The function returns a bound b such that:
// If s >= t, then b is a lower bound such that s >= b >= t.
// If s < t, then b is an upper bound such that s <= b < t.
// Note that (s >= t) == (b >= t).
// This can be used to check if the SEE value meets a certain threshold (for making pruning
// decisions), while also providing a bound that can be used for a fail-soft return value.
// At the same time, the function can still return early if the SEE value is already known to be
// on one side of the threshold.
[[nodiscard]] int staticExchangeEvaluationBound(
        const GameState& gameState, const Move& move, int threshold);

// Equivalent to: staticExchangeEvaluation(gameState, move) >= 0
// But can return even earlier than staticExchangeEvaluationBound.
[[nodiscard]] bool staticExchangeEvaluationNonLosing(const GameState& gameState, const Move& move);
