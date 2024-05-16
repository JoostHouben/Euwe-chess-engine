#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

#include <cstdint>
#include <numeric>

using EvalT = std::int16_t;

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();
inline constexpr EvalT kMateEval     = (EvalT)30'000;

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

[[nodiscard]] EvalT evaluate(const GameState& gameState, StackOfVectors<Move>& stack);
