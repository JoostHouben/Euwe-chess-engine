#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

#include <cstdint>
#include <numeric>

using EvalT = std::int16_t;

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();

[[nodiscard]] EvalT evaluate(const GameState& gameState, StackOfVectors<Move>& stack);
