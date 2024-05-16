#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

#include <numeric>
#include <optional>

#include <cstdint>

using EvalT = std::int16_t;

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();
inline constexpr EvalT kMateEval     = (EvalT)30'000;

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

[[nodiscard]] EvalT evaluate(const GameState& gameState, StackOfVectors<Move>& stack);

void selectBestMove(StackVector<Move>& moves, int firstMoveIdx, const GameState& gameState);
