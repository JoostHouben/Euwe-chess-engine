#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

#include <numeric>
#include <optional>

#include <cstdint>

using EvalT     = std::int16_t;
using MoveEvalT = int;

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();
inline constexpr EvalT kMateEval     = (EvalT)30'000;

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

[[nodiscard]] EvalT evaluate(
        const GameState& gameState, StackOfVectors<Move>& stack, bool checkEndState = true);

[[nodiscard]] StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const GameState& gameState,
        StackOfVectors<MoveEvalT>& stack);

[[nodiscard]] bool isMate(EvalT eval);
[[nodiscard]] int getMateDistance(EvalT eval);
