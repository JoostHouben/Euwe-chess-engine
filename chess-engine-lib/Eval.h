#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

#include <array>
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
        const std::array<Move, 2>& killerMoves,
        const Move& counterMove,
        StackOfVectors<MoveEvalT>& stack);

// Variant for when we have no killer and counter moves available, like in quiescence search.
[[nodiscard]] StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const GameState& gameState,
        StackOfVectors<MoveEvalT>& stack);

[[nodiscard]] bool isMate(EvalT eval);
[[nodiscard]] int getMateDistanceInPly(EvalT eval);