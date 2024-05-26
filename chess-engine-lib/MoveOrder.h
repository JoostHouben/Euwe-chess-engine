#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

using MoveEvalT = int;

[[nodiscard]] StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        const std::array<Move, 2>& killerMoves,
        const Move& counterMove,
        StackOfVectors<MoveEvalT>& stack);

// Variant for when we have no killer and counter moves available, like in quiescence search.
[[nodiscard]] StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        StackOfVectors<MoveEvalT>& stack);
