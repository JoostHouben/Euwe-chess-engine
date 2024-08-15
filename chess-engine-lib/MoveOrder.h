#pragma once

#include "GameState.h"

using MoveEvalT = int;

[[nodiscard]] StackVector<MoveEvalT> scoreMoves(
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        const std::array<Move, 2>& killerMoves,
        const Move& counterMove,
        const std::array<std ::array<unsigned, kSquares>, kNumPieceTypes>& historyCutOffs,
        const std::array<std ::array<unsigned, kSquares>, kNumPieceTypes>& historyUsed,
        StackOfVectors<MoveEvalT>& stack);

[[nodiscard]] StackVector<MoveEvalT> scoreMovesQuiesce(
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        StackOfVectors<MoveEvalT>& stack);
