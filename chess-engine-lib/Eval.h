#pragma once

#include "EvalT.h"
#include "GameState.h"

[[nodiscard]] int getPieceValue(Piece piece);

[[nodiscard]] int getPieceSquareValue(Piece piece, BoardPosition position, Side side);

[[nodiscard]] bool isInsufficientMaterial(const GameState& gameState);

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

[[nodiscard]] EvalT evaluate(
        const GameState& gameState, StackOfVectors<Move>& stack, bool checkEndState = true);
