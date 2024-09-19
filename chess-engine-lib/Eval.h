#pragma once

#include "EvalT.h"
#include "GameState.h"

[[nodiscard]] int getStaticPieceValue(Piece piece);

[[nodiscard]] int getPieceSquareValue(Piece piece, BoardPosition position, Side side);

[[nodiscard]] bool isInsufficientMaterial(const GameState& gameState);

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

[[nodiscard]] EvalT evaluate(const GameState& gameState);
