#pragma once

#include "EvalParams.h"
#include "EvalT.h"
#include "GameState.h"

[[nodiscard]] int getStaticPieceValue(Piece piece);

[[nodiscard]] bool isInsufficientMaterial(const GameState& gameState);

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

class Evaluator {
  public:
    Evaluator();
    Evaluator(const EvalParams& params);

    [[nodiscard]] int getPieceSquareValue(Piece piece, BoardPosition position, Side side) const;

    [[nodiscard]] EvalCalcT evaluateRaw(const GameState& gameState) const;

    [[nodiscard]] EvalT evaluate(const GameState& gameState) const;

  private:
    EvalParams params_;
    EvalCalcT maxPhaseMaterial_{};
};
