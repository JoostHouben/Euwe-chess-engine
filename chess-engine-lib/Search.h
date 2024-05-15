#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

struct SearchResult {
    Move bestMove;
    EvalT eval;
};

[[nodiscard]] SearchResult searchForBestMove(
        GameState& gameState, int depth, StackOfVectors<Move>& stack);
