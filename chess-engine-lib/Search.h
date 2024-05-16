#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

struct SearchResult {
    Move bestMove;
    EvalT eval;
};

struct SearchStatistics {
    int nodesSearched = 0;
};

[[nodiscard]] SearchResult searchForBestMove(
        GameState& gameState, int depth, StackOfVectors<Move>& stack);

void stopSearch();

[[nodiscard]] SearchStatistics getSearchStatistics();
void resetSearchStatistics();
