#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

#include <optional>

struct SearchResult {
    StackVector<Move> principalVariation;
    std::optional<EvalT> eval;
};

struct SearchStatistics {
    int normalNodesSearched = 0;
    int qNodesSearched      = 0;
    int tTableHits          = 0;
};

[[nodiscard]] SearchResult searchForBestMove(
        GameState& gameState, int depth, StackOfVectors<Move>& stack);

void resetStopSearchFlag();
void setStopSearchFlag();

[[nodiscard]] SearchStatistics getSearchStatistics();
void resetSearchStatistics();
