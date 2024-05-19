#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

#include <optional>

struct RootSearchResult {
    StackVector<Move> principalVariation;
    EvalT eval;
    bool wasInterrupted = false;
};

struct SearchStatistics {
    int normalNodesSearched = 0;
    int qNodesSearched      = 0;
    int tTableHits          = 0;
    float ttableUtilization = 0.0f;
};

[[nodiscard]] RootSearchResult searchForBestMove(
        GameState& gameState,
        int depth,
        StackOfVectors<Move>& stack,
        std::optional<EvalT> evalGuess = std::nullopt);

void prepareForSearch();
void requestSearchStop();

[[nodiscard]] SearchStatistics getSearchStatistics();
void resetSearchStatistics();
