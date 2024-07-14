#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

#include <memory>
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

class MoveSearcherImpl;

class MoveSearcher {
  public:
    MoveSearcher();
    ~MoveSearcher();

    // Perform search and return the principal variation and evaluation.
    [[nodiscard]] RootSearchResult searchForBestMove(
            GameState& gameState,
            int depth,
            StackOfVectors<Move>& stack,
            std::optional<EvalT> evalGuess = std::nullopt);

    // Must be called before the first invocation of searchForBestMove.
    void initializeSearch();

    // Must be called before each invocation of searchForBestMove.
    void prepareForSearch(const GameState& gameState);

    // Call this from a different thread to stop the search prematurely.
    void requestSearchStop();

    // Get statistics since the last call to getSearchStatistics.
    [[nodiscard]] SearchStatistics getSearchStatistics();

    // Reset the search statistics.
    void resetSearchStatistics();

  private:
    std::unique_ptr<MoveSearcherImpl> impl_;
};
