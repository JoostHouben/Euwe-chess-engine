#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"
#include "SearchStatistics.h"

#include <memory>
#include <optional>

struct RootSearchResult {
    StackVector<Move> principalVariation;
    EvalT eval;
    bool wasInterrupted = false;
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

    // Must be called before calling searchForBestMove from a new position or after
    // interruptSearch().
    void prepareForNewSearch(const GameState& gameState);

    // Call this from a different thread to stop the search prematurely.
    void interruptSearch();

    // Get statistics since the last call to resetSearchStatistics.
    [[nodiscard]] SearchStatistics getSearchStatistics();

    // Reset the search statistics.
    void resetSearchStatistics();

  private:
    std::unique_ptr<MoveSearcherImpl> impl_;
};
