#pragma once

#include "Eval.h"
#include "EvalT.h"
#include "GameState.h"
#include "IFrontEnd.h"
#include "SearchStatistics.h"
#include "TimeManager.h"

#include <memory>
#include <optional>

struct RootSearchResult {
    StackVector<Move> principalVariation;
    EvalT eval;
    bool wasInterrupted = false;
};

class MoveSearcher {
  public:
    static constexpr int kMaxDepth = 100;

    MoveSearcher(const TimeManager& timeManager, const Evaluator& evaluator);
    ~MoveSearcher();

    void setFrontEnd(IFrontEnd* frontEnd);

    void newGame();

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
    [[nodiscard]] SearchStatistics getSearchStatistics() const;

    // Reset the search statistics.
    void resetSearchStatistics();

    [[nodiscard]] int getDefaultTTableSizeInMb() const;

    void setTTableSize(int requestedSizeInMb);

  private:
    class Impl;

    std::unique_ptr<Impl> impl_;
};
