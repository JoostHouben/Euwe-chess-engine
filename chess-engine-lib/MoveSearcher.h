#pragma once

#include "Eval.h"
#include "EvalT.h"
#include "GameState.h"
#include "IFrontEnd.h"
#include "SearchStatistics.h"
#include "TimeManager.h"

#include <memory>
#include <optional>
#include <vector>

struct RootSearchResult {
    std::vector<Move> principalVariation;
    EvalT eval;
    bool wasInterrupted = false;
};

struct RootNodeInfo {
    EvalT eval;
    int depth;
};

class MoveSearcher {
  public:
    static constexpr int kMaxDepth = 100;

    MoveSearcher(const TimeManager& timeManager, const Evaluator& evaluator);
    ~MoveSearcher();

    MoveSearcher(const MoveSearcher&)            = delete;
    MoveSearcher& operator=(const MoveSearcher&) = delete;

    MoveSearcher(MoveSearcher&&)            = default;
    MoveSearcher& operator=(MoveSearcher&&) = default;

    void setFrontEnd(IFrontEnd* frontEnd);

    void setSyzygyEnabled(bool enabled);

    void newGame();

    // Perform search and return the principal variation and evaluation.
    [[nodiscard]] RootSearchResult searchForBestMove(
            GameState& gameState,
            int depth,
            StackOfVectors<Move>& stack,
            std::optional<EvalT> evalGuess = std::nullopt);

    // Must be called before calling searchForBestMove from a new position or after
    // interruptSearch().
    void prepareForNewSearch(
            const GameState& gameState, const std::vector<Move>* movesToSearch, bool tbHitAtRoot);

    // Call this from a different thread to stop the search prematurely.
    void interruptSearch();

    // Get statistics since the last call to resetSearchStatistics.
    [[nodiscard]] SearchStatistics getSearchStatistics() const;

    // Reset the search statistics.
    void resetSearchStatistics();

    [[nodiscard]] int getDefaultTTableSizeInMb() const;

    void setTTableSize(int requestedSizeInMb);

    [[nodiscard]] std::optional<RootNodeInfo> getRootNodeInfo(const GameState& gameState) const;

  private:
    class Impl;

    std::unique_ptr<Impl> impl_;
};
