#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Eval.h"
#include "GameState.h"

#include <chrono>
#include <memory>
#include <vector>

struct SearchInfo {
    std::vector<Move> principalVariation;
    EvalT score;
    int depth;
    int timeMs;
    int numNodes;
    int nodesPerSecond;
};

class EngineImpl;

class Engine {
  public:
    Engine();
    ~Engine();

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget);

  private:
    std::unique_ptr<EngineImpl> impl_;
};
