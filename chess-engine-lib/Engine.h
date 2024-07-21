#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"
#include "SearchInfo.h"

#include <chrono>
#include <memory>
#include <vector>

class EngineImpl;
class UciFrontEnd;

class Engine {
  public:
    explicit Engine(const UciFrontEnd* uciFrontEnd = nullptr);
    ~Engine();

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget);

  private:
    std::unique_ptr<EngineImpl> impl_;
};
