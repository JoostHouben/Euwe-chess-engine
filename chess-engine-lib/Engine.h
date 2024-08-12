#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"
#include "IEngine.h"
#include "IFrontEnd.h"
#include "SearchInfo.h"

#include <memory>

class Engine final : public IEngine {
  public:
    Engine();
    ~Engine();

    void setFrontEnd(const IFrontEnd* frontEnd) override;

    void newGame() override;

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget) override;

    void interruptSearch() override;

    void setTTableSize(int requestedSizeInMb) override;

  private:
    class Impl;

    std::unique_ptr<Impl> impl_;
};
