#pragma once

#include "GameState.h"
#include "IEngine.h"
#include "IFrontEnd.h"
#include "SearchInfo.h"

#include <memory>

class Engine final : public IEngine {
  public:
    Engine();
    ~Engine() override;

    Engine(const Engine&)            = delete;
    Engine& operator=(const Engine&) = delete;

    Engine(Engine&&)            = default;
    Engine& operator=(Engine&&) = default;

    TimeManager& getTimeManager() override;

    void setFrontEnd(IFrontEnd* frontEnd) override;

    void newGame() override;

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, const std::vector<Move>& searchMoves) override;

    void interruptSearch() override;

    [[nodiscard]] int getDefaultTTableSizeInMb() const override;

    void setTTableSize(int requestedSizeInMb) override;

    [[nodiscard]] EvalT evaluate(const GameState& gameState) const override;

  private:
    class Impl;

    std::unique_ptr<Impl> impl_;
};
