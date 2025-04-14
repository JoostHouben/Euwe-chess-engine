#pragma once

#include "EvalT.h"
#include "GameState.h"
#include "IFrontEnd.h"
#include "SearchInfo.h"
#include "TimeManager.h"

#include <chrono>

class IEngine {
  public:
    IEngine()          = default;
    virtual ~IEngine() = default;

    virtual TimeManager& getTimeManager() = 0;

    virtual void setFrontEnd(IFrontEnd* frontEnd) = 0;

    virtual void newGame() = 0;

    [[nodiscard]] virtual SearchInfo findMove(
            const GameState& gameState, const std::vector<Move>& searchMoves) = 0;

    virtual void interruptSearch() = 0;

    [[nodiscard]] virtual int getDefaultTTableSizeInMb() const = 0;

    virtual void setTTableSize(int requestedSizeInMb) = 0;

    [[nodiscard]] virtual EvalT evaluate(const GameState& gameState) const = 0;

  protected:
    IEngine(const IEngine&)            = default;
    IEngine& operator=(const IEngine&) = default;
    IEngine(IEngine&&)                 = default;
    IEngine& operator=(IEngine&&)      = default;
};
