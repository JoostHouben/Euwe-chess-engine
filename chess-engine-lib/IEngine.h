#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"
#include "IFrontEnd.h"
#include "SearchInfo.h"

#include <chrono>

class IEngine {
  public:
    IEngine()          = default;
    virtual ~IEngine() = default;

    virtual void setFrontEnd(const IFrontEnd* frontEnd) = 0;

    virtual void newGame() = 0;

    [[nodiscard]] virtual SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget) = 0;

    virtual void interruptSearch() = 0;

    virtual void setTTableSize(int requestedSizeInMb) = 0;
};
