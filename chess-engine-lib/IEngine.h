#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"
#include "SearchInfo.h"

#include <chrono>

class UciFrontEnd;

class IEngine {
  public:
    IEngine()          = default;
    virtual ~IEngine() = default;

    virtual void setUciFrontEnd(const UciFrontEnd* uciFrontEnd) = 0;

    virtual void newGame() = 0;

    [[nodiscard]] virtual SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget) = 0;

    virtual void interruptSearch() = 0;

    virtual void setTTableSize(int requestedSizeInMb) = 0;
};
