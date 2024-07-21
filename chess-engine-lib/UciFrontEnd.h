#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Engine.h"
#include "GameState.h"
#include "SearchInfo.h"
#include "SearchStatistics.h"

#include <sstream>

class UciFrontEnd {
  public:
    UciFrontEnd();

    // Run in a loop, handling UCI commands.
    void run();

    // Can be used by engine to report that a search to a certain depth has been fully completed.
    void reportFullSearch(const SearchInfo& searchInfo) const;

    // Can be used by engine to report that a search to a certain depth has been partially
    // completed. Normally this happens if the engine runs out of time while searching.
    void reportPartialSearch(const SearchInfo& searchInfo) const;

    void reportSearchStatistics(const SearchStatistics& searchStatistics) const;

  private:
    void handleIsReady() const;
    void handlePosition(std::stringstream& lineSStream);
    void handleGo(std::stringstream& lineSStream);

    Engine engine_;
    GameState gameState_;
};
