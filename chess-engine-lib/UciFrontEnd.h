#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Engine.h"
#include "FrontEndOption.h"
#include "GameState.h"
#include "SearchInfo.h"
#include "SearchStatistics.h"

#include <future>
#include <map>
#include <sstream>
#include <string>
#include <string_view>

class UciFrontEnd {
  public:
    UciFrontEnd();
    ~UciFrontEnd();

    // Run in a loop, handling UCI commands.
    void run();

    // Can be used by engine to report that a search to a certain depth has been fully completed.
    void reportFullSearch(
            const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const;

    // Can be used by engine to report that a search to a certain depth has been partially
    // completed. Normally this happens if the engine runs out of time while searching.
    void reportPartialSearch(
            const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const;

    // Can be used by engine to report statistics after ending the search.
    void reportSearchStatistics(const SearchStatistics& searchStatistics) const;

    // Can be used by the move searcher to report that we are re-searching with an updated
    // aspiration window.
    void reportAspirationWindowReSearch(
            EvalT previousLowerBound,
            EvalT previousUpperBound,
            EvalT searchEval,
            EvalT newLowerBound,
            EvalT newUpperBound) const;

    // Can be used by the move searcher to report that the PV from a (partial) search was discarded
    // for some reason.
    void reportDiscardedPv(std::string_view reason) const;

    void addOption(std::string name, FrontEndOption option);

  private:
    void handleIsReady();
    void handleNewGame();
    void handlePosition(std::stringstream& lineSStream);
    void handleGo(std::stringstream& lineSStream);
    void handleStop();
    void handleDebug(std::stringstream& lineSStream);
    void handleRegister() const;
    void handleSetOption(const std::string& line);

    void waitForGoToComplete();

    void writeOptions() const;

    Engine engine_;
    GameState gameState_;

    bool debugMode_         = false;
    bool printVisualString_ = false;

    std::map<std::string, FrontEndOption, std::less<>> optionsMap_;

    std::future<void> goFuture;
};
