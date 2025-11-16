#pragma once

#include "FrontEndOption.h"
#include "SearchInfo.h"
#include "SearchStatistics.h"

#include <string>
#include <string_view>

class IFrontEnd {
  public:
    IFrontEnd()          = default;
    virtual ~IFrontEnd() = default;

    // Run in a loop, handling commands and sending them to the engine.
    virtual void run() = 0;

    // Programmatically add a command to be processed by the front end.
    // Programmatic commands are processed before any (further) user input is processed.
    // The command is pushed to the top of the command stack, so it will be processed next.
    // If you want to add multiple programmatic commands to be processed in order, you need to
    // push them in reverse order.
    virtual void pushProgrammaticCommand(std::string_view command) = 0;

    // Used by the engine to report to the front end that the search has started and that
    // further commands can now be processed.
    virtual void reportSearchHasStarted() = 0;

    // Can be used by the engine to report that a search to a certain depth has been fully
    // completed.
    virtual void reportFullSearch(const SearchInfo& searchInfo) const = 0;

    // Can be used by the engine to report that a search to a certain depth has been partially
    // completed. Normally this happens if the engine runs out of time while searching.
    virtual void reportPartialSearch(const SearchInfo& searchInfo) const = 0;

    // Can be used by the engine to report statistics after ending the search.
    virtual void reportSearchStatistics(const SearchStatistics& searchStatistics) const = 0;

    // Can be used by the move searcher to report that we are re-searching with an updated
    // aspiration window.
    virtual void reportAspirationWindowReSearch(
            const SearchInfo& searchInfo,
            EvalT previousLowerBound,
            EvalT previousUpperBound,
            EvalT newLowerBound,
            EvalT newUpperBound) const = 0;

    // Can be used by the move searcher to report that the PV from a (partial) search was discarded
    // for some reason.
    virtual void reportDiscardedPv(std::string_view reason) const = 0;

    virtual void reportError(std::string_view message) const = 0;

    virtual void reportString(std::string_view message) const = 0;

    virtual void reportDebugString(std::string_view message) const = 0;

    // Add option that can be configured through the frontend.
    virtual void addOption(FrontEndOption option) = 0;

  protected:
    IFrontEnd(const IFrontEnd&)            = default;
    IFrontEnd& operator=(const IFrontEnd&) = default;
    IFrontEnd(IFrontEnd&&)                 = default;
    IFrontEnd& operator=(IFrontEnd&&)      = default;
};
