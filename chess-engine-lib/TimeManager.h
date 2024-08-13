#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"
#include "IFrontEnd.h"

#include <chrono>

#include <cstdint>

class TimeManager {
  public:
    TimeManager();

    void setFrontEnd(IFrontEnd* frontEnd);

    [[nodiscard]] bool shouldInterruptSearch(std::uint64_t nodesSearched) const;

    [[nodiscard]] bool shouldStopAfterFullPly(int depth) const;

    void configureForTimeControl(
            std::chrono::milliseconds timeLeft,
            std::chrono::milliseconds increment,
            int movesToGo,
            const GameState& gameState);

    void configureForInfiniteSearch();

    void configureForFixedTimeSearch(std::chrono::milliseconds timeBudget);

    void configureForFixedDepthSearch(int depth);

    void configureForFixedNodesSearch(std::uint64_t nodes);

  private:
    enum class TimeManagementMode {
        None,
        TimeControl,
        Infinite,
        FixedTime,
        FixedDepth,
        FixedNodes
    };

    TimeManagementMode mode_ = TimeManagementMode::None;

    std::chrono::high_resolution_clock::time_point deadLine_{};
    int depthTarget_{};
    std::uint64_t nodesTarget_{};

    mutable int interruptCheckCounter_ = 0;

    std::chrono::milliseconds moveOverhead_;

    IFrontEnd* frontEnd_ = nullptr;
};
