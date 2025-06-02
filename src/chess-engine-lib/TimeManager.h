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

    [[nodiscard]] bool shouldStopAfterFullPly(int depth, int numMovesToConsider) const;

    [[nodiscard]] bool shouldStopAfterMateFound(int depth, int mateDistanceInPly) const;

    [[nodiscard]] bool isInfiniteSearch() const;

    void forceNextCheck() const;

    void configureForTimeControl(
            std::chrono::milliseconds timeLeft,
            std::chrono::milliseconds increment,
            int movesToGo,
            const GameState& gameState);

    void configureForInfiniteSearch();

    void configureForFixedTimeSearch(std::chrono::milliseconds timeBudget);

    void configureForFixedDepthSearch(int depth);

    void configureForFixedNodesSearch(std::uint64_t nodes);

    [[nodiscard]] std::chrono::milliseconds getTimeElapsed() const;

  private:
    enum class TimeManagementMode {
        None,
        TimeControl,
        Infinite,
        FixedTime,
        FixedDepth,
        FixedNodes
    };

    void startNewSession();

    TimeManagementMode mode_ = TimeManagementMode::None;

    std::chrono::high_resolution_clock::time_point startTime_{};

    std::chrono::high_resolution_clock::time_point softDeadLine_{};
    std::chrono::high_resolution_clock::time_point hardDeadLine_{};
    int depthTarget_{};
    std::uint64_t nodesTarget_{};

    mutable int interruptCheckCounter_ = 0;

    std::chrono::milliseconds moveOverhead_;

    IFrontEnd* frontEnd_ = nullptr;
};
