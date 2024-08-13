#include "TimeManager.h"

#include "Math.h"

#include <format>

namespace {

[[nodiscard]] bool timeIsUp(const std::chrono::high_resolution_clock::time_point deadLine) {
    return std::chrono::high_resolution_clock::now() >= deadLine;
}

[[nodiscard]] bool shouldInterrupt(
        const std::chrono::high_resolution_clock::time_point deadLine, int& interruptCheckCounter) {
    static constexpr int interruptCheckInterval = 64;

    interruptCheckCounter = (interruptCheckCounter + 1) % interruptCheckInterval;
    if (interruptCheckCounter != 0) {
        return false;
    }

    return timeIsUp(deadLine);
}

}  // namespace

TimeManager::TimeManager() : moveOverhead_(std::chrono::milliseconds(100)) {}

void TimeManager::setFrontEnd(IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;

    frontEnd_->addOption(
            "move_overhead_ms",
            FrontEndOption::createInteger((int)moveOverhead_.count(), 0, 10'000, [this](int v) {
                moveOverhead_ = std::chrono::milliseconds(v);
            }));
}

bool TimeManager::shouldInterruptSearch(const std::uint64_t nodesSearched) const {
    MY_ASSERT(mode_ != TimeManagementMode::None);

    switch (mode_) {
        case TimeManagementMode::TimeControl: {
            return shouldInterrupt(deadLine_, interruptCheckCounter_);
        }

        case TimeManagementMode::Infinite: {
            return false;
        }

        case TimeManagementMode::FixedTime: {
            return shouldInterrupt(deadLine_, interruptCheckCounter_);
        }

        case TimeManagementMode::FixedDepth: {
            return false;
        }

        case TimeManagementMode::FixedNodes: {
            return nodesSearched >= nodesTarget_;
        }

        default: {
            UNREACHABLE;
        }
    }
}

bool TimeManager::shouldStopAfterFullPly(const int depth) const {
    MY_ASSERT(mode_ != TimeManagementMode::None);

    switch (mode_) {
        case TimeManagementMode::TimeControl: {
            return timeIsUp(deadLine_);
        }

        case TimeManagementMode::Infinite: {
            return false;
        }

        case TimeManagementMode::FixedTime: {
            return timeIsUp(deadLine_);
        }

        case TimeManagementMode::FixedDepth: {
            return depth >= depthTarget_;
        }

        case TimeManagementMode::FixedNodes: {
            return false;
        }

        default: {
            UNREACHABLE;
        }
    }
}

void TimeManager::configureForTimeControl(
        const std::chrono::milliseconds timeLeft,
        const std::chrono::milliseconds increment,
        const int movesToGo,
        const GameState& gameState) {
    const int expectedGameLength = 40;
    const int expectedMovesLeft =
            max(10, expectedGameLength - (int)gameState.getHalfMoveClock() / 2);
    const int expectedMovesToGo = min(expectedMovesLeft, movesToGo);

    // timeLeft includes the increment for the current move.
    const std::chrono::milliseconds totalTime   = timeLeft + increment * (expectedMovesToGo - 1);
    const std::chrono::milliseconds maxTime     = timeLeft * 8 / 10;
    const std::chrono::milliseconds timeForMove = std::min(maxTime, totalTime / expectedMovesToGo);

    const std::chrono::milliseconds timeBudget = timeForMove - moveOverhead_;

    if (frontEnd_) {
        frontEnd_->reportDebugString(std::format("Time budget: {} ms", timeBudget.count()));
    }

    mode_     = TimeManagementMode::TimeControl;
    deadLine_ = std::chrono::high_resolution_clock::now() + timeBudget;
}

void TimeManager::configureForInfiniteSearch() {
    mode_ = TimeManagementMode::Infinite;
}

void TimeManager::configureForFixedTimeSearch(const std::chrono::milliseconds time) {
    mode_     = TimeManagementMode::FixedTime;
    deadLine_ = std::chrono::high_resolution_clock::now() + time - moveOverhead_;
}

void TimeManager::configureForFixedDepthSearch(const int depth) {
    mode_        = TimeManagementMode::FixedDepth;
    depthTarget_ = depth;
}

void TimeManager::configureForFixedNodesSearch(const std::uint64_t nodes) {
    mode_        = TimeManagementMode::FixedNodes;
    nodesTarget_ = nodes;
}
