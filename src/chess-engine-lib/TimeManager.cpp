#include "TimeManager.h"

#include "Math.h"

#include <format>

namespace {

constexpr int kInterruptCheckInterval = 32;

[[nodiscard]] bool timeIsUp(const std::chrono::high_resolution_clock::time_point deadLine) {
    return std::chrono::high_resolution_clock::now() >= deadLine;
}

[[nodiscard]] bool shouldInterrupt(
        const std::chrono::high_resolution_clock::time_point deadLine, int& interruptCheckCounter) {
    if (interruptCheckCounter > 0) {
        --interruptCheckCounter;
        return false;
    }

    interruptCheckCounter = kInterruptCheckInterval;

    return timeIsUp(deadLine);
}

}  // namespace

TimeManager::TimeManager() : moveOverhead_(std::chrono::milliseconds(20)) {}

void TimeManager::setFrontEnd(IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;

    frontEnd_->addOption(FrontEndOption::createInteger(
            "move_overhead_ms", (int)moveOverhead_.count(), 0, 10'000, [this](int v) {
                moveOverhead_ = std::chrono::milliseconds(v);
            }));
}

bool TimeManager::shouldInterruptSearch(const std::uint64_t nodesSearched) const {
    MY_ASSERT(mode_ != TimeManagementMode::None);

    switch (mode_) {
        case TimeManagementMode::TimeControl: {
            return shouldInterrupt(hardDeadLine_, interruptCheckCounter_);
        }

        case TimeManagementMode::Infinite: {
            return false;
        }

        case TimeManagementMode::FixedTime: {
            return shouldInterrupt(hardDeadLine_, interruptCheckCounter_);
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

bool TimeManager::shouldStopAfterFullPly(const int depth, const int numMovesToConsider) const {
    MY_ASSERT(mode_ != TimeManagementMode::None);

    switch (mode_) {
        case TimeManagementMode::TimeControl: {
            if (numMovesToConsider == 1) {
                return depth >= 2;
            }
            return timeIsUp(softDeadLine_);
        }

        case TimeManagementMode::Infinite: {
            return false;
        }

        case TimeManagementMode::FixedTime: {
            return timeIsUp(softDeadLine_);
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

void TimeManager::forceNextCheck() const {
    interruptCheckCounter_ = 0;
}

void TimeManager::configureForTimeControl(
        const std::chrono::milliseconds timeLeft,
        const std::chrono::milliseconds increment,
        const int movesToGo,
        const GameState& gameState) {
    startNewSession();

    const int expectedGameLength = 40;
    const int expectedMovesLeft =
            max(10, expectedGameLength - (int)gameState.getHalfMoveClock() / 2);
    const int expectedMovesToGo = min(expectedMovesLeft, movesToGo);

    // timeLeft includes the increment for the current move.
    const std::chrono::milliseconds totalTime  = timeLeft + increment * (expectedMovesToGo - 1);
    const std::chrono::milliseconds maxTime    = timeLeft * 8 / 10 - moveOverhead_;
    const std::chrono::milliseconds timeTarget = totalTime / expectedMovesToGo - moveOverhead_;

    const std::chrono::milliseconds hardTimeBudget = std::min(maxTime, timeTarget * 4 / 3);
    const std::chrono::milliseconds softTimeBudget = hardTimeBudget / 2;

    if (frontEnd_) {
        frontEnd_->reportDebugString(std::format(
                "Time budget: soft {} ms / hard {} ms",
                softTimeBudget.count(),
                hardTimeBudget.count()));
    }

    mode_         = TimeManagementMode::TimeControl;
    softDeadLine_ = startTime_ + softTimeBudget;
    hardDeadLine_ = startTime_ + hardTimeBudget;
}

void TimeManager::configureForInfiniteSearch() {
    startNewSession();

    mode_ = TimeManagementMode::Infinite;
}

void TimeManager::configureForFixedTimeSearch(const std::chrono::milliseconds time) {
    startNewSession();

    mode_         = TimeManagementMode::FixedTime;
    softDeadLine_ = startTime_ + time - moveOverhead_;
    hardDeadLine_ = softDeadLine_;
}

void TimeManager::configureForFixedDepthSearch(const int depth) {
    startNewSession();

    mode_        = TimeManagementMode::FixedDepth;
    depthTarget_ = depth;
}

void TimeManager::configureForFixedNodesSearch(const std::uint64_t nodes) {
    startNewSession();

    mode_        = TimeManagementMode::FixedNodes;
    nodesTarget_ = nodes;
}

std::chrono::milliseconds TimeManager::getTimeElapsed() const {
    const auto elapsed = std::chrono::high_resolution_clock::now() - startTime_;
    return std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);
}

void TimeManager::startNewSession() {
    startTime_             = std::chrono::high_resolution_clock::now();
    interruptCheckCounter_ = kInterruptCheckInterval;
}
