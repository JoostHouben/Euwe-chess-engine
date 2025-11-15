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

    frontEnd_->addOption(
            FrontEndOption::createInteger(
                    "move_overhead_ms", (int)moveOverhead_.count(), 0, 10'000, [this](int v) {
                        moveOverhead_ = std::chrono::milliseconds(v);
                    }));
}

bool TimeManager::shouldInterruptSearch(const std::uint64_t nodesSearched) const {
    MY_ASSERT(mode_ != TimeManagementMode::None);

    switch (mode_) {
        case TimeManagementMode::TimeControl: {
            return shouldInterrupt(interruptDeadLine_, interruptCheckCounter_);
        }

        case TimeManagementMode::Infinite: {
            return false;
        }

        case TimeManagementMode::FixedTime: {
            return shouldInterrupt(interruptDeadLine_, interruptCheckCounter_);
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
            return timeIsUp(plyDeadLine_);
        }

        case TimeManagementMode::Infinite: {
            return false;
        }

        case TimeManagementMode::FixedTime: {
            return timeIsUp(plyDeadLine_);
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

bool TimeManager::shouldStopAfterMateFound(int depth, int mateDistanceInPly) const {
    MY_ASSERT(mode_ != TimeManagementMode::None);

    if (mode_ == TimeManagementMode::TimeControl) {
        return 3 * mateDistanceInPly <= depth;
    }

    return false;
}

bool TimeManager::isInfiniteSearch() const {
    return mode_ == TimeManagementMode::Infinite;
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

    const bool isSuddenDeath = movesToGo > 40 && increment == std::chrono::milliseconds(0);

    const int expectedGameLength   = isSuddenDeath ? 60 : 40;
    const int minExpectedMovesLeft = isSuddenDeath ? 30 : 10;
    const int expectedMovesLeft =
            max(minExpectedMovesLeft, expectedGameLength - (int)gameState.getHalfMoveClock() / 2);
    const int expectedMovesToGo = min(expectedMovesLeft, movesToGo);

    // timeLeft includes the increment for the current move.
    const std::chrono::milliseconds totalTime = timeLeft + increment * (expectedMovesToGo - 1);

    std::chrono::milliseconds maxTime{};
    std::chrono::milliseconds timeTarget{};

    if (isSuddenDeath) {
        const std::chrono::milliseconds margin = std::max(
                totalTime - moveOverhead_ * expectedMovesToGo, std::chrono::milliseconds(0));
        maxTime    = margin / 3;
        timeTarget = margin / expectedMovesToGo;
    } else {
        maxTime    = timeLeft * 8 / 10 - moveOverhead_;
        timeTarget = totalTime / expectedMovesToGo - moveOverhead_;
    }

    const std::chrono::milliseconds extendedTimeBudget = std::min(maxTime, timeTarget * 3);
    const std::chrono::milliseconds hardTimeBudget     = std::min(maxTime, timeTarget * 4 / 3);
    const std::chrono::milliseconds softTimeBudget     = hardTimeBudget / 2;

    if (frontEnd_) {
        frontEnd_->reportDebugString(
                std::format(
                        "Time budget: soft {} ms / hard {} ms / extended {} ms",
                        softTimeBudget.count(),
                        hardTimeBudget.count(),
                        extendedTimeBudget.count()));
    }

    mode_              = TimeManagementMode::TimeControl;
    plyDeadLine_       = startTime_ + softTimeBudget;
    interruptDeadLine_ = startTime_ + hardTimeBudget;
    extendedDeadLine_  = startTime_ + extendedTimeBudget;
}

void TimeManager::configureForInfiniteSearch() {
    startNewSession();

    mode_ = TimeManagementMode::Infinite;
}

void TimeManager::configureForFixedTimeSearch(const std::chrono::milliseconds time) {
    startNewSession();

    // In fixed time mode we ignore moveOverhead_.
    mode_              = TimeManagementMode::FixedTime;
    plyDeadLine_       = startTime_ + time;
    interruptDeadLine_ = plyDeadLine_;
    extendedDeadLine_  = plyDeadLine_;
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

bool TimeManager::requestAdditionalTime() {
    if (mode_ != TimeManagementMode::TimeControl) {
        return false;
    }

    interruptDeadLine_ = extendedDeadLine_;

    interruptCheckCounter_ = kInterruptCheckInterval;
    return !timeIsUp(extendedDeadLine_);
}

std::chrono::milliseconds TimeManager::getTimeElapsed() const {
    const auto elapsed = std::chrono::high_resolution_clock::now() - startTime_;
    return std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);
}

void TimeManager::startNewSession() {
    startTime_             = std::chrono::high_resolution_clock::now();
    interruptCheckCounter_ = kInterruptCheckInterval;
}
