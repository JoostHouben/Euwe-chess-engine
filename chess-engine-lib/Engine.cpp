#include "Engine.h"

#include "MoveSearcher.h"

class Engine::Impl {
  public:
    Impl();

    TimeManager& getTimeManager();

    void setFrontEnd(IFrontEnd* frontEnd);

    void newGame();

    [[nodiscard]] SearchInfo findMove(const GameState& gameState);

    void interruptSearch();

    void setTTableSize(int requestedSizeInMb);

  private:
    StackOfVectors<Move> moveStack_;
    TimeManager timeManager_;
    MoveSearcher moveSearcher_;
    IFrontEnd* frontEnd_ = nullptr;
};

Engine::Impl::Impl() : moveSearcher_(timeManager_) {
    moveStack_.reserve(1'000);
}

TimeManager& Engine::Impl::getTimeManager() {
    return timeManager_;
}

void Engine::Impl::setFrontEnd(IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;
    timeManager_.setFrontEnd(frontEnd);
    moveSearcher_.setFrontEnd(frontEnd);
}

void Engine::Impl::newGame() {
    moveSearcher_.newGame();
}

SearchInfo Engine::Impl::findMove(const GameState& gameState) {
    moveSearcher_.prepareForNewSearch(gameState);

    GameState copyState(gameState);

    moveSearcher_.resetSearchStatistics();

    std::optional<EvalT> evalGuess = std::nullopt;
    SearchInfo searchInfo;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth < MoveSearcher::kMaxDepth; ++depth) {
        const auto searchResult =
                moveSearcher_.searchForBestMove(copyState, depth, moveStack_, evalGuess);

        evalGuess = searchResult.eval;

        if (searchResult.principalVariation.size() > 0) {
            searchInfo.principalVariation = std::vector<Move>(
                    searchResult.principalVariation.begin(), searchResult.principalVariation.end());
        }

        const auto timeNow = std::chrono::high_resolution_clock::now();
        const auto millisecondsElapsed =
                std::chrono::duration_cast<std::chrono::milliseconds>(timeNow - startTime).count();

        const auto searchStatistics = moveSearcher_.getSearchStatistics();

        const std::uint64_t numNodes =
                searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched;
        const float nodesPerSecond = static_cast<float>(numNodes) / millisecondsElapsed * 1'000.0f;

        searchInfo.score          = searchResult.eval;
        searchInfo.depth          = depth;
        searchInfo.timeMs         = (int)millisecondsElapsed;
        searchInfo.numNodes       = numNodes;
        searchInfo.nodesPerSecond = nodesPerSecond;

        if (searchResult.wasInterrupted) {
            if (frontEnd_) {
                frontEnd_->reportPartialSearch(searchInfo, searchStatistics);
            }
            searchInfo.depth -= 1;
            break;
        }

        if (frontEnd_) {
            frontEnd_->reportFullSearch(searchInfo, searchStatistics);
        }

        if (isMate(searchResult.eval) && getMateDistanceInPly(searchResult.eval) <= depth) {
            break;
        }

        if (timeManager_.shouldStopAfterFullPly(depth)) {
            break;
        }
    }

    if (frontEnd_) {
        frontEnd_->reportSearchStatistics(moveSearcher_.getSearchStatistics());
    }

    return searchInfo;
}

void Engine::Impl::interruptSearch() {
    moveSearcher_.interruptSearch();
}

void Engine::Impl::setTTableSize(const int requestedSizeInMb) {
    moveSearcher_.setTTableSize(requestedSizeInMb);
}

// Implementation of interface: forward to implementation

Engine::Engine() : impl_(std::make_unique<Engine::Impl>()) {}

Engine::~Engine() = default;

TimeManager& Engine::getTimeManager() {
    return impl_->getTimeManager();
}

void Engine::setFrontEnd(IFrontEnd* frontEnd) {
    impl_->setFrontEnd(frontEnd);
}

void Engine::newGame() {
    impl_->newGame();
}

SearchInfo Engine::findMove(const GameState& gameState) {
    return impl_->findMove(gameState);
}

void Engine::interruptSearch() {
    impl_->interruptSearch();
}

void Engine::setTTableSize(const int requestedSizeInMb) {
    impl_->setTTableSize(requestedSizeInMb);
}
