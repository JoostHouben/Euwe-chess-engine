#include "Engine.h"

#include "MoveSearcher.h"

#include <future>

class Engine::Impl {
  public:
    Impl();

    void setFrontEnd(const IFrontEnd* frontEnd);

    void newGame();

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget);

    void interruptSearch();

    void setTTableSize(int requestedSizeInMb);

  private:
    [[nodiscard]] SearchInfo findMoveWorker(const GameState& gameState);

    StackOfVectors<Move> moveStack_;
    MoveSearcher moveSearcher_;
    const IFrontEnd* frontEnd_;
};

Engine::Impl::Impl() {
    moveStack_.reserve(1'000);
}

void Engine::Impl::setFrontEnd(const IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;
    moveSearcher_.setFrontEnd(frontEnd);
}

void Engine::Impl::newGame() {
    moveSearcher_.newGame();
}

SearchInfo Engine::Impl::findMoveWorker(const GameState& gameState) {
    GameState copySate(gameState);

    moveSearcher_.resetSearchStatistics();

    std::optional<EvalT> evalGuess = std::nullopt;
    SearchInfo searchInfo;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth < 40; ++depth) {
        const auto searchResult =
                moveSearcher_.searchForBestMove(copySate, depth, moveStack_, evalGuess);

        evalGuess = searchResult.eval;

        if (searchResult.principalVariation.size() > 0) {
            searchInfo.principalVariation = std::vector<Move>(
                    searchResult.principalVariation.begin(), searchResult.principalVariation.end());
        }

        const auto timeNow = std::chrono::high_resolution_clock::now();
        const auto millisecondsElapsed =
                std::chrono::duration_cast<std::chrono::milliseconds>(timeNow - startTime).count();

        const auto searchStatistics = moveSearcher_.getSearchStatistics();

        const int numNodes = searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched;
        const float nodesPerSecond = static_cast<float>(numNodes) / millisecondsElapsed * 1'000.0f;

        searchInfo.score          = searchResult.eval;
        searchInfo.depth          = depth;
        searchInfo.timeMs         = (int)millisecondsElapsed;
        searchInfo.numNodes       = numNodes;
        searchInfo.nodesPerSecond = (int)nodesPerSecond;

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
    }

    if (frontEnd_) {
        frontEnd_->reportSearchStatistics(moveSearcher_.getSearchStatistics());
    }

    return searchInfo;
}

SearchInfo Engine::Impl::findMove(
        const GameState& gameState, const std::chrono::milliseconds timeBudget) {
    moveSearcher_.prepareForNewSearch(gameState);
    auto moveFuture =
            std::async(std::launch::async, &Engine::Impl::findMoveWorker, this, gameState);

    (void)moveFuture.wait_for(timeBudget);
    moveSearcher_.interruptSearch();

    return moveFuture.get();
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

void Engine::setFrontEnd(const IFrontEnd* frontEnd) {
    impl_->setFrontEnd(frontEnd);
}

void Engine::newGame() {
    impl_->newGame();
}

SearchInfo Engine::findMove(
        const GameState& gameState, const std::chrono::milliseconds timeBudget) {
    return impl_->findMove(gameState, timeBudget);
}

void Engine::interruptSearch() {
    impl_->interruptSearch();
}

void Engine::setTTableSize(const int requestedSizeInMb) {
    impl_->setTTableSize(requestedSizeInMb);
}
