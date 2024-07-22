#include "Engine.h"

#include "Search.h"
#include "UciFrontEnd.h"

#include <future>
#include <iostream>
#include <print>
#include <ranges>

class EngineImpl {
  public:
    EngineImpl(const UciFrontEnd* uciFrontEnd);

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget);

    void interruptSearch();

  private:
    [[nodiscard]] SearchInfo findMoveWorker(const GameState& gameState);

    StackOfVectors<Move> moveStack_;
    MoveSearcher moveSearcher_;
    const UciFrontEnd* uciFrontEnd_;
};

EngineImpl::EngineImpl(const UciFrontEnd* uciFrontEnd)
    : moveSearcher_(uciFrontEnd), uciFrontEnd_(uciFrontEnd) {
    moveStack_.reserve(1'000);
}

SearchInfo EngineImpl::findMoveWorker(const GameState& gameState) {
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
            if (uciFrontEnd_) {
                uciFrontEnd_->reportPartialSearch(searchInfo);
            }
            searchInfo.depth -= 1;
            break;
        }

        if (uciFrontEnd_) {
            uciFrontEnd_->reportFullSearch(searchInfo);
        }

        if (isMate(searchResult.eval) && getMateDistanceInPly(searchResult.eval) <= depth) {
            break;
        }
    }

    if (uciFrontEnd_) {
        uciFrontEnd_->reportSearchStatistics(moveSearcher_.getSearchStatistics());
    }

    return searchInfo;
}

SearchInfo EngineImpl::findMove(
        const GameState& gameState, const std::chrono::milliseconds timeBudget) {
    moveSearcher_.prepareForNewSearch(gameState);
    auto moveFuture = std::async(std::launch::async, &EngineImpl::findMoveWorker, this, gameState);

    (void)moveFuture.wait_for(timeBudget);
    moveSearcher_.interruptSearch();

    return moveFuture.get();
}

void EngineImpl::interruptSearch() {
    moveSearcher_.interruptSearch();
}

Engine::Engine(const UciFrontEnd* uciFrontEnd) : impl_(std::make_unique<EngineImpl>(uciFrontEnd)) {}

Engine::~Engine() = default;

SearchInfo Engine::findMove(
        const GameState& gameState, const std::chrono::milliseconds timeBudget) {
    return impl_->findMove(gameState, timeBudget);
}

void Engine::interruptSearch() {
    impl_->interruptSearch();
}
