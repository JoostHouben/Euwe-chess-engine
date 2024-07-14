#include "Engine.h"

#include "Search.h"

#include <atomic>
#include <future>
#include <iostream>
#include <print>
#include <random>
#include <ranges>

class EngineImpl {
  public:
    EngineImpl();

    [[nodiscard]] SearchInfo findMove(
            const GameState& gameState, std::chrono::milliseconds timeBudget);

  private:
    [[nodiscard]] SearchInfo findMoveWorker(const GameState& gameState);

    StackOfVectors<Move> moveStack_;
    MoveSearcher moveSearcher_;
};

EngineImpl::EngineImpl() {
    moveStack_.reserve(1'000);
}

SearchInfo EngineImpl::findMoveWorker(const GameState& gameState) {
    GameState copySate(gameState);

    moveSearcher_.resetSearchStatistics();

    std::vector<Move> principalVariation;
    std::optional<EvalT> eval = std::nullopt;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth < 40; ++depth) {
        const auto searchResult =
                moveSearcher_.searchForBestMove(copySate, depth, moveStack_, eval);

        if (searchResult.principalVariation.size() > 0) {
            principalVariation = std::vector<Move>(
                    searchResult.principalVariation.begin(), searchResult.principalVariation.end());
        }

        std::string pvString = principalVariation | std::views::transform(moveToExtendedString)
                             | std::views::join_with(' ') | std::ranges::to<std::string>();

        const auto timeNow = std::chrono::high_resolution_clock::now();
        const auto millisecondsElapsed =
                std::chrono::duration_cast<std::chrono::milliseconds>(timeNow - startTime).count();

        if (searchResult.wasInterrupted) {
            std::print(
                    std::cerr,
                    "Partial search Depth {} - pv: {} (eval: {}; time elapsed: {} ms)\n",
                    depth,
                    pvString,
                    searchResult.eval,
                    millisecondsElapsed);

            --depth;
            break;
        }

        eval = searchResult.eval;

        std::print(
                std::cerr,
                "Depth {} - pv: {} (eval: {}; time elapsed: {} ms)\n",
                depth,
                pvString,
                *eval,
                millisecondsElapsed);

        if (isMate(*eval) && getMateDistanceInPly(*eval) <= depth) {
            break;
        }
    }

    const auto endTime = std::chrono::high_resolution_clock::now();
    const auto millisecondsElapsed =
            std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    const auto searchStatistics = moveSearcher_.getSearchStatistics();

    const int numNodes = searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched;

    const float nodesPerSecond = static_cast<float>(numNodes) / millisecondsElapsed * 1'000.0f;

    std::println(std::cerr, "Normal nodes searched: {}", searchStatistics.normalNodesSearched);
    std::println(std::cerr, "Quiescence nodes searched: {}", searchStatistics.qNodesSearched);
    std::println(std::cerr, "TTable hits: {}", searchStatistics.tTableHits);
    std::print(
            std::cerr, "TTable utilization: {:.1f}%\n", searchStatistics.ttableUtilization * 100.f);

    return {.principalVariation = principalVariation,
            .score              = *eval,
            .depth              = depth,
            .timeMs             = (int)millisecondsElapsed,
            .numNodes           = numNodes,
            .nodesPerSecond     = (int)nodesPerSecond};
}

SearchInfo EngineImpl::findMove(
        const GameState& gameState, const std::chrono::milliseconds timeBudget) {
    moveSearcher_.prepareForNewSearch(gameState);
    auto moveFuture = std::async(std::launch::async, &EngineImpl::findMoveWorker, this, gameState);

    (void)moveFuture.wait_for(timeBudget);
    moveSearcher_.interruptSearch();

    return moveFuture.get();
}

Engine::Engine() : impl_(std::make_unique<EngineImpl>()) {}

Engine::~Engine() = default;

SearchInfo Engine::findMove(
        const GameState& gameState, const std::chrono::milliseconds timeBudget) {
    return impl_->findMove(gameState, timeBudget);
}
