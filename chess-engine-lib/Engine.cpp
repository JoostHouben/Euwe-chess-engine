#include "Engine.h"

#include "Search.h"

#include <atomic>
#include <future>
#include <iostream>
#include <print>
#include <random>
#include <ranges>

namespace {

StackOfVectors<Move> gMoveStack;
MoveSearcher gMoveSearcher;

[[nodiscard]] SearchInfo findMoveWorker(const GameState& gameState) {
    gMoveStack.reserve(1'000);

    GameState copySate(gameState);

    gMoveSearcher.resetSearchStatistics();

    std::vector<Move> principalVariation;
    std::optional<EvalT> eval = std::nullopt;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth < 40; ++depth) {
        const auto searchResult =
                gMoveSearcher.searchForBestMove(copySate, depth, gMoveStack, eval);

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

    const auto searchStatistics = gMoveSearcher.getSearchStatistics();

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

}  // namespace

void initializeEngine() {
    gMoveSearcher.initializeSearch();
}

SearchInfo findMove(const GameState& gameState, std::chrono::milliseconds timeBudget) {
    gMoveSearcher.prepareForSearch(gameState);
    auto moveFuture = std::async(std::launch::async, findMoveWorker, gameState);

    (void)moveFuture.wait_for(timeBudget);
    gMoveSearcher.requestSearchStop();

    return moveFuture.get();
}