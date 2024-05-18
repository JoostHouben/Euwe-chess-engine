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

[[nodiscard]] SearchInfo findMoveWorker(const GameState& gameState) {
    gMoveStack.reserve(1'000);

    GameState copySate(gameState);

    resetSearchStatistics();

    std::vector<Move> principalVariation;
    std::optional<EvalT> eval = std::nullopt;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth < 40; ++depth) {
        const auto searchResult = searchForBestMove(copySate, depth, gMoveStack, eval);

        if (searchResult.principalVariation.size() > 0) {
            principalVariation = std::vector<Move>(
                    searchResult.principalVariation.begin(), searchResult.principalVariation.end());
        }

        std::string pvString = principalVariation | std::views::transform(moveToExtendedString)
                             | std::views::join_with(' ') | std::ranges::to<std::string>();

        if (!searchResult.eval.has_value()) {
            // Search was stopped externally
            std::print(std::cerr, "Partial search Depth {} - pv: {}\n", depth, pvString);

            --depth;
            break;
        }
        eval = searchResult.eval.value();

        const auto timeNow = std::chrono::high_resolution_clock::now();
        const auto millisecondsElapsed =
                std::chrono::duration_cast<std::chrono::milliseconds>(timeNow - startTime).count();

        std::print(
                std::cerr,
                "Depth {} - pv: {} (eval: {}; time elapsed: {} ms)\n",
                depth,
                pvString,
                *eval,
                millisecondsElapsed);

        if (isMate(*eval) && getMateDistance(*eval) <= depth) {
            break;
        }
    }

    const auto endTime = std::chrono::high_resolution_clock::now();
    const auto millisecondsElapsed =
            std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    const auto searchStatistics = getSearchStatistics();

    const int numNodes = searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched;

    const float nodesPerSecond = static_cast<float>(numNodes) / millisecondsElapsed * 1'000.0f;

    std::print(std::cerr, "Normal nodes searched: {}\n", searchStatistics.normalNodesSearched);
    std::print(std::cerr, "Quiescence nodes searched: {}\n", searchStatistics.qNodesSearched);
    std::print(std::cerr, "TTable hits: {}\n", searchStatistics.tTableHits);
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

SearchInfo findMove(const GameState& gameState, std::chrono::milliseconds timeBudget) {
    prepareForSearch();
    auto moveFuture = std::async(std::launch::async, findMoveWorker, gameState);

    (void)moveFuture.wait_for(timeBudget);
    requestSearchStop();

    return moveFuture.get();
}