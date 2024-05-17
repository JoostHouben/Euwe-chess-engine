#include "Engine.h"

#include "Search.h"

#include <atomic>
#include <chrono>
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
    EvalT eval;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth < 40; ++depth) {
        const auto searchResult = searchForBestMove(copySate, depth, gMoveStack);

        principalVariation = std::vector<Move>(
                searchResult.principalVariation.begin(), searchResult.principalVariation.end());

        std::string pvString = principalVariation | std::views::transform(moveToExtendedString)
                             | std::views::join_with(' ') | std::ranges::to<std::string>();

        if (!searchResult.eval.has_value()) {
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
                eval,
                millisecondsElapsed);
    }

    const auto endTime = std::chrono::high_resolution_clock::now();
    const auto millisecondsElapsed =
            std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    const auto searchStatistics = getSearchStatistics();

    const float nodesPerSecond =
            static_cast<float>(searchStatistics.nodesSearched) / (millisecondsElapsed / 1'000.0f);

    std::print(std::cerr, "TTable hits: {}\n", searchStatistics.tTableHits);

    return {.principalVariation = principalVariation,
            .score              = eval,
            .depth              = depth,
            .timeMs             = (int)millisecondsElapsed,
            .numNodes           = searchStatistics.nodesSearched,
            .nodesPerSecond     = (int)nodesPerSecond};
}

}  // namespace

SearchInfo findMove(const GameState& gameState) {
    auto moveFuture = std::async(std::launch::async, findMoveWorker, gameState);

    // Sleep while search is in progress.
    std::this_thread::sleep_for(std::chrono::seconds(1));
    // Send stop signal to search function.
    stopSearch();

    return moveFuture.get();
}