#include "Engine.h"

#include "Search.h"

#include <atomic>
#include <chrono>
#include <future>
#include <iostream>
#include <print>
#include <random>

namespace {

StackOfVectors<Move> gMoveStack;
std::atomic_bool gStopSearch;

[[nodiscard]] SearchInfo findMoveWorker(const GameState& gameState) {
    gMoveStack.reserve(1'000);

    GameState copySate(gameState);

    resetSearchStatistics();

    Move bestMove;
    EvalT eval;

    auto startTime = std::chrono::high_resolution_clock::now();

    int depth;
    for (depth = 1; depth < 40; ++depth) {
        const auto searchResult = searchForBestMove(copySate, depth, gMoveStack);

        if (gStopSearch) {
            std::print(
                    std::cerr,
                    "Terminating search during search of depth {} (completed to depth {}).\n",
                    depth,
                    depth - 1);

            --depth;
            break;
        }

        bestMove = searchResult.bestMove;
        eval     = searchResult.eval;

        const auto timeNow = std::chrono::high_resolution_clock::now();
        const auto millisecondsElapsed =
                std::chrono::duration_cast<std::chrono::milliseconds>(timeNow - startTime).count();

        std::print(
                std::cerr,
                "Depth {} - best move: {} (eval: {}; time elapsed: {} ms)\n",
                depth,
                moveToExtendedString(bestMove),
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

    const SearchInfo info{
            .bestMove       = bestMove,
            .score          = eval,
            .depth          = depth,
            .timeMs         = (int)millisecondsElapsed,
            .numNodes       = searchStatistics.nodesSearched,
            .nodesPerSecond = (int)nodesPerSecond};

    return info;
}

}  // namespace

SearchInfo findMove(const GameState& gameState) {
    gStopSearch = false;

    auto moveFuture = std::async(std::launch::async, findMoveWorker, gameState);

    // Sleep while search is in progress.
    std::this_thread::sleep_for(std::chrono::seconds(1));
    // Set stop signal for worker thread.
    gStopSearch = true;
    // Send stop signal to search function.
    stopSearch();

    return moveFuture.get();
}