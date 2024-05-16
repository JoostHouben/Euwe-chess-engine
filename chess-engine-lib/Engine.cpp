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

std::atomic_bool gSearchIsRunning = false;

[[nodiscard]] bool reachesDraw(
        GameState& gameState, const StackVector<Move>& principalVariation, int moveIdx = 0) {
    if (moveIdx == principalVariation.size()) {
        if (gameState.isRepetition() || gameState.isFiftyMoves()) {
            return true;
        }

        const auto moves = gameState.generateMoves(gMoveStack);
        return moves.size() == 0;
    }

    const auto move = principalVariation[moveIdx];

    const auto unmakeInfo = gameState.makeMove(move);
    const bool isDraw     = reachesDraw(gameState, principalVariation, moveIdx + 1);
    gameState.unmakeMove(move, unmakeInfo);

    return isDraw;
}

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

        if (isMate(eval) || (eval == 0 && reachesDraw(copySate, searchResult.principalVariation))) {
            break;
        }
    }

    gSearchIsRunning = false;

    const auto endTime = std::chrono::high_resolution_clock::now();
    const auto millisecondsElapsed =
            std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    const auto searchStatistics = getSearchStatistics();

    const int numNodes = searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched;

    const float nodesPerSecond = static_cast<float>(numNodes) / millisecondsElapsed * 1'000.0f;

    std::print(std::cerr, "Normal nodes searched: {}\n", searchStatistics.normalNodesSearched);
    std::print(std::cerr, "Quiescence nodes searched: {}\n", searchStatistics.qNodesSearched);
    std::print(std::cerr, "TTable hits: {}\n", searchStatistics.tTableHits);

    return {.principalVariation = principalVariation,
            .score              = eval,
            .depth              = depth,
            .timeMs             = (int)millisecondsElapsed,
            .numNodes           = numNodes,
            .nodesPerSecond     = (int)nodesPerSecond};
}

void stopSearchAfter(std::chrono::milliseconds timeLimit) {
    const auto startTime = std::chrono::high_resolution_clock::now();

    do {
        std::this_thread::sleep_for(timeLimit / 10);

        if (!gSearchIsRunning) {
            return;
        }
    } while (std::chrono::high_resolution_clock::now() - startTime < timeLimit);

    stopSearch();
}

}  // namespace

SearchInfo findMove(const GameState& gameState) {
    startSearch();
    gSearchIsRunning = true;

    auto moveFuture = std::async(std::launch::async, findMoveWorker, gameState);

    stopSearchAfter(std::chrono::milliseconds(1'000));

    return moveFuture.get();
}