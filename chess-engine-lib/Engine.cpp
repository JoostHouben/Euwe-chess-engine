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

[[nodiscard]] Move findMoveWorker(const GameState& gameState) {
    gMoveStack.reserve(1'000);

    GameState copySate(gameState);

    Move moveToPlay;
    EvalT eval;

    for (int depth = 1; depth < 40; ++depth) {
        const auto searchResult = searchForBestMove(copySate, depth, gMoveStack);

        if (gStopSearch) {
            std::print(
                    std::cerr,
                    "Terminating search during search of depth {} (completed to depth {}).\n",
                    depth,
                    depth - 1);
            break;
        }

        moveToPlay = searchResult.bestMove;
        eval       = searchResult.eval;
    }

    std::print(std::cerr, "Best move: {} (eval: {})\n", moveToExtendedString(moveToPlay), eval);

    return moveToPlay;
}

}  // namespace

Move findMove(const GameState& gameState) {
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