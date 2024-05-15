#include "Perft.h"

#include <chrono>
#include <print>

std::size_t perft(const GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    if (depth == 0) {
        return 1;
    }

    const StackVector<Move> moves = gameState.generateMoves(stack);

    if (depth == 1) {
        return moves.size();
    }

    std::size_t nodes = 0;
    for (Move move : moves) {
        GameState copy = gameState;
        copy.makeMove(move);
        nodes += perft(copy, depth - 1, stack);
    }

    return nodes;
}

std::size_t perftUnmake(GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    if (depth == 0) {
        return 1;
    }

    const StackVector<Move> moves = gameState.generateMoves(stack);

    if (depth == 1) {
        return moves.size();
    }

    std::size_t nodes = 0;
    for (Move move : moves) {
        auto unmakeInfo = gameState.makeMove(move);
        nodes += perftUnmake(gameState, depth - 1, stack);
        gameState.unmakeMove(move, unmakeInfo);
    }

    return nodes;
}

std::size_t perftSplit(
        const GameState& gameState,
        const int depth,
        const int splitDepth,
        StackOfVectors<Move>& stack,
        std::map<std::string, std::size_t>& splitMap,
        const std::string& movePrefix) {
    if (splitDepth == 0) {
        const std::size_t nodes = perft(gameState, depth, stack);
        MY_ASSERT(splitMap.count(movePrefix) == 0);
        splitMap.emplace(movePrefix, nodes);
        return nodes;
    }

    const StackVector<Move> moves = gameState.generateMoves(stack);

    std::size_t nodes = 0;
    for (Move move : moves) {
        std::string moveString = movePrefix;
        if (!moveString.empty()) {
            moveString += ',';
        }
        moveString += moveToUciString(move);

        GameState copy = gameState;
        copy.makeMove(move);
        nodes += perftSplit(copy, depth - 1, splitDepth - 1, stack, splitMap, moveString);
    }

    return nodes;
}

void perftPrint(GameState& gameState, const int maxDepth, const bool useUnmake) {
    StackOfVectors<Move> stack;
    stack.reserve(300);

    for (int depth = 1; depth <= maxDepth; ++depth) {
        const auto startTime = std::chrono::high_resolution_clock::now();
        std::size_t nodes;
        if (useUnmake) {
            nodes = perftUnmake(gameState, depth, stack);
        } else {
            nodes = perft(gameState, depth, stack);
        }
        const auto endTime = std::chrono::high_resolution_clock::now();

        using DoubleSecondsT = std::chrono::duration<double, std::ratio<1>>;

        const auto seconds =
                std::chrono::duration_cast<DoubleSecondsT>(endTime - startTime).count();
        const double megaNps = (double)nodes / seconds / 1'000'000;

        std::print(
                "Depth: {} - nodes: {:>13L} ({:>6.3f} s; {:>5.1f} Mn/s)\n",
                depth,
                nodes,
                seconds,
                megaNps);
    }
}

void perftSplitPrint(const GameState& gameState, const int depth, const int splitDepth) {
    StackOfVectors<Move> stack;
    stack.reserve(300);
    std::map<std::string, std::size_t> splitMap;

    const std::size_t nodes = perftSplit(gameState, depth, splitDepth, stack, splitMap);

    for (const auto& [moveString, nodes] : splitMap) {
        std::print("{}: {}\n", moveString, nodes);
    }
    std::print("{} total nodes\n", nodes);
}
