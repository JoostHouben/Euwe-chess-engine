#include "chess-engine-lib/GameState.h"

#include <chrono>
#include <fstream>
#include <map>
#include <print>
#include <ranges>
#include <vector>

inline const std::string kKiwipeteFen =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
inline const std::string kPosition3Fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
inline const std::string kPosition4Fen =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
inline const std::string kPosition5Fen =
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
inline const std::string kPosition6Fen =
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";

std::size_t perft(const GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    if (depth == 0) {
        return 1;
    }

    const StackVector<Move> moves = gameState.generateMoves(stack);

    if (depth == 1) {
        return moves.size();
    }

    std::size_t nodes = 0;
    for (const auto& move : moves) {
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
    for (const auto& move : moves) {
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
        const std::string& movePrefix = "") {
    if (splitDepth == 0) {
        const std::size_t nodes = perft(gameState, depth, stack);
        MY_ASSERT(splitMap.count(movePrefix) == 0);
        splitMap.emplace(movePrefix, nodes);
        return nodes;
    }

    const StackVector<Move> moves = gameState.generateMoves(stack);

    std::size_t nodes = 0;
    for (const auto& move : moves) {
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

void perftPrint(GameState& gameState, const int maxDepth, bool useUnmake = false) {
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

void playMoves(GameState& gameState, const std::vector<std::string>& moveStrings) {
    StackOfVectors<Move> stack;

    std::print("Starting fen: {}\n", gameState.toFen(0));
    std::string movesAsString =
            moveStrings | std::views::join_with(std::string(", ")) | std::ranges::to<std::string>();
    std::print("Moves: {}\n", movesAsString);

    for (const auto& moveString : moveStrings) {
        const auto moves = gameState.generateMoves(stack);
        for (const auto& move : moves) {
            if (moveToUciString(move) == moveString) {
                gameState.makeMove(move);
                break;
            }
        }
        std::print("{}: {}\n", moveString, gameState.toFen(0));
    }
}

int main() {
    std::locale::global(std::locale("en_US.UTF-8"));

    GameState gameState = GameState::startingPosition();

    std::print("Copy + make:\n");
    perftPrint(gameState, 7);

    std::print("\nMake + unmake:\n");
    perftPrint(gameState, 7, true);
}