#include "chess-engine-lib/GameState.h"

#include <chrono>
#include <fstream>
#include <map>
#include <print>
#include <vector>

inline const std::string kKiwipeteFen =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
inline const std::string kPosition3Fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
inline const std::string kPosition4Fen =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
inline const std::string kPosition5Fen =
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";

std::size_t perft(const GameState& gameState, int depth, StackOfVectors<Move>& stack) {
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

std::size_t perftSplit(
        const GameState& gameState,
        int depth,
        int splitDepth,
        StackOfVectors<Move>& stack,
        std::map<std::string, std::size_t>& splitMap,
        const std::string& movePrefix = "") {
    if (splitDepth == 0) {
        std::size_t nodes = perft(gameState, depth, stack);
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
        moveString += moveToStringSimple(move);

        GameState copy = gameState;
        copy.makeMove(move);
        nodes += perftSplit(copy, depth - 1, splitDepth - 1, stack, splitMap, moveString);
    }

    return nodes;
}

void playMoves(GameState& gameState, const std::vector<std::string>& moveStrings) {
    StackOfVectors<Move> stack;

    for (const auto& moveString : moveStrings) {
        auto moves = gameState.generateMoves(stack);
        for (const auto& move : moves) {
            if (moveToStringSimple(move) == moveString) {
                gameState.makeMove(move);
                break;
            }
        }
    }
}

int main() {
    std::locale::global(std::locale("en_US.UTF-8"));

    GameState gameState = GameState::startingPosition();
    StackOfVectors<Move> stack;
    stack.reserve(300);

    for (int depth = 1; depth <= 7; ++depth) {
        auto startTime    = std::chrono::high_resolution_clock::now();
        std::size_t nodes = perft(gameState, depth, stack);
        auto endTime      = std::chrono::high_resolution_clock::now();

        using DoubleSecondsT = std::chrono::duration<double, std::ratio<1>>;

        auto seconds   = std::chrono::duration_cast<DoubleSecondsT>(endTime - startTime).count();
        double megaNps = (double)nodes / seconds / 1'000'000;

        std::print(
                "Depth: {} - nodes: {:>13L} ({:>8.3g} s; {:>5.1f} Mnps)\n",
                depth,
                nodes,
                seconds,
                megaNps);
    }
}