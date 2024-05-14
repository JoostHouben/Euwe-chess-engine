#include "chess-engine-lib/Perft.h"

#include <print>
#include <ranges>

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