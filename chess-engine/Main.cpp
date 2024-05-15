#include "chess-engine-lib/Engine.h"
#include "chess-engine-lib/Perft.h"

#include <iostream>
#include <print>
#include <ranges>
#include <sstream>

void playMoves(GameState& gameState, const std::vector<std::string>& moveStrings) {
    StackOfVectors<Move> stack;

    std::print("Starting fen: {}\n", gameState.toFen(0));
    std::string movesAsString =
            moveStrings | std::views::join_with(std::string(", ")) | std::ranges::to<std::string>();
    std::print("Moves: {}\n", movesAsString);

    for (const auto& moveString : moveStrings) {
        const auto moves = gameState.generateMoves(stack);
        for (Move move : moves) {
            if (moveToUciString(move) == moveString) {
                gameState.makeMove(move);
                break;
            }
        }
        std::print("{}: {}\n", moveString, gameState.toFen(0));
    }
}

struct UciState {
    GameState gameState = GameState::startingPosition();
};

void handleIsReady() {
    std::print("readyok\n");
}

void handlePosition(std::stringstream& lineSStream, UciState& uciState) {
    std::string token;
    lineSStream >> token;

    GameState& gameState = uciState.gameState;

    if (token == "startpos") {
        gameState = GameState::startingPosition();

        lineSStream >> token;
    } else if (token == "fen") {
        std::string fen;
        lineSStream >> token;
        while (token != "moves" && lineSStream) {
            fen += token + " ";
            lineSStream >> token;
        }
        fen.pop_back();  // remove trailing space
        gameState = GameState::fromFen(fen);
    }

    if (token != "moves") {
        std::print(std::cerr, "Unrecognized token '{}'. Expected 'moves'.\n", token);
        return;
    }

    while (lineSStream) {
        std::string moveString;
        lineSStream >> moveString;
        if (moveString.empty()) {
            break;
        }

        const Move move = moveFromUciString(moveString, gameState);
        (void)gameState.makeMove(move);
    }

    std::print(std::cerr, "Position:\n{}\n", gameState.toVisualString());
}

void handleGo(std::stringstream& lineSStream, UciState& uciState) {
    // No sub-commands supported

    const Move moveToPlay = findMove(uciState.gameState);
    std::print("bestmove {}\n", moveToUciString(moveToPlay));
}

void runUci() {
    std::print("id name Minimax-depth4\n");
    std::print("id author Joost Houben\n");
    std::print("uciok\n");

    UciState uciState{};

    while (true) {
        std::string inputLine;
        std::getline(std::cin, inputLine);

        std::stringstream lineSStream(inputLine);

        std::string command;
        lineSStream >> command;

        // Not implemented:
        //  debug
        //  setoption
        //  register
        //  stop
        //  ponderhit

        if (command == "isready") {
            handleIsReady();
        } else if (command == "position") {
            handlePosition(lineSStream, uciState);
        } else if (command == "go") {
            handleGo(lineSStream, uciState);
        } else if (command == "quit") {
            return;
        }
    }
}

void runPerft() {
    GameState gameState = GameState::startingPosition();

    std::print("Copy + make:\n");
    perftPrint(gameState, 7);

    std::print("\nMake + unmake:\n");
    perftPrint(gameState, 7, true);
}

int main() {
    std::locale::global(std::locale("en_US.UTF-8"));

    while (true) {
        std::string command;
        std::cin >> command;

        if (command == "uci") {
            runUci();
            break;
        } else if (command == "perft") {
            runPerft();
        } else if (command == "exit") {
            break;
        }
    }
}