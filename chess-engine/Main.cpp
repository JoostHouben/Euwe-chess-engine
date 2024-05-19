#include "chess-engine-lib/Engine.h"
#include "chess-engine-lib/Perft.h"

#include <iostream>
#include <print>
#include <ranges>
#include <sstream>

void playMoves(GameState& gameState, const std::vector<std::string>& moveStrings) {
    StackOfVectors<Move> stack;

    std::print("Starting fen: {}\n", gameState.toFen());
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
        std::print("{}: {}\n", moveString, gameState.toFen());
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
    // Most sub-commands not supported

    auto timeBudget = std::chrono::milliseconds(1000);

    while (lineSStream) {
        std::string token;
        lineSStream >> token;
        if (token.empty()) {
            break;
        }

        const std::string ourIncString =
                uciState.gameState.getSideToMove() == Side::White ? "winc" : "binc";

        // If an increment is given, use the time budget as increment
        if (token == ourIncString) {
            int incMs;
            lineSStream >> incMs;
            timeBudget = std::chrono::milliseconds(incMs);
        }
    }

    const auto searchInfo = findMove(uciState.gameState, timeBudget);

    std::string scoreString = std::format("cp {}", searchInfo.score);
    if (isMate(searchInfo.score)) {
        const int mateInMoves = getMateDistance(searchInfo.score);
        scoreString           = std::format("mate {}", mateInMoves);
    }

    const std::string pvString = searchInfo.principalVariation
                               | std::views::transform(moveToUciString) | std::views::join_with(' ')
                               | std::ranges::to<std::string>();

    std::print("info depth {}\n", searchInfo.depth);
    std::print("info time {}\n", searchInfo.timeMs);
    std::print("info nodes {}\n", searchInfo.numNodes);
    std::print("info nps {}\n", searchInfo.nodesPerSecond);
    std::print("info pv {}\n", pvString);
    std::print("info score {}\n", scoreString);

    std::print("bestmove {}\n", moveToUciString(searchInfo.principalVariation[0]));
}

void runUci() {
    std::print("id name no-score-from-partial\n");
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