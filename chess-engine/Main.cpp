#include "chess-engine-lib/Engine.h"
#include "chess-engine-lib/Math.h"
#include "chess-engine-lib/Perft.h"

#include <iostream>
#include <print>
#include <ranges>
#include <sstream>

void playMoves(GameState& gameState, const std::vector<std::string>& moveStrings) {
    StackOfVectors<Move> stack;

    std::println("Starting fen: {}", gameState.toFen());
    std::string movesAsString =
            moveStrings | std::views::join_with(std::string(", ")) | std::ranges::to<std::string>();
    std::println("Moves: {}", movesAsString);

    for (const auto& moveString : moveStrings) {
        const auto moves = gameState.generateMoves(stack);
        for (Move move : moves) {
            if (moveToUciString(move) == moveString) {
                gameState.makeMove(move);
                break;
            }
        }
        std::println("{}: {}", moveString, gameState.toFen());
    }
}

struct UciState {
    GameState gameState = GameState::startingPosition();
};

void handleIsReady() {
    std::println("readyok");
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
        std::println(std::cerr, "Unrecognized token '{}'. Expected 'moves'.", token);
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

    std::println(std::cerr, "Position:\n{}", gameState.toVisualString());
}

void handleGo(Engine& engine, std::stringstream& lineSStream, UciState& uciState) {
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

    const auto searchInfo = engine.findMove(uciState.gameState, timeBudget);

    std::string scoreString = std::format("cp {}", searchInfo.score);
    if (isMate(searchInfo.score)) {
        const int mateInPly           = getMateDistanceInPly(searchInfo.score);
        const int mateInMoves         = (mateInPly + 1) / 2;
        const int relativeMateInMoves = signum(searchInfo.score) * mateInMoves;
        scoreString                   = std::format("mate {}", relativeMateInMoves);
    }

    const std::string pvString = searchInfo.principalVariation
                               | std::views::transform(moveToUciString) | std::views::join_with(' ')
                               | std::ranges::to<std::string>();

    std::println(
            "info depth {} time {} nodes {} nps {} score {} pv {}",
            searchInfo.depth,
            searchInfo.timeMs,
            searchInfo.numNodes,
            searchInfo.nodesPerSecond,
            scoreString,
            pvString);

    std::println("bestmove {}", moveToUciString(searchInfo.principalVariation[0]));
}

void runUci() {
    std::println("id name pimpl");
    std::println("id author Joost Houben");
    std::println("uciok");

    UciState uciState{};
    Engine engine{};

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
            handleGo(engine, lineSStream, uciState);
        } else if (command == "quit") {
            return;
        }
    }
}

void runPerft() {
    GameState gameState = GameState::startingPosition();

    std::println("Make + unmake:");
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