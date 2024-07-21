#include "UciFrontEnd.h"

#include "Math.h"

#include <iostream>
#include <print>
#include <ranges>

UciFrontEnd::UciFrontEnd() : engine_(), gameState_(GameState::startingPosition()) {}

void UciFrontEnd::run() {
    std::println("id name prefetch");
    std::println("id author Joost Houben");
    std::println("uciok");

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
            handlePosition(lineSStream);
        } else if (command == "go") {
            handleGo(lineSStream);
        } else if (command == "quit") {
            return;
        }
    }
}

void UciFrontEnd::handleIsReady() const {
    std::println("readyok");
}

void UciFrontEnd::handlePosition(std::stringstream& lineSStream) {
    std::string token;
    lineSStream >> token;

    if (token == "startpos") {
        gameState_ = GameState::startingPosition();

        lineSStream >> token;
    } else if (token == "fen") {
        std::string fen;
        lineSStream >> token;
        while (token != "moves" && lineSStream) {
            fen += token + " ";
            lineSStream >> token;
        }
        fen.pop_back();  // remove trailing space
        gameState_ = GameState::fromFen(fen);
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

        const Move move = moveFromUciString(moveString, gameState_);
        (void)gameState_.makeMove(move);
    }

    std::println(std::cerr, "Position:\n{}", gameState_.toVisualString());
}

void UciFrontEnd::handleGo(std::stringstream& lineSStream) {
    // Most sub-commands not supported

    auto timeBudget = std::chrono::milliseconds(1000);

    while (lineSStream) {
        std::string token;
        lineSStream >> token;
        if (token.empty()) {
            break;
        }

        const std::string ourIncString =
                gameState_.getSideToMove() == Side::White ? "winc" : "binc";

        // If an increment is given, use the time budget as increment
        if (token == ourIncString) {
            int incMs;
            lineSStream >> incMs;
            timeBudget = std::chrono::milliseconds(incMs);
        }
    }

    const auto searchInfo = engine_.findMove(gameState_, timeBudget);

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
