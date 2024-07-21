#include "UciFrontEnd.h"

#include "Math.h"

#include <iostream>
#include <print>
#include <ranges>

namespace {

std::string pvToString(const std::vector<Move>& principalVariation) {
    return principalVariation | std::views::transform(moveToUciString) | std::views::join_with(' ')
         | std::ranges::to<std::string>();
}

std::string scoreToString(const EvalT score) {
    MY_ASSERT(isValid(score));

    if (isMate(score)) {
        const int mateInPly           = getMateDistanceInPly(score);
        const int mateInMoves         = (mateInPly + 1) / 2;
        const int relativeMateInMoves = signum(score) * mateInMoves;
        return std::format("mate {}", relativeMateInMoves);
    }

    return std::format("cp {}", score);
}

}  // namespace

UciFrontEnd::UciFrontEnd() : engine_(this), gameState_(GameState::startingPosition()) {}

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
        } else if (command.empty()) {
            continue;
        } else {
            std::println(std::cerr, "Ignoring unknown command: '{}'", command);
        }
    }
}

void UciFrontEnd::reportFullSearch(const SearchInfo& searchInfo) const {
    const std::string scoreString = scoreToString(searchInfo.score);

    const std::string pvString = pvToString(searchInfo.principalVariation);

    std::println(
            "info depth {} time {} nodes {} nps {} score {} pv {}",
            searchInfo.depth,
            searchInfo.timeMs,
            searchInfo.numNodes,
            searchInfo.nodesPerSecond,
            scoreString,
            pvString);
}

void UciFrontEnd::reportPartialSearch(const SearchInfo& searchInfo) const {
    std::println(std::cerr, "Completed partial search of depth {}", searchInfo.depth);

    const int completedDepth = searchInfo.depth - 1;

    std::string optionalScoreString = "";
    if (isValid(searchInfo.score)) {
        optionalScoreString = std::format(" score {}", scoreToString(searchInfo.score));
    }

    const std::string pvString = pvToString(searchInfo.principalVariation);

    std::println(
            "info depth {} time {} nodes {} nps {}{} pv {}",
            completedDepth,
            searchInfo.timeMs,
            searchInfo.numNodes,
            searchInfo.nodesPerSecond,
            optionalScoreString,
            pvString);
}

void UciFrontEnd::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    std::println(std::cerr, "Normal nodes searched: {}", searchStatistics.normalNodesSearched);
    std::println(std::cerr, "Quiescence nodes searched: {}", searchStatistics.qNodesSearched);
    std::println(std::cerr, "TTable hits: {}", searchStatistics.tTableHits);
    std::print(
            std::cerr, "TTable utilization: {:.1f}%\n", searchStatistics.ttableUtilization * 100.f);
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

    std::println("bestmove {}", moveToUciString(searchInfo.principalVariation[0]));
}
