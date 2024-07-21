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

template <typename... Args>
void writeUci(std::format_string<Args...> fmt, Args&&... args) {
    std::println(fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void writeDebug(std::format_string<Args...> fmt, Args&&... args) {
    std::println(std::cerr, "[DEBUG] {}", std::format(fmt, std::forward<Args>(args)...));
}

}  // namespace

UciFrontEnd::UciFrontEnd() : engine_(this), gameState_(GameState::startingPosition()) {}

void UciFrontEnd::run() {
    writeUci("id name prefetch");
    writeUci("id author Joost Houben");
    writeUci("uciok");

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
            writeDebug("Ignoring unknown command: '{}'", command);
        }
    }
}

void UciFrontEnd::reportFullSearch(const SearchInfo& searchInfo) const {
    const std::string scoreString = scoreToString(searchInfo.score);

    const std::string pvString = pvToString(searchInfo.principalVariation);

    writeUci(
            "info depth {} time {} nodes {} nps {} score {} pv {}",
            searchInfo.depth,
            searchInfo.timeMs,
            searchInfo.numNodes,
            searchInfo.nodesPerSecond,
            scoreString,
            pvString);
}

void UciFrontEnd::reportPartialSearch(const SearchInfo& searchInfo) const {
    writeDebug("Completed partial search of depth {}", searchInfo.depth);

    const int completedDepth = searchInfo.depth - 1;

    std::string optionalScoreString = "";
    if (isValid(searchInfo.score)) {
        optionalScoreString = std::format(" score {}", scoreToString(searchInfo.score));
    }

    const std::string pvString = pvToString(searchInfo.principalVariation);

    writeUci(
            "info depth {} time {} nodes {} nps {}{} pv {}",
            completedDepth,
            searchInfo.timeMs,
            searchInfo.numNodes,
            searchInfo.nodesPerSecond,
            optionalScoreString,
            pvString);
}

void UciFrontEnd::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    writeDebug("Normal nodes searched: {}", searchStatistics.normalNodesSearched);
    writeDebug("Quiescence nodes searched: {}", searchStatistics.qNodesSearched);
    writeDebug("TTable hits: {}", searchStatistics.tTableHits);
    writeDebug("TTable utilization: {:.1f}%", searchStatistics.ttableUtilization * 100.f);
}

void UciFrontEnd::reportAspirationWindowReSearch(
        const EvalT previousLowerBound,
        const EvalT previousUpperBound,
        const EvalT searchEval,
        const EvalT newLowerBound,
        const EvalT newUpperBound) const {
    writeDebug(
            "Aspiration window [{}, {}] failed (search returned {}); re-searching with window [{}, "
            "{}]",
            previousLowerBound,
            previousUpperBound,
            searchEval,
            newLowerBound,
            newUpperBound);
}

void UciFrontEnd::reportDiscardedPv(std::string_view reason) const {
    writeDebug("Discarded PV: {}", reason);
}

void UciFrontEnd::handleIsReady() const {
    writeUci("readyok");
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
        writeDebug("Unrecognized token '{}'. Expected 'moves'.", token);
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

    writeDebug("Position:\n{}", gameState_.toVisualString());
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

    writeUci("bestmove {}", moveToUciString(searchInfo.principalVariation[0]));
}