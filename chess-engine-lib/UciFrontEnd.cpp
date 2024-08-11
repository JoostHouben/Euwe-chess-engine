#include "UciFrontEnd.h"

#include "ConsoleColor.h"
#include "Math.h"
#include "MyAssert.h"

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
void writeUci(const std::format_string<Args...> fmt, Args&&... args) {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Green);

    std::println(fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void writeDebug(const bool debugToUci, const std::format_string<Args...> fmt, Args&&... args) {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow);

    if (debugToUci) {
        std::println("info string {}", std::format(fmt, std::forward<Args>(args)...));
    } else {
        std::println(std::cerr, "[DEBUG] {}", std::format(fmt, std::forward<Args>(args)...));
    }
}

}  // namespace

UciFrontEnd::UciFrontEnd() : engine_(this), gameState_(GameState::startingPosition()) {
    // Add UCI hard-coded options
    addOption(
            "Hash",
            FrontEndOption::createInteger(
                    1, 1, 1 * 1024 * 1024, [this](const int requestedSizeInMb) {
                        engine_.setTTableSize(requestedSizeInMb);
                    }));
}

UciFrontEnd::~UciFrontEnd() {
    MY_ASSERT(!goFuture.valid());
}

void UciFrontEnd::run() {
    writeUci("id name setoption");
    writeUci("id author Joost Houben");

    writeOptions();

    writeUci("uciok");
    std::flush(std::cout);

    while (true) {
        std::string inputLine;
        std::getline(std::cin, inputLine);

        std::stringstream lineSStream(inputLine);

        std::string command;
        lineSStream >> command;

        // Not implemented:
        //  ponderhit

        if (command == "isready") {
            handleIsReady();
        } else if (command == "ucinewgame") {
            handleNewGame();
        } else if (command == "position") {
            handlePosition(lineSStream);
        } else if (command == "go") {
            handleGo(lineSStream);
        } else if (command == "stop") {
            handleStop();
        } else if (command == "debug") {
            handleDebug(lineSStream);
        } else if (command == "quit") {
            waitForGoToComplete();
            return;
        } else if (command == "register") {
            handleRegister();
        } else if (command == "setoption") {
            handleSetOption(inputLine);
        } else if (command.empty()) {
            continue;
        } else {
            writeDebug(debugMode_, "Ignoring unknown command: '{}'", command);
        }
    }
}

void UciFrontEnd::reportFullSearch(
        const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const {
    std::string optionalScoreString = "";
    if (isValid(searchInfo.score)) {
        optionalScoreString = std::format(" score {}", scoreToString(searchInfo.score));
    }

    const std::string pvString = pvToString(searchInfo.principalVariation);

    writeUci(
            "info depth {} seldepth {} time {} nodes {} nps {} hashfull {}{} pv {}",
            searchInfo.depth,
            searchStatistics.selectiveDepth,
            searchInfo.timeMs,
            searchInfo.numNodes,
            searchInfo.nodesPerSecond,
            (int)(searchStatistics.ttableUtilization * 1000),
            optionalScoreString,
            pvString);
}

void UciFrontEnd::reportPartialSearch(
        const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const {
    writeDebug(debugMode_, "Completed partial search of depth {}", searchInfo.depth);

    SearchInfo completedSearchInfo = searchInfo;
    completedSearchInfo.depth      = searchInfo.depth - 1;

    reportFullSearch(completedSearchInfo, searchStatistics);
}

void UciFrontEnd::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    writeDebug(debugMode_, "Normal nodes searched: {}", searchStatistics.normalNodesSearched);
    writeDebug(debugMode_, "Quiescence nodes searched: {}", searchStatistics.qNodesSearched);
    writeDebug(debugMode_, "TTable hits: {}", searchStatistics.tTableHits);
    writeDebug(
            debugMode_, "TTable utilization: {:.1f}%", searchStatistics.ttableUtilization * 100.f);
}

void UciFrontEnd::reportAspirationWindowReSearch(
        const EvalT previousLowerBound,
        const EvalT previousUpperBound,
        const EvalT searchEval,
        const EvalT newLowerBound,
        const EvalT newUpperBound) const {
    writeDebug(
            debugMode_,
            "Aspiration window [{}, {}] failed (search returned {}); re-searching with window [{}, "
            "{}]",
            previousLowerBound,
            previousUpperBound,
            searchEval,
            newLowerBound,
            newUpperBound);
}

void UciFrontEnd::reportDiscardedPv(std::string_view reason) const {
    writeDebug(debugMode_, "Discarded PV: {}", reason);
}

void UciFrontEnd::addOption(std::string name, FrontEndOption option) {
    optionsMap_.emplace(std::move(name), std::move(option));
}

void UciFrontEnd::handleIsReady() {
    waitForGoToComplete();
    writeUci("readyok");
    std::flush(std::cout);
}

void UciFrontEnd::handleNewGame() {
    engine_.newGame();
    gameState_ = GameState::startingPosition();
}

void UciFrontEnd::handlePosition(std::stringstream& lineSStream) {
    waitForGoToComplete();

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
        writeDebug(debugMode_, "Unrecognized token '{}'. Expected 'moves'.", token);
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

    writeDebug(/*debugToUci =*/false, "Position:\n{}", gameState_.toVisualString());
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

    waitForGoToComplete();
    MY_ASSERT(!goFuture.valid());

    goFuture = std::async(std::launch::async, [=, this] {
        const auto searchInfo = engine_.findMove(gameState_, timeBudget);

        writeUci("bestmove {}", moveToUciString(searchInfo.principalVariation[0]));
        std::flush(std::cout);
    });
}

void UciFrontEnd::handleStop() {
    if (goFuture.valid()) {
        engine_.interruptSearch();
        goFuture.get();
    }
}

void UciFrontEnd::handleDebug(std::stringstream& lineSStream) {
    std::string debugSettingString;
    lineSStream >> debugSettingString;

    if (debugSettingString == "on") {
        debugMode_ = true;
    } else if (debugSettingString == "off") {
        debugMode_ = false;
    } else {
        writeDebug(
                debugMode_,
                "Unknown debug setting '{}'. Expected 'on' or 'off'.",
                debugSettingString);
    }

    std::stringstream debugModeSS;
    debugModeSS << std::boolalpha << debugMode_;
    writeDebug(debugMode_, "Debug mode enabled: {}", debugModeSS.str());
}

void UciFrontEnd::handleRegister() const {
    writeUci("info string No registration is needed!");
    writeUci("registration checking");
    writeUci("registration ok");
    std::flush(std::cout);
}

void UciFrontEnd::handleSetOption(const std::string& line) {
    const std::string nameLiteral  = "name";
    const auto nameLiteralPosition = line.find(nameLiteral);
    const auto nameStart           = nameLiteralPosition + nameLiteral.size() + 1;

    if (nameLiteralPosition == std::string::npos) {
        writeDebug(
                debugMode_,
                "Failed to find expected token '{}' in the following string: '{}'",
                nameLiteral,
                line);
        return;
    }

    const std::string valueLiteral  = "value";
    const auto valueLiteralPosition = line.find(valueLiteral);
    const auto valueStart           = valueLiteralPosition + valueLiteral.size() + 1;

    int nameLength;
    if (valueLiteralPosition == std::string::npos) {
        nameLength = (int)line.size() - (int)nameStart;
    } else {
        nameLength = (int)valueLiteralPosition - (int)nameStart - 1;
    }
    if (nameLength <= 0) {
        writeDebug(debugMode_, "Failed to find option name in the following string: '{}'", line);
        return;
    }
    const std::string optionName = line.substr(nameStart, nameLength);

    auto it = optionsMap_.find(optionName);
    if (it == optionsMap_.end()) {
        writeDebug(debugMode_, "Unknown option '{}'", optionName);
        return;
    }

    FrontEndOption& option = it->second;

    if (option.getType() == FrontEndOption::Type::Action) {
        writeDebug(debugMode_, "Action option '{}' triggered", optionName);
        option.getOnSet()("");
        return;
    }

    if (valueLiteralPosition == std::string::npos) {
        writeDebug(
                debugMode_,
                "Option '{}' is not a button. Failed to find expected token '{}' in the following "
                "string: '{}'",
                optionName,
                valueLiteral,
                line);
        return;
    }

    const std::string valueString = line.substr(valueStart);
    if (valueString.empty()) {
        if (option.getType() == FrontEndOption::Type::String) {
            writeDebug(debugMode_, "Setting option '{}' to empty string.", optionName);
            option.getOnSet()("");
        } else {
            writeDebug(
                    debugMode_, "Failed to find option value in the following string: '{}'", line);
        }
        return;
    }

    writeDebug(debugMode_, "Setting option '{}' to '{}'", optionName, valueString);
    option.getOnSet()(valueString);
}

void UciFrontEnd::waitForGoToComplete() {
    if (goFuture.valid()) {
        goFuture.get();
    }
}

void UciFrontEnd::writeOptions() const {
    for (const auto& [name, option] : optionsMap_) {
        switch (option.getType()) {
            case FrontEndOption::Type::Action: {
                writeUci("option name {} type button", name);
                break;
            }

            case FrontEndOption::Type::Boolean: {
                writeUci("option name {} type check default {}", name, *option.getDefaultValue());
                break;
            }

            case FrontEndOption::Type::String: {
                writeUci("option name {} type string default {}", name, *option.getDefaultValue());
                break;
            }

            case FrontEndOption::Type::Integer: {
                writeUci(
                        "option name {} type spin default {} min {} max {}",
                        name,
                        *option.getDefaultValue(),
                        *option.getMinValue(),
                        *option.getMaxValue());
                break;
            }

            case FrontEndOption::Type::Alternative: {
                const std::vector<std::string> validValues = *option.getValidValues();
                const std::string varsString =
                        validValues
                        | std::views::transform([](auto v) { return std::format("var {}", v); })
                        | std::views::join_with(' ') | std::ranges::to<std::string>();
                writeUci(
                        "option name {} type combo default {} {}",
                        name,
                        *option.getDefaultValue(),
                        varsString);
                break;
            }

            default: {
                UNREACHABLE;
            }
        }
    }
}
