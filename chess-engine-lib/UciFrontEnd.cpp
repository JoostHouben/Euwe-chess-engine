#include "UciFrontEnd.h"

#include "ConsoleColor.h"
#include "Engine.h"
#include "GameState.h"
#include "Math.h"
#include "MyAssert.h"

#include <future>
#include <iostream>
#include <map>
#include <optional>
#include <print>
#include <ranges>
#include <sstream>

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

struct OptionStringParseResult {
    std::string_view optionName;
    std::optional<std::string_view> optionValue;
};

}  // namespace

class UciFrontEnd::Impl final : public IFrontEnd {
  public:
    Impl(IEngine& engine, std::istream& in, std::ostream& out, std::ostream& debug);
    ~Impl();

    void run() override;

    void reportFullSearch(
            const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const override;

    void reportPartialSearch(
            const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const override;

    void reportSearchStatistics(const SearchStatistics& searchStatistics) const override;

    void reportAspirationWindowReSearch(
            EvalT previousLowerBound,
            EvalT previousUpperBound,
            EvalT searchEval,
            EvalT newLowerBound,
            EvalT newUpperBound) const override;

    void reportDiscardedPv(std::string_view reason) const override;

    void addOption(std::string name, FrontEndOption option) override;

  private:
    void handleIsReady();
    void handleNewGame();
    void handlePosition(std::stringstream& lineSStream);
    void handleGo(std::stringstream& lineSStream);
    void handleStop();
    void handleDebug(std::stringstream& lineSStream);
    void handleRegister() const;
    void handleSetOption(const std::string& line);

    void waitForGoToComplete();

    void writeOptions() const;

    std::optional<OptionStringParseResult> parseOptionLine(std::string_view line) const;

    template <typename... Args>
    void writeUci(std::format_string<Args...> fmt, Args&&... args) const;

    template <typename... Args>
    void writeDebug(std::format_string<Args...> fmt, Args&&... args) const;

    template <typename... Args>
    void writeDebugNonUci(std::format_string<Args...> fmt, Args&&... args) const;

    IEngine& engine_;

    std::istream& in_;
    std::ostream& out_;
    std::ostream& debug_;

    GameState gameState_;

    bool debugMode_ = false;

    std::map<std::string, FrontEndOption, std::less<>> optionsMap_;

    std::future<void> goFuture;
};

UciFrontEnd::Impl::Impl(IEngine& engine, std::istream& in, std::ostream& out, std::ostream& debug)
    : engine_(engine),
      in_(in),
      out_(out),
      debug_(debug),
      gameState_(GameState::startingPosition()) {
    engine_.setFrontEnd(this);

    // Add UCI hard-coded options
    addOption(
            "Hash",
            FrontEndOption::createInteger(
                    0, 0, 1 * 1024 * 1024, [this](const int requestedSizeInMb) {
                        engine_.setTTableSize(requestedSizeInMb);
                    }));
}

UciFrontEnd::Impl::~Impl() {
    MY_ASSERT(!goFuture.valid());
}

void UciFrontEnd::Impl::run() {
    writeUci("id name abstract-front-end");
    writeUci("id author Joost Houben");

    writeOptions();

    writeUci("uciok");
    std::flush(out_);

    while (true) {
        std::string inputLine;
        std::getline(in_, inputLine);

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
            writeDebug("Warning: Ignoring unknown command: '{}'", command);
        }
    }
}

void UciFrontEnd::Impl::reportFullSearch(
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

void UciFrontEnd::Impl::reportPartialSearch(
        const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const {
    writeDebug("Completed partial search of depth {}", searchInfo.depth);

    SearchInfo completedSearchInfo = searchInfo;
    completedSearchInfo.depth      = searchInfo.depth - 1;

    reportFullSearch(completedSearchInfo, searchStatistics);
}

void UciFrontEnd::Impl::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    writeDebug("Normal nodes searched: {}", searchStatistics.normalNodesSearched);
    writeDebug("Quiescence nodes searched: {}", searchStatistics.qNodesSearched);
    writeDebug("TTable hits: {}", searchStatistics.tTableHits);
    writeDebug("TTable utilization: {:.1f}%", searchStatistics.ttableUtilization * 100.f);
}

void UciFrontEnd::Impl::reportAspirationWindowReSearch(
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

void UciFrontEnd::Impl::reportDiscardedPv(std::string_view reason) const {
    writeDebug("Discarded PV: {}", reason);
}

void UciFrontEnd::Impl::addOption(std::string name, FrontEndOption option) {
    optionsMap_.emplace(std::move(name), std::move(option));
}

void UciFrontEnd::Impl::handleIsReady() {
    waitForGoToComplete();
    writeUci("readyok");
    std::flush(out_);
}

void UciFrontEnd::Impl::handleNewGame() {
    engine_.newGame();
    gameState_ = GameState::startingPosition();
}

void UciFrontEnd::Impl::handlePosition(std::stringstream& lineSStream) {
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
        writeDebug("Error: Unrecognized token '{}'. Expected 'moves'.", token);
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

    if (debugMode_) {
        writeDebugNonUci("Position:\n{}", gameState_.toVisualString());
    }
}

void UciFrontEnd::Impl::handleGo(std::stringstream& lineSStream) {
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
        std::flush(out_);
    });
}

void UciFrontEnd::Impl::handleStop() {
    if (goFuture.valid()) {
        engine_.interruptSearch();
        goFuture.get();
    }
}

void UciFrontEnd::Impl::handleDebug(std::stringstream& lineSStream) {
    std::string debugSettingString;
    lineSStream >> debugSettingString;

    if (debugSettingString == "on") {
        debugMode_ = true;
    } else if (debugSettingString == "off") {
        debugMode_ = false;
    } else {
        writeDebug(

                "Error: Unknown debug setting '{}'. Expected 'on' or 'off'.", debugSettingString);
    }

    std::stringstream debugModeSS;
    debugModeSS << std::boolalpha << debugMode_;
    writeDebug("Debug mode enabled: {}", debugModeSS.str());
}

void UciFrontEnd::Impl::handleRegister() const {
    writeUci("info string No registration is needed!");
    writeUci("registration checking");
    writeUci("registration ok");
    std::flush(out_);
}

void UciFrontEnd::Impl::handleSetOption(const std::string& line) {
    const auto optionParseResult = parseOptionLine(line);
    if (!optionParseResult.has_value()) {
        return;
    }

    const auto it = optionsMap_.find(optionParseResult->optionName);
    if (it == optionsMap_.end()) {
        writeDebug("Error: Unknown option '{}'", optionParseResult->optionName);
        return;
    }
    FrontEndOption& option = it->second;

    if (option.getType() == FrontEndOption::Type::Action) {
        if (optionParseResult->optionValue.has_value()) {
            writeDebug(

                    "Warning: Option '{}' is a button. Expected no value, but found '{}'. Ignoring "
                    "this value.",
                    optionParseResult->optionName,
                    *optionParseResult->optionValue);
        }

        try {
            option.trigger();
            writeDebug("Action option '{}' triggered", optionParseResult->optionName);
        } catch (const std::exception& e) {
            writeDebug(

                    "Error: Failed to trigger action option '{}': {}",
                    optionParseResult->optionName,
                    e.what());
        }
        return;
    }

    if (!optionParseResult->optionValue.has_value()) {
        writeDebug(

                "Error: Option '{}' is not a button. Failed to find value in the following string: "
                "'{}'",
                optionParseResult->optionName,
                line);
        return;
    }

    if (optionParseResult->optionValue->empty()) {
        if (option.getType() != FrontEndOption::Type::String) {
            writeDebug(

                    "Error: Failed to find non-empty option value for non-string option '{}' in "
                    "the following string: '{}'",
                    optionParseResult->optionName,
                    line);
            return;
        }

        try {
            option.set("");
            writeDebug("Set option '{}' to empty string.", optionParseResult->optionName);
        } catch (const std::exception& e) {
            writeDebug(

                    "Error: Failed to set option '{}' to empty string: {}",
                    optionParseResult->optionName,
                    e.what());
        }
        return;
    }

    try {
        option.set(*optionParseResult->optionValue);
        writeDebug(

                "Set option '{}' to '{}'",
                optionParseResult->optionName,
                *optionParseResult->optionValue);
    } catch (const std::exception& e) {
        writeDebug(

                "Error: Failed to set option '{}' to '{}': {}",
                optionParseResult->optionName,
                *optionParseResult->optionValue,
                e.what());
    }
}

void UciFrontEnd::Impl::waitForGoToComplete() {
    if (goFuture.valid()) {
        goFuture.get();
    }
}

void UciFrontEnd::Impl::writeOptions() const {
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

std::optional<OptionStringParseResult> UciFrontEnd::Impl ::parseOptionLine(
        std::string_view line) const {
    static constexpr std::string_view nameLiteral = "name";
    const auto nameLiteralPosition                = line.find(nameLiteral);
    const bool foundNameLiteral                   = nameLiteralPosition != std::string_view::npos;

    if (!foundNameLiteral) {
        writeDebug(
                "Error: Failed to find expected token '{}' in the following string: '{}'",
                nameLiteral,
                line);
        return std::nullopt;
    }

    const auto nameStart = nameLiteralPosition + nameLiteral.size() + 1;

    static constexpr std::string_view valueLiteral = "value";
    const auto valueLiteralPosition                = line.find(valueLiteral);
    const bool foundValueLiteral                   = valueLiteralPosition != std::string_view::npos;

    const int nameLength = foundValueLiteral ? (int)valueLiteralPosition - (int)nameStart - 1
                                             : (int)line.size() - (int)nameStart;
    if (nameLength <= 0) {
        writeDebug("Error: Failed to find option name in the following string: '{}'", line);
        return std::nullopt;
    }

    OptionStringParseResult result;
    result.optionName = line.substr(nameStart, nameLength);

    if (foundValueLiteral) {
        const auto valueStart = valueLiteralPosition + valueLiteral.size() + 1;
        result.optionValue    = line.substr(valueStart);
    }

    return result;
}

template <typename... Args>
void UciFrontEnd::Impl::writeUci(const std::format_string<Args...> fmt, Args&&... args) const {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Green);

    std::println(out_, fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void UciFrontEnd::Impl::writeDebug(const std::format_string<Args...> fmt, Args&&... args) const {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow);

    if (debugMode_) {
        std::println(out_, "info string {}", std::format(fmt, std::forward<Args>(args)...));
    } else {
        std::println(debug_, "[DEBUG] {}", std::format(fmt, std::forward<Args>(args)...));
    }
}

template <typename... Args>
void UciFrontEnd::Impl::writeDebugNonUci(
        const std::format_string<Args...> fmt, Args&&... args) const {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow);

    std::println(debug_, "[DEBUG] {}", std::format(fmt, std::forward<Args>(args)...));
}

// Implementation of interface: forward to implementation

UciFrontEnd::UciFrontEnd(IEngine& engine, std::istream& in, std::ostream& out, std::ostream& debug)
    : impl_(std::make_unique<Impl>(engine, in, out, debug)) {}

UciFrontEnd::~UciFrontEnd() = default;

void UciFrontEnd::run() {
    impl_->run();
}

void UciFrontEnd::reportFullSearch(
        const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const {
    impl_->reportFullSearch(searchInfo, searchStatistics);
}

void UciFrontEnd::reportPartialSearch(
        const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const {
    impl_->reportPartialSearch(searchInfo, searchStatistics);
}

void UciFrontEnd::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    impl_->reportSearchStatistics(searchStatistics);
}

void UciFrontEnd::reportAspirationWindowReSearch(
        EvalT previousLowerBound,
        EvalT previousUpperBound,
        EvalT searchEval,
        EvalT newLowerBound,
        EvalT newUpperBound) const {
    impl_->reportAspirationWindowReSearch(
            previousLowerBound, previousUpperBound, searchEval, newLowerBound, newUpperBound);
}

void UciFrontEnd::reportDiscardedPv(std::string_view reason) const {
    impl_->reportDiscardedPv(reason);
}

void UciFrontEnd::addOption(std::string name, FrontEndOption option) {
    impl_->addOption(std::move(name), std::move(option));
}
