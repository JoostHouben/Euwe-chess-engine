#include "UciFrontEnd.h"

#include "ConsoleColor.h"
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

#include <cctype>

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

std::string stringToLower(std::string_view str) {
    return str | std::views::transform([](unsigned char c) { return std::tolower(c); })
         | std::ranges::to<std::string>();
}

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

    void reportDebugString(std::string_view message) const override;

    void addOption(FrontEndOption option) override;

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

    std::future<void> goFuture_;
};

UciFrontEnd::Impl::Impl(IEngine& engine, std::istream& in, std::ostream& out, std::ostream& debug)
    : engine_(engine),
      in_(in),
      out_(out),
      debug_(debug),
      gameState_(GameState::startingPosition()) {
    engine_.setFrontEnd(this);

    // Add UCI hard-coded options
    addOption(FrontEndOption::createInteger(
            "Hash", 0, 0, 1 * 1024 * 1024, [this](const int requestedSizeInMb) {
                engine_.setTTableSize(requestedSizeInMb);
            }));
}

UciFrontEnd::Impl::~Impl() {
    MY_ASSERT(!goFuture_.valid());
}

void UciFrontEnd::Impl::run() {
    writeUci("id name flush");
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

    std::string optionalNpsString = "";
    if (searchInfo.timeMs > 0) {
        optionalNpsString = std::format(" nps {}", (int)std::round(searchInfo.nodesPerSecond));
    }

    const std::string pvString = pvToString(searchInfo.principalVariation);

    writeUci(
            "info depth {} seldepth {} time {} nodes {}{} hashfull {}{} pv {}",
            searchInfo.depth,
            searchStatistics.selectiveDepth,
            searchInfo.timeMs,
            searchInfo.numNodes,
            optionalNpsString,
            (int)(searchStatistics.ttableUtilization * 1000),
            optionalScoreString,
            pvString);
    std::flush(out_);
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

void UciFrontEnd::Impl::reportDebugString(std::string_view message) const {
    writeDebug("{}", message);
}

void UciFrontEnd::Impl::addOption(FrontEndOption option) {
    // UCI option names are case insensitive, so convert to lower case for lookup.
    optionsMap_.emplace(stringToLower(option.getName()), std::move(option));
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
    // Not implemented: searchmoves, ponder, mate

    const std::string ourTimeString = gameState_.getSideToMove() == Side::White ? "wtime" : "btime";
    const std::string ourIncString  = gameState_.getSideToMove() == Side::White ? "winc" : "binc";

    std::optional<std::chrono::milliseconds> timeLeft      = std::nullopt;
    std::optional<std::chrono::milliseconds> timeIncrement = std::nullopt;
    std::optional<int> movesToGo                           = std::nullopt;
    std::optional<int> depth                               = std::nullopt;
    std::optional<std::uint64_t> nodes                     = std::nullopt;
    std::optional<std::chrono::milliseconds> fixedTime     = std::nullopt;
    bool isInfinite                                        = false;

    while (lineSStream) {
        std::string token;
        if (!(lineSStream >> token)) {
            break;
        }

        if (token == ourIncString) {
            int incMs;
            if (lineSStream >> incMs) {
                timeIncrement = std::chrono::milliseconds(incMs);
            }
        } else if (token == ourTimeString) {
            int timeMs;
            if (lineSStream >> timeMs) {
                timeLeft = std::chrono::milliseconds(timeMs);
            }
        } else if (token == "movestogo") {
            int movesToGoVal;
            if (lineSStream >> movesToGoVal) {
                movesToGo = movesToGoVal;
            }
        } else if (token == "depth") {
            int depthVal;
            if (lineSStream >> depthVal) {
                depth = depthVal;
            }
        } else if (token == "nodes") {
            std::uint64_t nodesVal;
            if (lineSStream >> nodesVal) {
                nodes = nodesVal;
            }
        } else if (token == "movetime") {
            int timeMs;
            if (lineSStream >> timeMs) {
                fixedTime = std::chrono::milliseconds(timeMs);
            }
        } else if (token == "infinite") {
            isInfinite = true;
        }
    }

    TimeManager& timeManager = engine_.getTimeManager();

    if (isInfinite) {
        timeManager.configureForInfiniteSearch();
    } else if (depth) {
        timeManager.configureForFixedDepthSearch(*depth);
    } else if (nodes) {
        timeManager.configureForFixedNodesSearch(*nodes);
    } else if (fixedTime) {
        timeManager.configureForFixedTimeSearch(*fixedTime);
    } else if (timeLeft) {
        timeIncrement = timeIncrement.value_or(std::chrono::milliseconds(0));
        movesToGo     = movesToGo.value_or(std::numeric_limits<int>::max());

        timeManager.configureForTimeControl(*timeLeft, *timeIncrement, *movesToGo, gameState_);
    } else {
        writeDebug("Error: no time control specified. Defaulting to fixed 1 second search.");
        timeManager.configureForFixedTimeSearch(std::chrono::seconds(1));
    }

    waitForGoToComplete();
    MY_ASSERT(!goFuture_.valid());

    goFuture_ = std::async(std::launch::async, [=, this] {
        const auto searchInfo = engine_.findMove(gameState_);

        writeUci("bestmove {}", moveToUciString(searchInfo.principalVariation[0]));
        std::flush(out_);
    });
}

void UciFrontEnd::Impl::handleStop() {
    if (goFuture_.valid()) {
        engine_.interruptSearch();
        goFuture_.get();
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

    // UCI option names are case insensitive, so convert to lower case for lookup.
    const auto it = optionsMap_.find(stringToLower(optionParseResult->optionName));
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
                    option.getName(),
                    *optionParseResult->optionValue);
        }

        try {
            option.trigger();
            writeDebug("Action option '{}' was triggered.", option.getName());
        } catch (const std::exception& e) {
            writeDebug(
                    "Error: Failed to trigger action option '{}': {}", option.getName(), e.what());
        }
        return;
    }

    if (!optionParseResult->optionValue.has_value()) {
        writeDebug(
                "Error: Option '{}' is not a button. Failed to find value in the following string: "
                "'{}'",
                option.getName(),
                line);
        return;
    }

    if (optionParseResult->optionValue->empty()) {
        if (option.getType() != FrontEndOption::Type::String) {
            writeDebug(
                    "Error: Failed to find non-empty option value for non-string option '{}' in "
                    "the following string: '{}'",
                    option.getName(),
                    line);
            return;
        }

        try {
            option.set("");
            writeDebug("Option '{}' was set to empty string.", option.getName());
        } catch (const std::exception& e) {
            writeDebug(
                    "Error: Failed to set option '{}' to empty string: {}",
                    option.getName(),
                    e.what());
        }
        return;
    }

    try {
        option.set(*optionParseResult->optionValue);
        writeDebug(

                "Option '{}' was set to '{}'.", option.getName(), *optionParseResult->optionValue);
    } catch (const std::exception& e) {
        writeDebug(
                "Error: Failed to set option '{}' to '{}': {}",
                option.getName(),
                *optionParseResult->optionValue,
                e.what());
    }
}

void UciFrontEnd::Impl::waitForGoToComplete() {
    if (goFuture_.valid()) {
        goFuture_.get();
    }
}

void UciFrontEnd::Impl::writeOptions() const {
    for (const auto& [_, option] : optionsMap_) {
        // Use the original name from the option, not the case-insensitive key.
        const std::string& name = option.getName();
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
        const EvalT previousLowerBound,
        const EvalT previousUpperBound,
        const EvalT searchEval,
        const EvalT newLowerBound,
        const EvalT newUpperBound) const {
    impl_->reportAspirationWindowReSearch(
            previousLowerBound, previousUpperBound, searchEval, newLowerBound, newUpperBound);
}

void UciFrontEnd::reportDiscardedPv(std::string_view reason) const {
    impl_->reportDiscardedPv(reason);
}

void UciFrontEnd::reportDebugString(std::string_view message) const {
    impl_->reportDebugString(message);
}

void UciFrontEnd::addOption(FrontEndOption option) {
    impl_->addOption(std::move(option));
}
