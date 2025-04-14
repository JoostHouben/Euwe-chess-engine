#include "UciFrontEnd.h"

#include "ConsoleColor.h"
#include "Eval.h"
#include "GameState.h"
#include "Math.h"
#include "MyAssert.h"
#include "RangePatches.h"

#include <future>
#include <iostream>
#include <map>
#include <optional>
#include <print>
#include <ranges>
#include <sstream>

#include <cctype>
#include <cmath>

namespace {

std::string moveListToString(const std::vector<Move>& moves) {
    return moves | std::views::transform(&Move::toUci) | joinToString(" ");
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
    return str | std::views::transform([](unsigned char c) { return (char)std::tolower(c); })
         | range_to<std::string>();
}

}  // namespace

class UciFrontEnd::Impl final : public IFrontEnd {
  public:
    Impl(IEngine& engine,
         std::string name,
         std::istream& in,
         std::ostream& out,
         std::ostream& debug);
    ~Impl() override;

    Impl(const Impl&)            = delete;
    Impl& operator=(const Impl&) = delete;

    Impl(Impl&&)            = delete;
    Impl& operator=(Impl&&) = delete;

    void run() override;

    void reportFullSearch(const SearchInfo& searchInfo) const override;

    void reportPartialSearch(const SearchInfo& searchInfo) const override;

    void reportSearchStatistics(const SearchStatistics& searchStatistics) const override;

    void reportAspirationWindowReSearch(
            int depth,
            EvalT previousLowerBound,
            EvalT previousUpperBound,
            EvalT searchEval,
            EvalT newLowerBound,
            EvalT newUpperBound,
            const SearchStatistics& searchStatistics) const override;

    void reportDiscardedPv(std::string_view reason) const override;

    void reportError(std::string_view message) const override;

    void reportString(std::string_view message) const override;

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

    // Non-UCI commands
    void handleEval();
    void handleListMoves();

    void stopSearchIfNeeded();

    void waitForGoToComplete();

    void writeOptions() const;

    std::optional<OptionStringParseResult> parseOptionLine(std::string_view line) const;

    template <typename... Args>
    void reportError(std::format_string<Args...> fmt, Args&&... args) const;

    template <typename... Args>
    void writeUci(std::format_string<Args...> fmt, Args&&... args) const;

    template <typename... Args>
    void writeDebug(std::format_string<Args...> fmt, Args&&... args) const;

    template <typename... Args>
    void writeDebugNonUci(std::format_string<Args...> fmt, Args&&... args) const;

    IEngine& engine_;

    std::string name_;

    std::istream& in_;
    std::ostream& out_;
    std::ostream& debug_;

    GameState gameState_;

    bool debugMode_ = false;

    std::map<std::string, FrontEndOption, std::less<>> optionsMap_;

    std::future<void> goFuture_;
};

UciFrontEnd::Impl::Impl(
        IEngine& engine, std::string name, std::istream& in, std::ostream& out, std::ostream& debug)
    : engine_(engine),
      name_(std::move(name)),
      in_(in),
      out_(out),
      debug_(debug),
      gameState_(GameState::startingPosition()) {
    engine_.setFrontEnd(this);

    // Add UCI hard-coded options
    addOption(FrontEndOption::createInteger(
            "Hash",
            engine_.getDefaultTTableSizeInMb(),
            0,
            1 * 1024 * 1024,
            [this](const int requestedSizeInMb) { engine_.setTTableSize(requestedSizeInMb); }));
}

UciFrontEnd::Impl::~Impl() {
    MY_ASSERT(!goFuture_.valid());
}

void UciFrontEnd::Impl::run() {
    writeUci("id name {}", name_);
    writeUci("id author Joost Houben");

    writeOptions();

    writeUci("uciok");
    std::flush(out_);

    while (in_.good()) {
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
            break;
        } else if (command == "register") {
            handleRegister();
        } else if (command == "setoption") {
            handleSetOption(inputLine);
        } else if (command == "eval") {
            handleEval();
        } else if (command == "listmoves") {
            handleListMoves();
        } else if (command.empty()) {
            continue;
        } else {
            writeDebug("Warning: Ignoring unknown command: '{}'", command);
        }
    }

    stopSearchIfNeeded();
}

void UciFrontEnd::Impl::reportFullSearch(const SearchInfo& searchInfo) const {
    std::string optionalScoreString = "";
    if (isValid(searchInfo.score)) {
        optionalScoreString = std::format(" score {}", scoreToString(searchInfo.score));
    }

    std::string optionalTbHitsString = "";
    if (searchInfo.statistics.tbHits) {
        optionalTbHitsString = std::format(" tbhits {}", *searchInfo.statistics.tbHits);
    }

    std::string optionalNpsString = "";
    if (searchInfo.statistics.timeElapsed.count() > 0) {
        optionalNpsString =
                std::format(" nps {}", (int)std::round(searchInfo.statistics.nodesPerSecond));
    }

    const std::string pvString = moveListToString(searchInfo.principalVariation);

    writeUci(
            "info depth {} seldepth {}{} nodes {}{} time {}{} hashfull {} pv {}",
            searchInfo.depth,
            searchInfo.statistics.selectiveDepth,
            optionalScoreString,
            searchInfo.statistics.normalNodesSearched + searchInfo.statistics.qNodesSearched,
            optionalTbHitsString,
            searchInfo.statistics.timeElapsed.count(),
            optionalNpsString,
            (int)std::round(searchInfo.statistics.ttableUtilization * 1000),
            pvString);
    std::flush(out_);
}

void UciFrontEnd::Impl::reportPartialSearch(const SearchInfo& searchInfo) const {
    writeDebug("Completed partial search of depth {}", searchInfo.depth);

    SearchInfo completedSearchInfo = searchInfo;
    completedSearchInfo.depth      = searchInfo.depth - 1;

    reportFullSearch(completedSearchInfo);
}

void UciFrontEnd::Impl::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    if (debugMode_) {
        writeDebug("Normal nodes searched: {}", searchStatistics.normalNodesSearched);
        writeDebug("Quiescence nodes searched: {}", searchStatistics.qNodesSearched);
        writeDebug("TTable hits: {}", searchStatistics.tTableHits);
        writeDebug("TTable utilization: {:.1f}%", searchStatistics.ttableUtilization * 100.f);
    }
}

void UciFrontEnd::Impl::reportAspirationWindowReSearch(
        const int depth,
        const EvalT previousLowerBound,
        const EvalT previousUpperBound,
        const EvalT searchEval,
        const EvalT newLowerBound,
        const EvalT newUpperBound,
        const SearchStatistics& searchStatistics) const {
    if (debugMode_) {
        writeDebug(
                "Aspiration window [{}, {}] failed (search returned {}); re-searching with "
                "window "
                "[{}, "
                "{}]",
                previousLowerBound,
                previousUpperBound,
                searchEval,
                newLowerBound,
                newUpperBound);
    }

    std::string optionalTbHitsString = "";
    if (searchStatistics.tbHits) {
        optionalTbHitsString = std::format(" tbhits {}", *searchStatistics.tbHits);
    }

    std::string optionalNpsString = "";
    if (searchStatistics.timeElapsed.count() > 0) {
        optionalNpsString =
                std::format(" nps {}", (int)std::round(searchStatistics.nodesPerSecond));
    }

    writeUci(
            "info depth {} seldepth {} score {} {} nodes {}{} time {}{} hashfull {}",
            depth,
            searchStatistics.selectiveDepth,
            scoreToString(searchEval),
            searchEval <= previousLowerBound ? "upperbound" : "lowerbound",
            searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched,
            optionalTbHitsString,
            searchStatistics.timeElapsed.count(),
            optionalNpsString,
            (int)std::round(searchStatistics.ttableUtilization * 1000));
    std::flush(out_);
}

void UciFrontEnd::Impl::reportDiscardedPv(std::string_view reason) const {
    writeDebug("Discarded PV: {}", reason);
}

void UciFrontEnd::Impl::reportError(std::string_view message) const {
    writeUci("info string Error: {}", message);
}

void UciFrontEnd::Impl::reportString(std::string_view message) const {
    writeUci("info string {}", message);
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

        try {
            gameState_ = GameState::fromFen(fen);
        } catch (const std::exception& e) {
            reportError("Failed to parse FEN: {}", e.what());
            return;
        }
    }

    // Allow for the 'moves' token to be omitted at the end of the line.
    // This allows things like 'position startpos' as a short-hand for 'position startpos moves'.
    // While this behavior isn't specified in the original UCI protocol, it seems to be common
    // practice in many UCI GUIs and engines.
    if (lineSStream && token != "moves") {
        reportError("Unrecognized token '{}'. Expected 'moves'.", token);
        return;
    }

    while (lineSStream) {
        std::string moveString;
        lineSStream >> moveString;
        if (moveString.empty()) {
            break;
        }

        try {
            const Move move = Move::fromUci(moveString, gameState_);
            doBasicSanityChecks(move, gameState_);

            (void)gameState_.makeMove(move);
        } catch (const std::exception& e) {
            reportError("Failed to parse or apply move '{}': {}", moveString, e.what());
            return;
        }
    }

    if (debugMode_) {
        writeDebug("FEN: {}", gameState_.toFen());
        writeDebugNonUci("Position:\n{}", gameState_.toVisualString());
    }
}

void UciFrontEnd::Impl::handleGo(std::stringstream& lineSStream) {
    // Not implemented: ponder, mate

    const std::string ourTimeString = gameState_.getSideToMove() == Side::White ? "wtime" : "btime";
    const std::string ourIncString  = gameState_.getSideToMove() == Side::White ? "winc" : "binc";

    std::optional<std::chrono::milliseconds> timeLeft      = std::nullopt;
    std::optional<std::chrono::milliseconds> timeIncrement = std::nullopt;
    std::optional<int> movesToGo                           = std::nullopt;
    std::optional<int> depth                               = std::nullopt;
    std::optional<std::uint64_t> nodes                     = std::nullopt;
    std::optional<std::chrono::milliseconds> fixedTime     = std::nullopt;
    bool isInfinite                                        = false;

    std::vector<Move> searchMoves;

    std::string excessToken;
    while (lineSStream) {
        std::string token;
        if (!excessToken.empty()) {
            token = excessToken;
            excessToken.clear();
        } else if (!(lineSStream >> token)) {
            break;
        }

        if (token == ourIncString) {
            int incMs{};
            if (lineSStream >> incMs) {
                timeIncrement = std::chrono::milliseconds(incMs);
            }
        } else if (token == ourTimeString) {
            int timeMs{};
            if (lineSStream >> timeMs) {
                timeLeft = std::chrono::milliseconds(timeMs);
            }
        } else if (token == "movestogo") {
            int movesToGoVal{};
            if (lineSStream >> movesToGoVal) {
                movesToGo = movesToGoVal;
            }
        } else if (token == "depth") {
            int depthVal{};
            if (lineSStream >> depthVal) {
                depth = depthVal;
            }
        } else if (token == "nodes") {
            std::uint64_t nodesVal{};
            if (lineSStream >> nodesVal) {
                nodes = nodesVal;
            }
        } else if (token == "movetime") {
            int timeMs{};
            if (lineSStream >> timeMs) {
                fixedTime = std::chrono::milliseconds(timeMs);
            }
        } else if (token == "infinite") {
            isInfinite = true;
        } else if (token == "searchmoves") {
            std::array specialTokens = {
                    "ponder",
                    "wtime",
                    "btime",
                    "winc",
                    "binc",
                    "movestogo",
                    "depth",
                    "nodes",
                    "mate",
                    "movetime",
                    "infinite"};

            while (lineSStream >> token) {
                if (std::ranges::contains(specialTokens, token)) {
                    excessToken = token;
                    break;
                }

                try {
                    const Move move = Move::fromUci(token, gameState_);
                    doBasicSanityChecks(move, gameState_);

                    searchMoves.push_back(move);
                } catch (const std::exception& e) {
                    reportError("Failed to parse search move '{}': {}", token, e.what());
                    return;
                }
            }
        }
    }

    waitForGoToComplete();

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
        reportError("no time control specified. Defaulting to fixed 1 second search.");
        timeManager.configureForFixedTimeSearch(std::chrono::seconds(1));
    }

    MY_ASSERT(!goFuture_.valid());

    goFuture_ = std::async(std::launch::async, [searchMoves, this] {
        try {
            const auto searchInfo = engine_.findMove(gameState_, searchMoves);

            if (searchInfo.principalVariation.empty()) {
                reportError("No principal variation found.");
                std::abort();
            }

            writeUci("bestmove {}", searchInfo.principalVariation[0].toUci());
            std::flush(out_);
        } catch (const std::exception& e) {
            reportError(e.what());
        }
    });
}

void UciFrontEnd::Impl::handleStop() {
    stopSearchIfNeeded();
}

void UciFrontEnd::Impl::handleDebug(std::stringstream& lineSStream) {
    std::string debugSettingString;
    lineSStream >> debugSettingString;

    if (debugSettingString == "on") {
        debugMode_ = true;
    } else if (debugSettingString == "off") {
        debugMode_ = false;
    } else {
        reportError("Unknown debug setting '{}'. Expected 'on' or 'off'.", debugSettingString);
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
        reportError("Unknown option '{}'", optionParseResult->optionName);
        return;
    }
    FrontEndOption& option = it->second;

    if (option.getType() == FrontEndOption::Type::Action) {
        if (optionParseResult->optionValue.has_value()) {
            writeDebug(
                    "Warning: Option '{}' is a button. Expected no value, but found '{}'. "
                    "Ignoring "
                    "this value.",
                    option.getName(),
                    *optionParseResult->optionValue);
        }

        try {
            option.trigger();
            writeUci("info string Action option '{}' was triggered.", option.getName());
        } catch (const std::exception& e) {
            reportError("Failed to trigger action option '{}': {}", option.getName(), e.what());
        }
        return;
    }

    if (!optionParseResult->optionValue.has_value()) {
        reportError(
                "Option '{}' is not a button. Failed to find value in the following "
                "string: "
                "'{}'",
                option.getName(),
                line);
        return;
    }

    if (optionParseResult->optionValue->empty()) {
        if (option.getType() != FrontEndOption::Type::String) {
            reportError(
                    "Failed to find non-empty option value for non-string option '{}' "
                    "in "
                    "the following string: '{}'",
                    option.getName(),
                    line);
            return;
        }

        try {
            option.set("");
            writeUci("info string Option '{}' was set to empty string.", option.getName());
        } catch (const std::exception& e) {
            reportError(
                    "Failed to set option '{}' to empty string: {}", option.getName(), e.what());
        }
        return;
    }

    try {
        option.set(*optionParseResult->optionValue);
        writeUci(
                "info string Option '{}' was set to '{}'.",
                option.getName(),
                *optionParseResult->optionValue);
    } catch (const std::exception& e) {
        reportError(
                "Failed to set option '{}' to '{}': {}",
                option.getName(),
                *optionParseResult->optionValue,
                e.what());
    }
}

void UciFrontEnd::Impl::handleEval() {
    StackOfVectors<Move> stack;
    const EvalT eval = engine_.evaluate(gameState_);
    writeDebug("Eval: {:+}", (float)eval / 100);
}

void UciFrontEnd::Impl::handleListMoves() {
    StackOfVectors<Move> stack;
    const auto moves = gameState_.generateMoves(stack);
    std::vector<Move> movesVector(moves.begin(), moves.end());
    writeDebug("Moves: {}", moveListToString(movesVector));
}

void UciFrontEnd::Impl::stopSearchIfNeeded() {
    if (goFuture_.valid()) {
        engine_.interruptSearch();
        goFuture_.get();
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
                writeUci(
                        "option name {} type check default {}",
                        name,
                        option.retrieveDefaultValue());
                break;
            }

            case FrontEndOption::Type::String: {
                writeUci(
                        "option name {} type string default {}",
                        name,
                        option.retrieveDefaultValue());
                break;
            }

            case FrontEndOption::Type::Integer: {
                writeUci(
                        "option name {} type spin default {} min {} max {}",
                        name,
                        option.retrieveDefaultValue(),
                        option.retrieveMinValue(),
                        option.retrieveMaxValue());
                break;
            }

            case FrontEndOption::Type::Alternative: {
                const std::string varsString =
                        option.retrieveValidValues()
                        | std::views::transform([](auto v) { return std::format("var {}", v); })
                        | joinToString(" ");
                writeUci(
                        "option name {} type combo default {} {}",
                        name,
                        option.retrieveDefaultValue(),
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
        reportError(
                "Failed to find expected token '{}' in the following string: '{}'",
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
        reportError("Failed to find option name in the following string: '{}'", line);
        return std::nullopt;
    }

    OptionStringParseResult result;
    result.optionName = line.substr(nameStart, nameLength);

    if (foundValueLiteral) {
        const auto valueStart = valueLiteralPosition + valueLiteral.size() + 1;
        if (valueStart > line.size()) {
            result.optionValue = "";
        } else {
            result.optionValue = line.substr(valueStart);
        }
    }

    return result;
}

template <typename... Args>
void UciFrontEnd::Impl::reportError(const std::format_string<Args...> fmt, Args&&... args) const {
    reportError(std::format(fmt, std::forward<Args>(args)...));
}

template <typename... Args>
void UciFrontEnd::Impl::writeUci(const std::format_string<Args...> fmt, Args&&... args) const {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Green, out_);

    std::println(out_, fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void UciFrontEnd::Impl::writeDebug(const std::format_string<Args...> fmt, Args&&... args) const {
    if (debugMode_) {
        ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow, out_);
        std::println(out_, "info string {}", std::format(fmt, std::forward<Args>(args)...));
    } else {
        ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow, debug_);
        std::println(debug_, "[DEBUG] {}", std::format(fmt, std::forward<Args>(args)...));
    }
}

template <typename... Args>
void UciFrontEnd::Impl::writeDebugNonUci(
        const std::format_string<Args...> fmt, Args&&... args) const {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow, debug_);

    std::println(debug_, "[DEBUG] {}", std::format(fmt, std::forward<Args>(args)...));
}

// Implementation of interface: forward to implementation

UciFrontEnd::UciFrontEnd(
        IEngine& engine, std::string name, std::istream& in, std::ostream& out, std::ostream& debug)
    : impl_(std::make_unique<Impl>(engine, std::move(name), in, out, debug)) {}

UciFrontEnd::~UciFrontEnd() = default;

void UciFrontEnd::run() {
    impl_->run();
}

void UciFrontEnd::reportFullSearch(const SearchInfo& searchInfo) const {
    impl_->reportFullSearch(searchInfo);
}

void UciFrontEnd::reportPartialSearch(const SearchInfo& searchInfo) const {
    impl_->reportPartialSearch(searchInfo);
}

void UciFrontEnd::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    impl_->reportSearchStatistics(searchStatistics);
}

void UciFrontEnd::reportAspirationWindowReSearch(
        const int depth,
        const EvalT previousLowerBound,
        const EvalT previousUpperBound,
        const EvalT searchEval,
        const EvalT newLowerBound,
        const EvalT newUpperBound,
        const SearchStatistics& searchStatistics) const {
    impl_->reportAspirationWindowReSearch(
            depth,
            previousLowerBound,
            previousUpperBound,
            searchEval,
            newLowerBound,
            newUpperBound,
            searchStatistics);
}

void UciFrontEnd::reportDiscardedPv(std::string_view reason) const {
    impl_->reportDiscardedPv(reason);
}

void UciFrontEnd::reportError(std::string_view message) const {
    impl_->reportError(message);
}

void UciFrontEnd::reportString(std::string_view message) const {
    impl_->reportString(message);
}

void UciFrontEnd::reportDebugString(std::string_view message) const {
    impl_->reportDebugString(message);
}

void UciFrontEnd::addOption(FrontEndOption option) {
    impl_->addOption(std::move(option));
}
