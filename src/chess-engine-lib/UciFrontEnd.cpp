#include "UciFrontEnd.h"

#include "ConsoleColor.h"
#include "EvalT.h"
#include "FrontEndOption.h"
#include "GameState.h"
#include "IEngine.h"
#include "IFrontEnd.h"
#include "Move.h"
#include "MyAssert.h"
#include "RangePatches.h"
#include "SearchInfo.h"
#include "SearchStatistics.h"
#include "Side.h"
#include "StackOfVectors.h"
#include "TimeManager.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <exception>
#include <format>
#include <functional>
#include <future>
#include <istream>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <ranges>
#include <sstream>
#include <stack>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

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
        const int mateInMoves         = mateInPly / 2;
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

std::vector<std::string> getBenchCommands() {
    std::array positions = {
            "position startpos",

            // 'Kiwipete'
            "position fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",

            // Opening positions from UHO_Lichess_4852_v1.epd
            "position fen r1b1k1nr/ppp2ppp/2np1q2/8/1bP5/P1NQ1N2/1P2PPPP/R1B1KB1R b KQkq - 0 7",
            "position fen rnbqk2r/ppp2ppp/8/b2pp3/3Pn3/2PBBN2/PP3PPP/RN1QK2R w KQkq - 0 7",
            "position fen rnbq1rk1/ppp3bp/4ppp1/3pPn2/3P1P2/2NB1N2/PPP3PP/R1BQK2R w KQ - 0 9",
            "position fen r3kb1r/pppb1ppp/2np1q1n/4p3/4P3/P1N2N1P/1PPPBPP1/R1BQK2R b KQkq - 0 7",
            "position fen rnb1k2r/ppp2ppp/1b1p1q2/1N6/2BNPPn1/8/PPP3PP/R1BQK2R w KQkq - 3 9",

            // Midgame positions
            "position fen r1bqkbn1/ppp1ppp1/2n5/7r/2Pp4/3P3P/PP2PPB1/RNBQK1NR b KQq - 0 6 moves "
            "g8f6 g2f3 h5e5 b1d2 c8d7 d2e4 a7a5 a2a3 a5a4 c1f4 f6e4 f3e4 "
            "e5c5 f4g3 e7e6 g1f3 f7f5 e4c6 d7c6 h1g1 c6f3 e2f3 b7b5 a1c1 d8d7 c4b5 d7b5 c1c5 b5c5 "
            "e1f1 e8f7 f1g2 c5b6 d1e2 c7c5 g3e5 a8a5 f3f4 f7g8 e2c2 f8d6 g2g3 a5a8 g1b1 d6e5 f4e5 "
            "b6b5",

            "position startpos moves e2e4 c7c5 c2c3 d7d5 e4d5 d8d5 d2d4 c8f5 c1e3 e7e6 b1a3 c5d4 "
            "a3b5 d5d7 d1d4 b8c6 d4d7 e8d7 g1f3 a7a6 e1c1 d7c8 b5d4 g8e7 f3g5 f5g6 d4c6 e7c6 f1d3 "
            "g6d3 g5f7 d3e4 f7h8 e4g2 h1g1",

            "position startpos moves d2d4 g8f6 c1g5 f6e4 g5f4 c7c5 f2f3 e4f6 d4d5 f6h5 f4e3 e7e5 "
            "g2g4 h5f4 e3f2 d8b6 b2b3 h7h5 e2e3 b6a5 c2c3 f4g6 g4h5 h8h5 f3f4 h5h8 f4f5 g6e7 e3e4 "
            "d7d6 d1f3 b7b5 f1d3 b8d7 g1e2 g7g6 b1d2 g6f5 a2a4 a7a6 e1g1 b5a4 e4f5 c8b7 a1a4 a5b6 "
            "c3c4 f8h6 d2e4 e8c8 b3b4 d8g8 g1h1 b6c7 f1b1 f7f6",

            "position startpos moves e2e4 c7c5 b1c3 b8c6 f1b5 g7g6 b5c6 d7c6 f2f4 f8g7 g1f3 g8f6 "
            "e1g1 e8g8 d2d3 d8c7 d1e1 c8g4 c1e3 g4f3 f1f3 b7b6 h2h3 a8d8 a2a3 f6e8 b2b4 g7d4 b4c5 "
            "b6c5 a1b1 c7a5 c3e2 d4e3 f3e3 a5e1 b1e1 e8c7 e4e5 c7b5 a3a4",

            "position startpos moves g1f3 g8f6 g2g3 b7b5 f1g2 c8b7 e1g1 c7c5 d2d3 g7g6 e2e4 d7d6 "
            "d3d4 c5d4 f3d4 a7a6 b1c3 e7e5 d4b3 b8d7 f1e1 h7h5 a2a4 b5b4 c3d5 f6d5 e4d5 a6a5 b3d4 "
            "f8g7 d4c6 d8c7 c2c3 b4c3 b2c3 d7b8 c6d4 e8g8 d4b5 c7e7 a1b1 b7a6 c3c4 b8d7 c1a3 a6b5 "
            "c4b5",

            "position fen 3rr1k1/pp1nq3/4p3/4n1p1/4N3/8/PPPQBPP1/4RRK1 b - - 1 20",
            "position fen r4rk1/p4p2/3b3p/2pp1qp1/8/2P3Q1/PP3PPP/R3R1K1 w - - 0 21",
            "position fen 3r2k1/pp3pb1/6p1/P2p3p/1B2nP1q/1R2PQ1P/1Pr1BP2/3R2K1 w - - 4 27",
            "position fen r3r1k1/1p3pp1/pq6/5bN1/3p1P1p/bP3Q1P/P2B2PK/2R1R3 w - - 1 26",
            "position fen 2r3k1/2pn1p2/pqn3p1/3p2Pp/1p1P1B1P/1P3P2/PKP5/1N1RQ3 w - - 1 26",
            "position fen 1r4k1/5p2/p1qpp1pP/3p3n/3P3P/1rPP1P2/1P1Q4/1K1NR1R1 w - - 2 26",

            // Likely drawn from 3-fold-repetition
            "position startpos moves e2e4 c7c5 g1f3 e7e6 d2d4 c5d4 f3d4 a7a6 b1c3 d8c7 f1d3 f8d6 "
            "d1g4 h7h5 g4e2 g8f6 d4f3 b8c6 h2h3 b7b5 e1g1 c6e5 f3e5 d6e5 f2f4 e5c3 b2c3 c7c3 c1d2 "
            "c3c5 g1h2 d7d6 e4e5 f6d5 c2c4 b5c4 d3c4 d6e5 f4e5 c8d7 a1c1 d7b5 a2a4 b5c4 c1c4 c5a7 "
            "f1c1 a7d7 c4c6 d7b7 d2g5 e8g8 e2h5 a8c8 c6c4 c8c4 c1c4 b7b8",

            // Adjudicated end game positions
            "position fen rn1qkbnr/ppp3pp/3pb3/4p3/4Pp2/1P1P1N1P/P1P2PP1/RNBQKB1R w KQkq - 0 6 "
            "moves d3d4 b8d7 b1d2 d8f6 f1c4 e8c8 c4e6 f6e6 e1g1 g7g5 d2c4 g8f6 "
            "d1e1 g5g4 d4d5 e6g8 h3g4 g8g4 f3h2 g4h5 f2f3 h8g8 f1f2 g8g6 c4d2 c8b8 c1a3 f8e7 c2c4 "
            "d8g8 d2f1 h5g5 g1h1 f6h5 e1d2 h5g3 h1g1 g5h4 c4c5 g6h6 c5d6 e7d6 a3d6 c7d6 a1c1 g3f1 "
            "g1f1 h4h2 d2c3 b8a8 c3c7 h2g3 f1e2 d7b6 c1c2 h6h2 e2f1 g3h4 c7d6 h2g2 d6c5",

            "position fen rn1qk2r/pp3pbp/3p1np1/2pPp3/4P1b1/2N3P1/PPP1NPBP/R1BQ1RK1 b kq - 7 8 "
            "moves e8g8 h2h3 g4d7 c1e3 b8a6 a2a3 b7b5 a3a4 b5b4 c3b5 d8b6 c2c4 "
            "b4c3 e2c3 a6b4 a4a5 b6b8 d1e2 h7h6 g1h2 h6h5 b5a3 d7c8 a3c4 c8a6 b2b3 b8d8 e2d2 a8b8 "
            "f2f4 e5f4 e3f4 h5h4 g3g4 f6h7 h2h1 a6c4 b3c4 d8e7 c3b5 b8d8 a1e1 a7a6 b5c3 g7f6 e4e5 "
            "f6e5 f4e5 d6e5 c3e4 e7c7 d5d6 c7a5 e1d1 d8d7 d2f2 f8c8 f2h4 b4c2 h4h6 a5a3 f1f3 a3a2 "
            "f3f2 a2a4 f2f7 d7f7 h6g6 g8h8 g6f7 c8f8 e4c5 f8f7 c5a4 h7f8 c4c5 c2d4 c5c6 f8e6 d1c1 "
            "d4b5",

            // Positions near SyzygyTB horizon
            "position fen rnbq1rk1/ppp3bp/4ppp1/3pPn2/3P1P2/2NB1N2/PPP3PP/R1BQK2R w KQ - 0 9 moves "
            "e1g1 b8c6 c3e2 c6b4 d3f5 e6f5 b2b3 f8e8 c1a3 a7a5 d1d2 b4c6 h2h3 b7b6 e2g3 c8a6 f1e1 "
            "d8d7 g1h2 a8d8 a1d1 d7f7 a3b2 c6b4 a2a3 b4c6 a3a4 c6b4 b2c3 b4a2 c3a1 a2b4 a1b2 f6e5 "
            "d4e5 h7h6 f3d4 c7c5 d4b5 a6b5 a4b5 d5d4 c2c3 d4c3 d2d8 c3b2 d8b6 g8h7 e5e6 f7f6 b6d6 "
            "b4c2 e1e5 f6e7 d6e7 e8e7 e5e2 c2b4 b5b6 g7d4 h3h4 b4d5 e2b2 e7e6 b2a2 d5c3 d1d4 c5d4 "
            "a2a5 e6b6 a5a7 h7g8 h4h5 c3e4 h5g6 e4g3 h2g3 b6d6 b3b4 d4d3 a7a1 d3d2 a1d1 g8g7 g3f2 "
            "g7g6 f2e3 d6b6 d1d2 b6b4 d2d6 g6h5 d6a6 b4b2 a6f6 b2b3 e3d4 b3b5 f6a6 h5g4 a6h6 g4f4",

            "position fen r1bq1rk1/pp1nppbp/2p2np1/3p4/3P4/1P2P3/PBPNQPPP/2KR1BNR w - - 6 8 moves "
            "g2g4 a7a5 g4g5 f6h5 a2a4 b7b5 a4b5 c6b5 e2b5 d7b6 b2c3 c8d7 b5c5 a5a4 c3a5 a8b8 b3a4 "
            "d7a4 f1a6 f7f6 g1f3 f6g5 f3g5 f8f6 a6d3 g7f8 c5a3 e7e5 a3a2 d8e7 d4e5 e7a3 a2a3 f8a3 "
            "c1b1 f6f2 b1a2 a3e7 g5h3 f2f8 d1b1 b8a8 b1b6 e7d8 d2b3 d8b6 a5b6 f8b8 b6c7 a4b3 a2b2 "
            "b8c8 c2b3 c8c7 h3f4 h5f4 e3f4 c7f7 h1f1 a8f8 f4f5 g6f5 b2c3 f5f4 f1f3 f8b8 c3d4 g8g7 "
            "d3f1 f7b7 d4d5 b7b3 f3f4 b8d8 d5e6 d8e8 e6d6 b3e3 f4f5 g7g6 f5f6 g6h5 f6e6 e8d8 d6c6 "
            "d8d2 f1b5 h5g5 h2h4 g5f4 e6d6 d2c2 c6d7 e3e5 b5d3 c2b2 d7c6 h7h5 d6g6 e5e3 d3a6 b2h2 "
            "g6f6 f4g3 c6d5 h2h4 f6h6 h4a4 a6c8 h5h4 h6g6 g3h2 g6c6 h4h3 c6h6 e3c3",

            "position fen rn1qkbnr/pp3ppp/2p1p3/7b/8/2N2NPP/PPPP1PB1/R1BQ1RK1 w kq - 0 9 moves "
            "d2d4 b8d7 c1f4 g8f6 d1d2 f8e7 a2a4 e8g8 f1e1 a7a5 g3g4 h5g6 f3h4 d7b6 b2b3 e7b4 h4g6 "
            "h7g6 d2d3 b6d5 c3d5 f6d5 f4d2 d8f6 g2d5 c6d5 e1e5 b4d2 d3d2 f8c8 a1c1 c8c6 g1g2 a8c8 "
            "c2c3 f6e7 e5e3 e7g5 d2b2 g5f4 e3f3 f4c7 b2d2 c7b8 d2g5 b8d6 g5d2 d6a3 c1b1 b7b6 f3g3 "
            "g6g5 g3f3 a3d6 b1e1 d6c7 e1c1 c7e7 c1e1 g7g6 e1c1 g8g7 c1e1 e7d8 e1c1 f7f6 c1e1 d8d6 "
            "f3e3 d6f4 d2b2 c8e8 b2d2 e8d8 d2e2 d8c8 e3e6 c6c3 e6b6 g7h6 b6b7 c3c2 e2e3 c2a2 b7d7 "
            "a2d2 d7f7 c8b8 e3f4 g5f4 h3h4 f6f5 e1e7 b8h8 e7d7 f5g4 f7f4 g6g5 f4g4 h8f8 g4g5 f8f2 "
            "g2g3 f2g2 g3f4 d2d4 f4e5 g2d2 d7d6 h6h7 g5h5 h7g7 d6a6 d4e4 e5d6 d2d3 a6a5 d3b3 h5g5 "
            "g7h6 g5d5 e4h4 a5a8 h4g4 a4a5 b3c3 a8e8 g4a4 e8e6 h6g7 d6e7 c3f3 d5g5 g7h7",

            // Matetrack2000.epd
            "position fen BK6/4p1N1/1pN1R1n1/3pn3/4k2P/3pPppP/3P1pr1/5bq1 w - -",
            "position fen 6Q1/4p3/N3K1B1/1p3p2/2k2pR1/1bb2nr1/3pp2B/5R2 w - -",
            "position fen 4r2n/3R3B/p2N4/2RP4/1P1k1P2/1Kn2b1r/2PN1P2/4B3 b - -",
            "position fen 3qrrnk/ppnbbppp/2p5/2PpBP2/1P1P3R/P2B2N1/6PP/R2Q2K1 w - -",
            "position fen 2n2K2/2pp4/6p1/2pB4/2pkP2B/5R1P/2PpPp2/3nb3 b - -",
            "position fen 1NbN4/1p1p4/1P1B1p2/P4Pp1/6P1/5B1p/4PP1P/4K1k1 w - -",
            "position fen 2Bn2qN/4N1P1/2n1PP1p/p1p1P3/2P1P1Rp/pP1QBR1K/p1pp2P1/kbrr2b1 b - -",
    };

    std::vector<std::string> benchCommands;

    benchCommands.push_back("startbench");
    for (const auto& position : positions) {
        benchCommands.push_back(position);
        benchCommands.push_back("fen");
        benchCommands.push_back("go depth 14");
    }
    benchCommands.push_back("stopbench");

    return benchCommands;
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

    void pushProgrammaticCommand(std::string_view command) override;

    void reportSearchHasStarted() override;

    void reportFullSearch(const SearchInfo& searchInfo) const override;

    void reportPartialSearch(const SearchInfo& searchInfo) const override;

    void reportSearchStatistics(const SearchStatistics& searchStatistics) const override;

    void reportAspirationWindowReSearch(
            const SearchInfo& searchInfo,
            EvalT previousLowerBound,
            EvalT previousUpperBound,
            EvalT newLowerBound,
            EvalT newUpperBound) const override;

    void reportDiscardedPv(std::string_view reason) const override;

    void reportError(std::string_view message) const override;

    void reportString(std::string_view message) const override;

    void reportDebugString(std::string_view message) const override;

    void addOption(FrontEndOption option) override;

  private:
    void handleUci();
    void handleIsReady();
    void handleNewGame();
    void handlePosition(std::stringstream& lineSStream);
    void handleGo(std::stringstream& lineSStream);
    void handleStop();
    void handleDebug(std::stringstream& lineSStream);
    void handleRegister() const;
    void handleSetOption(const std::string& line);

    // Non-standard extensions to UCI
    void handleEval() const;
    void handleListMoves() const;
    void handleHash() const;
    void handleFen() const;
    void handleBoard() const;
    void handleStartBench();
    void handleStopBench();
    void handleBench();

    void stopSearchIfNeeded();

    void waitForGoToComplete();

    void writeOptions() const;

    std::optional<OptionStringParseResult> parseOptionLine(std::string_view line) const;

    void reportSearchInfo(const SearchInfo& searchInfo) const;

    // Write error over UCI protocol.
    template <typename... Args>
    void writeError(std::format_string<Args...> fmt, Args&&... args) const;

    // Write UCI-compliant output.
    template <typename... Args>
    void writeUci(std::format_string<Args...> fmt, Args&&... args) const;

    // Write output that is not UCI-compliant.
    // The output will be written to the main output stream (same as UCI-compliant output).
    // This should only be done if the user has triggered a non-standard command.
    // In console mode, the output will be colored as debug output.
    template <typename... Args>
    void writeUciExtension(std::format_string<Args...> fmt, Args&&... args) const;

    // Write a debug message. If UCI debug mode is on, the message is sent over the UCI protocol.
    // Otherwise, it is written to the debug output stream.
    template <typename... Args>
    void writeDebug(std::format_string<Args...> fmt, Args&&... args) const;

    // Write a debug message directly to the debug output stream, bypassing UCI debug mode.
    template <typename... Args>
    void writeDebugNonUci(std::format_string<Args...> fmt, Args&&... args) const;

    IEngine& engine_;

    std::string name_;

    std::istream& in_;
    std::ostream& out_;
    std::ostream& debug_;

    std::stack<std::string> programmaticLines_;

    GameState gameState_;

    bool debugMode_ = false;
    bool quietMode_ = false;

    std::map<std::string, FrontEndOption, std::less<>> optionsMap_;

    std::atomic<bool> searchHasStarted_{false};
    std::future<void> goFuture_;

    std::optional<SearchStatistics> benchmarkStatistics_ = std::nullopt;
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
    addOption(
            FrontEndOption::createInteger(
                    "Hash",
                    engine_.getDefaultTTableSizeInMb(),
                    0,
                    1 * 1024 * 1024,
                    [this](const int requestedSizeInMb) {
                        engine_.setTTableSize(requestedSizeInMb);
                    }));

    addOption(FrontEndOption::createBoolean("Quiet", quietMode_));
}

UciFrontEnd::Impl::~Impl() {
    MY_ASSERT(!goFuture_.valid());
}

void UciFrontEnd::Impl::run() {
    handleUci();

    while (true) {
        std::string inputLine;

        if (!programmaticLines_.empty()) {
            inputLine = programmaticLines_.top();
            programmaticLines_.pop();
        } else if (in_.good()) {
            std::getline(in_, inputLine);
        } else {
            break;
        }

        std::stringstream lineSStream(inputLine);

        std::string command;
        lineSStream >> command;

        // Not implemented:
        //  ponderhit

        // Official UCI commands
        if (command == "uci") {
            handleUci();
        } else if (command == "isready") {
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
        }
        // UCI extensions
        else if (command == "eval") {
            handleEval();
        } else if (command == "listmoves") {
            handleListMoves();
        } else if (command == "hash") {
            handleHash();
        } else if (command == "fen") {
            handleFen();
        } else if (command == "board") {
            handleBoard();
        } else if (command == "startbench") {
            handleStartBench();
        } else if (command == "stopbench") {
            handleStopBench();
        } else if (command == "bench") {
            handleBench();
        }
        // Edge cases
        else if (command.empty()) {
            continue;
        } else {
            writeDebug("Warning: Ignoring unknown command: '{}'", command);
        }
    }

    stopSearchIfNeeded();
}

void UciFrontEnd::Impl::pushProgrammaticCommand(std::string_view command) {
    programmaticLines_.emplace(command);
}

void UciFrontEnd::Impl::reportSearchHasStarted() {
    searchHasStarted_ = true;
    searchHasStarted_.notify_all();
}

void UciFrontEnd::Impl::reportSearchInfo(const SearchInfo& searchInfo) const {
    std::string optionalScoreString = "";
    if (searchInfo.result.scoreType != ScoreType::NotSet) {
        MY_ASSERT(isValid(searchInfo.result.eval));

        optionalScoreString = std::format(" score {}", scoreToString(searchInfo.result.eval));
        if (searchInfo.result.scoreType == ScoreType::UpperBound) {
            optionalScoreString += " upperbound";
        } else if (searchInfo.result.scoreType == ScoreType::LowerBound) {
            optionalScoreString += " lowerbound";
        }
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

    std::string optionalPvString = "";
    if (!searchInfo.result.principalVariation.empty()) {
        optionalPvString =
                std::format(" pv {}", moveListToString(searchInfo.result.principalVariation));
    }

    writeUci(
            "info depth {} seldepth {}{} nodes {}{} time {}{} hashfull {}{}",
            searchInfo.depth,
            searchInfo.statistics.selectiveDepth,
            optionalScoreString,
            searchInfo.statistics.normalNodesSearched + searchInfo.statistics.qNodesSearched,
            optionalTbHitsString,
            searchInfo.statistics.timeElapsed.count(),
            optionalNpsString,
            (int)std::round(searchInfo.statistics.ttableUtilization * 1000),
            optionalPvString);
}

void UciFrontEnd::Impl::reportFullSearch(const SearchInfo& searchInfo) const {
    reportSearchInfo(searchInfo);
    std::flush(out_);
}

void UciFrontEnd::Impl::reportPartialSearch(const SearchInfo& searchInfo) const {
    if (!quietMode_) {
        writeDebug("Completed partial search of depth {}", searchInfo.depth);
    }

    reportSearchInfo(searchInfo);
    std::flush(out_);
}

void UciFrontEnd::Impl::reportSearchStatistics(const SearchStatistics& searchStatistics) const {
    if (!debugMode_) {
        return;
    }

    writeDebug("Normal nodes searched: {}", searchStatistics.normalNodesSearched);
    writeDebug("Quiescence nodes searched: {}", searchStatistics.qNodesSearched);
    writeDebug("TTable hits: {}", searchStatistics.tTableHits);
    writeDebug("TTable utilization: {:.1f}%", searchStatistics.ttableUtilization * 100.f);
}

void UciFrontEnd::Impl::reportAspirationWindowReSearch(
        const SearchInfo& searchInfo,
        const EvalT previousLowerBound,
        const EvalT previousUpperBound,
        const EvalT newLowerBound,
        const EvalT newUpperBound) const {
    reportSearchInfo(searchInfo);

    if (debugMode_) {
        writeDebug(
                "Aspiration window [{}, {}] failed (search returned {}); re-searching with "
                "window "
                "[{}, "
                "{}]",
                previousLowerBound,
                previousUpperBound,
                searchInfo.result.eval,
                newLowerBound,
                newUpperBound);
    }

    std::flush(out_);
}

void UciFrontEnd::Impl::reportDiscardedPv(std::string_view reason) const {
    if (quietMode_) {
        return;
    }

    writeDebug("Discarded PV: {}", reason);
}

void UciFrontEnd::Impl::reportError(std::string_view message) const {
    writeError("{}", message);
}

void UciFrontEnd::Impl::reportString(std::string_view message) const {
    writeUci("info string {}", message);
}

void UciFrontEnd::Impl::reportDebugString(std::string_view message) const {
    if (quietMode_) {
        return;
    }

    writeDebug("{}", message);
}

void UciFrontEnd::Impl::addOption(FrontEndOption option) {
    // UCI option names are case insensitive, so convert to lower case for lookup.
    optionsMap_.emplace(stringToLower(option.getName()), std::move(option));
}

void UciFrontEnd::Impl::handleUci() {
    writeUci("id name {}", name_);
    writeUci("id author Joost Houben");
    writeOptions();
    writeUci("uciok");
    std::flush(out_);
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
            writeError("Failed to parse FEN: {}", e.what());
            return;
        }
    }

    // Allow for the 'moves' token to be omitted at the end of the line.
    // This allows things like 'position startpos' as a short-hand for 'position startpos moves'.
    // While this behavior isn't specified in the original UCI protocol, it seems to be common
    // practice in many UCI GUIs and engines.
    if (lineSStream && token != "moves") {
        writeError("Unrecognized token '{}'. Expected 'moves'.", token);
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
            writeError("Failed to parse or apply move '{}': {}", moveString, e.what());
            return;
        }
    }

    if (debugMode_) {
        writeDebug("FEN: {}", gameState_.toFen());
        writeDebugNonUci("Board:\n{}", gameState_.toVisualString());
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
                    writeError("Failed to parse search move '{}': {}", token, e.what());
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
        writeDebug("Warning: no time control specified. Defaulting to fixed 1 second search.");
        timeManager.configureForFixedTimeSearch(std::chrono::seconds(1));
    }

    MY_ASSERT(!goFuture_.valid());

    searchHasStarted_ = false;

    goFuture_ = std::async(std::launch::async, [searchMoves, this] {
        try {
            const auto searchInfo = engine_.findMove(gameState_, searchMoves);

            MY_ASSERT(!searchInfo.result.principalVariation.empty());

            writeUci("bestmove {}", searchInfo.result.principalVariation[0].toUci());
            std::flush(out_);

            if (benchmarkStatistics_) {
                *benchmarkStatistics_ += searchInfo.statistics;
            }
        } catch (const std::exception& e) {
            writeError("{}", e.what());
        }
    });

    // Wait until the search has started before processing any further commands, to prevent race
    // conditions.
    searchHasStarted_.wait(/*old*/ false);
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
        writeError("Unknown debug setting '{}'. Expected 'on' or 'off'.", debugSettingString);
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
        writeError("Unknown option '{}'", optionParseResult->optionName);
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
            writeError("Failed to trigger action option '{}': {}", option.getName(), e.what());
        }
        return;
    }

    if (!optionParseResult->optionValue.has_value()) {
        writeError(
                "Option '{}' is not a button. Failed to find value in the following "
                "string: "
                "'{}'",
                option.getName(),
                line);
        return;
    }

    if (optionParseResult->optionValue->empty()) {
        if (option.getType() != FrontEndOption::Type::String) {
            writeError(
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
            writeError("Failed to set option '{}' to empty string: {}", option.getName(), e.what());
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
        writeError(
                "Failed to set option '{}' to '{}': {}",
                option.getName(),
                *optionParseResult->optionValue,
                e.what());
    }
}

void UciFrontEnd::Impl::handleEval() const {
    StackOfVectors<Move> stack;
    const EvalT eval = engine_.evaluate(gameState_);
    writeUciExtension("Eval: {:+} ({})", (float)eval / 100, scoreToString(eval));
}

void UciFrontEnd::Impl::handleListMoves() const {
    StackOfVectors<Move> stack;
    const auto moves = gameState_.generateMoves(stack);
    std::vector<Move> movesVector(moves.begin(), moves.end());
    writeUciExtension("Moves: {}", moveListToString(movesVector));
}

void UciFrontEnd::Impl::handleHash() const {
    writeUciExtension("Hash: 0x{:016x}", gameState_.getBoardHash());
}

void UciFrontEnd::Impl::handleFen() const {
    writeUciExtension("FEN: {}", gameState_.toFen());
}

void UciFrontEnd::Impl::handleBoard() const {
    writeUciExtension("{}", gameState_.toVisualString());
}

void UciFrontEnd::Impl::handleStartBench() {
    waitForGoToComplete();

    benchmarkStatistics_ = SearchStatistics{};
}

void UciFrontEnd::Impl::handleStopBench() {
    waitForGoToComplete();

    if (!benchmarkStatistics_.has_value()) {
        writeError("No benchmark in progress.");
        return;
    }

    const std::uint64_t totalNodes =
            benchmarkStatistics_->normalNodesSearched + benchmarkStatistics_->qNodesSearched;

    const float nps             = benchmarkStatistics_->nodesPerSecond;
    const std::string npsString = nps > 1e9 ? std::format("{:.2f} Gn/s", nps / 1e9)
                                : nps > 1e6 ? std::format("{:.2f} Mn/s", nps / 1e6)
                                : nps > 1e3 ? std::format("{:.2f} kn/s", nps / 1e3)
                                            : std::format("{:.0f} n/s", nps);

    writeUciExtension(
            "== Benchmark finished ==\n"
            "Total nodes searched: {}\n"
            "TB hits: {}\n"
            "Time elapsed: {:%T}\n"
            "Search speed: {}",
            totalNodes,
            benchmarkStatistics_->tbHits.value_or(0),
            benchmarkStatistics_->timeElapsed,
            npsString);

    benchmarkStatistics_ = std::nullopt;
}

void UciFrontEnd::Impl::handleBench() {
    // TODO: add sub-commands for:
    //  - a 'small' bench; useful for slow debug builds
    //  - a timed bench, using time management instead of fixed depth/nodes

    const auto benchCommands = getBenchCommands();
    for (const auto& benchCommand : benchCommands | std::views::reverse) {
        pushProgrammaticCommand(benchCommand);
    }
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
        writeError(
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
        writeError("Failed to find option name in the following string: '{}'", line);
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
void UciFrontEnd::Impl::writeError(const std::format_string<Args...> fmt, Args&&... args) const {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Red, out_);

    std::println(out_, "info string Error: {}", std::format(fmt, std::forward<Args>(args)...));
}

template <typename... Args>
void UciFrontEnd::Impl::writeUci(const std::format_string<Args...> fmt, Args&&... args) const {
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Green, out_);

    std::println(out_, fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void UciFrontEnd::Impl::writeUciExtension(
        const std::format_string<Args...> fmt, Args&&... args) const {
    // Color as debug output.
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow, out_);

    // But write to main output stream.
    std::println(out_, fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void UciFrontEnd::Impl::writeDebug(const std::format_string<Args...> fmt, Args&&... args) const {
    // Color as debug output regardless of which stream we write to.
    ScopedConsoleColor scopedConsoleColor(ConsoleColor::Yellow, out_);
    if (debugMode_) {
        // In debug mode, write over UCI protocol.
        std::println(out_, "info string {}", std::format(fmt, std::forward<Args>(args)...));
    } else {
        // Otherwise, write to debug output stream.
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

void UciFrontEnd::pushProgrammaticCommand(std::string_view command) {
    impl_->pushProgrammaticCommand(command);
}

void UciFrontEnd::reportSearchHasStarted() {
    impl_->reportSearchHasStarted();
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
        const SearchInfo& searchInfo,
        const EvalT previousLowerBound,
        const EvalT previousUpperBound,
        const EvalT newLowerBound,
        const EvalT newUpperBound) const {
    impl_->reportAspirationWindowReSearch(
            searchInfo, previousLowerBound, previousUpperBound, newLowerBound, newUpperBound);
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
