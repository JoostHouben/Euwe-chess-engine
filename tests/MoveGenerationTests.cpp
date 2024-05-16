#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

#include <algorithm>
#include <iostream>
#include <optional>

namespace MoveGenerationTests {

struct MoveStatistics {
    std::size_t numMoves      = 0;
    std::size_t numCaptures   = 0;
    std::size_t numEnPassant  = 0;
    std::size_t numCastle     = 0;
    std::size_t numPromotions = 0;
};

struct ExpectedMoveStatistics {
    std::optional<std::size_t> numMoves      = std::nullopt;
    std::optional<std::size_t> numCaptures   = std::nullopt;
    std::optional<std::size_t> numEnPassant  = std::nullopt;
    std::optional<std::size_t> numCastle     = std::nullopt;
    std::optional<std::size_t> numPromotions = std::nullopt;
};

void compareStatistics(const MoveStatistics& actual, const ExpectedMoveStatistics& expected) {
    if (expected.numMoves.has_value()) {
        EXPECT_EQ(actual.numMoves, expected.numMoves.value());
    }
    if (expected.numCaptures.has_value()) {
        EXPECT_EQ(actual.numCaptures, expected.numCaptures.value());
    }
    if (expected.numEnPassant.has_value()) {
        EXPECT_EQ(actual.numEnPassant, expected.numEnPassant.value());
    }
    if (expected.numCastle.has_value()) {
        EXPECT_EQ(actual.numCastle, expected.numCastle.value());
    }
    if (expected.numPromotions.has_value()) {
        EXPECT_EQ(actual.numPromotions, expected.numPromotions.value());
    }
}

void updateStatistics(const StackVector<Move>& moves, MoveStatistics& statistics) {
    statistics.numMoves += moves.size();
    for (const auto& move : moves) {
        statistics.numCaptures += isCapture(move.flags);
        statistics.numEnPassant += isEnPassant(move.flags);
        statistics.numCastle += isCastle(move.flags);
        statistics.numPromotions += isPromotion(move.flags);
    }
}

void countMoveStatisticsAtPly(
        GameState& gameState, int ply, MoveStatistics& statistics, StackOfVectors<Move>& stack) {
    const StackVector<Move> moves = gameState.generateMoves(stack);
    if (ply == 0) {
        return;
    }
    if (ply == 1) {
        updateStatistics(moves, statistics);
        return;
    };
    for (const auto& move : moves) {
        GameState copyState(gameState);
        (void)copyState.makeMove(move);
        countMoveStatisticsAtPly(copyState, ply - 1, statistics, stack);
    }
}

void countMoveStatisticsAtPlyWithUnmake(
        GameState& gameState, int ply, MoveStatistics& statistics, StackOfVectors<Move>& stack) {
    const StackVector<Move> moves = gameState.generateMoves(stack);
    if (ply == 0) {
        return;
    }
    if (ply == 1) {
        updateStatistics(moves, statistics);
        return;
    };

    HashT hash = gameState.getBoardHash();

    for (const auto& move : moves) {
        auto unmakeInfo = gameState.makeMove(move);

        EXPECT_NE(hash, gameState.getBoardHash());

        countMoveStatisticsAtPlyWithUnmake(gameState, ply - 1, statistics, stack);
        gameState.unmakeMove(move, unmakeInfo);

        EXPECT_EQ(hash, gameState.getBoardHash());

        if (hash != gameState.getBoardHash()) {
            std::cerr << moveToExtendedString(move) << std::endl;
            hash = gameState.getBoardHash();
        }
    }
}

struct TestStatsConfig {
    std::string fen;
    int depth;
    ExpectedMoveStatistics expectedStats;
};

class ValidateMoveStats : public ::testing::TestWithParam<TestStatsConfig> {};

TEST_P(ValidateMoveStats, TestMoveStats) {
    const TestStatsConfig config = GetParam();
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(config.fen);
    StackOfVectors<Move> stack;
    stack.reserve(300);
    countMoveStatisticsAtPly(gameState, config.depth, statistics, stack);
    compareStatistics(statistics, config.expectedStats);
}

class ValidateMoveStatsWithUnmake : public ::testing::TestWithParam<TestStatsConfig> {};

TEST_P(ValidateMoveStatsWithUnmake, TestMoveStats) {
    const TestStatsConfig config = GetParam();
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(config.fen);
    StackOfVectors<Move> stack;
    stack.reserve(300);
    countMoveStatisticsAtPlyWithUnmake(gameState, config.depth, statistics, stack);
    compareStatistics(statistics, config.expectedStats);
}

// Positions and statistics taken from https://www.chessprogramming.org/Perft_Results

inline const std::string kKiwipeteFen =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
inline const std::string kPosition3Fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
inline const std::string kPosition4Fen =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
inline const std::string kPosition5Fen =
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
inline const std::string kPosition6Fen =
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";

inline const std::string kInCheckByPawn =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q2/PPPBBPpP/R4K1R w kq - 0 1";
inline const std::string kInCheckByPawnWithPinnedPawn = "8/2p5/3p4/KP5r/1R3p1k/6P1/4P3/8 b - - 0 1";
inline const std::string kBlackAboutToPromote =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P1RPP/R2Q2K1 b kq - 1 1";  // Promotion b2a1Q
inline const std::string kEnPassantNoDiscoveredCheck = "8/8/3p4/KPp1P2r/1R3p2/6k1/6P1/8 w - c6 0 1";

// Position 5: white about to promote (d7c8B), black captures promoted piece (d8c8)

std::string validateMoveStatsName(const ::testing::TestParamInfo<TestStatsConfig>& info) {
    std::string fenName = "";
    if (info.param.fen == getStartingPositionFen()) {
        fenName = "root";
    } else if (info.param.fen == kKiwipeteFen) {
        fenName = "kiwipete";
    } else if (info.param.fen == kPosition3Fen) {
        fenName = "position3";
    } else if (info.param.fen == kPosition4Fen) {
        fenName = "position4";
    } else if (info.param.fen == kPosition5Fen) {
        fenName = "position5";
    } else if (info.param.fen == kPosition6Fen) {
        fenName = "position6";
    } else if (info.param.fen == kInCheckByPawn) {
        fenName = "inCheckByPawn";
    } else if (info.param.fen == kInCheckByPawnWithPinnedPawn) {
        fenName = "inCheckByPawnWithPinnedPawn";
    } else if (info.param.fen == kBlackAboutToPromote) {
        fenName = "blackAboutToPromote";
    } else if (info.param.fen == kEnPassantNoDiscoveredCheck) {
        fenName = "enPassantNoDiscoveredCheck";
    }

    return fenName + "_depth" + std::to_string(info.param.depth);
}

auto testCasesFast = ::testing::Values(
        // root
        TestStatsConfig{
                .fen   = getStartingPositionFen(),
                .depth = 1,
                .expectedStats =
                        {.numMoves      = 20,
                         .numCaptures   = 0,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = getStartingPositionFen(),
                .depth = 2,
                .expectedStats =
                        {.numMoves      = 400,
                         .numCaptures   = 0,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = getStartingPositionFen(),
                .depth = 3,
                .expectedStats =
                        {.numMoves      = 8'902,
                         .numCaptures   = 34,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // kiwipete
        TestStatsConfig{
                .fen   = kKiwipeteFen,
                .depth = 1,
                .expectedStats =
                        {.numMoves      = 48,
                         .numCaptures   = 8,
                         .numEnPassant  = 0,
                         .numCastle     = 2,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = kKiwipeteFen,
                .depth = 2,
                .expectedStats =
                        {.numMoves      = 2'039,
                         .numCaptures   = 351,
                         .numEnPassant  = 1,
                         .numCastle     = 91,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = kKiwipeteFen,
                .depth = 3,
                .expectedStats =
                        {.numMoves      = 97'862,
                         .numCaptures   = 17'102,
                         .numEnPassant  = 45,
                         .numCastle     = 3'162,
                         .numPromotions = 0}},
        // position3
        TestStatsConfig{
                .fen   = kPosition3Fen,
                .depth = 1,
                .expectedStats =
                        {.numMoves      = 14,
                         .numCaptures   = 1,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = kPosition3Fen,
                .depth = 2,
                .expectedStats =
                        {.numMoves      = 191,
                         .numCaptures   = 14,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = kPosition3Fen,
                .depth = 3,
                .expectedStats =
                        {.numMoves      = 2'812,
                         .numCaptures   = 209,
                         .numEnPassant  = 2,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = kPosition3Fen,
                .depth = 4,
                .expectedStats =
                        {.numMoves      = 43'238,
                         .numCaptures   = 3'348,
                         .numEnPassant  = 123,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // position4
        TestStatsConfig{
                .fen   = kPosition4Fen,
                .depth = 1,
                .expectedStats =
                        {.numMoves      = 6,
                         .numCaptures   = 0,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        TestStatsConfig{
                .fen   = kPosition4Fen,
                .depth = 2,
                .expectedStats =
                        {.numMoves      = 264,
                         .numCaptures   = 87,
                         .numEnPassant  = 0,
                         .numCastle     = 6,
                         .numPromotions = 48}},
        TestStatsConfig{
                .fen   = kPosition4Fen,
                .depth = 3,
                .expectedStats =
                        {.numMoves      = 9'467,
                         .numCaptures   = 1'021,
                         .numEnPassant  = 4,
                         .numCastle     = 0,
                         .numPromotions = 120}},
        // position5
        TestStatsConfig{.fen = kPosition5Fen, .depth = 1, .expectedStats = {.numMoves = 44}},
        TestStatsConfig{.fen = kPosition5Fen, .depth = 2, .expectedStats = {.numMoves = 1'486}},
        TestStatsConfig{.fen = kPosition5Fen, .depth = 3, .expectedStats = {.numMoves = 62'379}},
        // position6
        TestStatsConfig{.fen = kPosition6Fen, .depth = 1, .expectedStats = {.numMoves = 46}},
        TestStatsConfig{.fen = kPosition6Fen, .depth = 2, .expectedStats = {.numMoves = 2'079}},
        TestStatsConfig{.fen = kPosition6Fen, .depth = 3, .expectedStats = {.numMoves = 89'890}},

        // inCheckByPawn
        TestStatsConfig{
                .fen   = kInCheckByPawn,
                .depth = 1,
                .expectedStats =
                        {.numMoves      = 4,
                         .numCaptures   = 2,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // inCheckByPawnWithPinnedPawn
        TestStatsConfig{
                .fen   = kInCheckByPawnWithPinnedPawn,
                .depth = 1,
                .expectedStats =
                        {.numMoves      = 4,
                         .numCaptures   = 1,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // blackAboutToPromote
        TestStatsConfig{
                .fen   = kBlackAboutToPromote,
                .depth = 2,
                .expectedStats =
                        {.numMoves      = 1623,
                         .numCaptures   = 167,
                         .numEnPassant  = 1,
                         .numCastle     = 0,
                         .numPromotions = 20}},
        // enPassantNoDiscoveredCheck
        TestStatsConfig{
                .fen           = kEnPassantNoDiscoveredCheck,
                .depth         = 1,
                .expectedStats = {
                        .numMoves      = 15,
                        .numCaptures   = 3,
                        .numEnPassant  = 1,
                        .numCastle     = 0,
                        .numPromotions = 0}});

// Total in release mode: ~4.6s
auto testCasesSlow = ::testing::Values(
        // Release mode: ~2ms
        TestStatsConfig{
                .fen   = getStartingPositionFen(),
                .depth = 4,
                .expectedStats =
                        {.numMoves      = 197'281,
                         .numCaptures   = 1'576,
                         .numEnPassant  = 0,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // Release mode: ~50ms
        TestStatsConfig{
                .fen   = getStartingPositionFen(),
                .depth = 5,
                .expectedStats =
                        {.numMoves      = 4'865'609,
                         .numCaptures   = 8'2719,
                         .numEnPassant  = 258,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // Release mode: ~1.1s
        TestStatsConfig{
                .fen   = getStartingPositionFen(),
                .depth = 6,
                .expectedStats =
                        {.numMoves      = 119'060'324,
                         .numCaptures   = 2'812'008,
                         .numEnPassant  = 5248,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // Release mode: ~30ms
        TestStatsConfig{
                .fen   = kKiwipeteFen,
                .depth = 4,
                .expectedStats =
                        {.numMoves      = 4'085'603,
                         .numCaptures   = 757'163,
                         .numEnPassant  = 1'929,
                         .numCastle     = 128'013,
                         .numPromotions = 15'172}},
        // Release mode: ~1.3s
        TestStatsConfig{
                .fen   = kKiwipeteFen,
                .depth = 5,
                .expectedStats =
                        {.numMoves      = 193'690'690,
                         .numCaptures   = 35'043'416,
                         .numEnPassant  = 73'365,
                         .numCastle     = 4'993'637,
                         .numPromotions = 8'392}},
        // Release mode: ~10ms
        TestStatsConfig{
                .fen   = kPosition3Fen,
                .depth = 5,
                .expectedStats =
                        {.numMoves      = 674'624,
                         .numCaptures   = 52'051,
                         .numEnPassant  = 1'165,
                         .numCastle     = 0,
                         .numPromotions = 0}},
        // Release mode: ~110ms
        TestStatsConfig{
                .fen   = kPosition3Fen,
                .depth = 6,
                .expectedStats =
                        {.numMoves      = 11'030'083,
                         .numCaptures   = 940'350,
                         .numEnPassant  = 33'325,
                         .numCastle     = 0,
                         .numPromotions = 7'552}},
        // Release mode: ~4ms
        TestStatsConfig{
                .fen   = kPosition4Fen,
                .depth = 4,
                .expectedStats =
                        {.numMoves      = 422'333,
                         .numCaptures   = 131'393,
                         .numEnPassant  = 0,
                         .numCastle     = 7'795,
                         .numPromotions = 60'032}},
        // Release mode: ~130ms
        TestStatsConfig{
                .fen   = kPosition4Fen,
                .depth = 5,
                .expectedStats =
                        {.numMoves      = 15'833'292,
                         .numCaptures   = 2'046'173,
                         .numEnPassant  = 6'512,
                         .numCastle     = 0,
                         .numPromotions = 329'464}},
        // Release mode: ~15ms
        TestStatsConfig{.fen = kPosition5Fen, .depth = 4, .expectedStats = {.numMoves = 2'103'487}},
        // Release mode: ~600ms
        TestStatsConfig{
                .fen = kPosition5Fen, .depth = 5, .expectedStats = {.numMoves = 89'941'194}},
        // Release mode: ~30ms
        TestStatsConfig{.fen = kPosition6Fen, .depth = 4, .expectedStats = {.numMoves = 3'894'594}},
        // Release mode: ~1.2s
        TestStatsConfig{
                .fen = kPosition6Fen, .depth = 5, .expectedStats = {.numMoves = 164'075'551}});

INSTANTIATE_TEST_CASE_P(MoveGeneration, ValidateMoveStats, testCasesFast, validateMoveStatsName);

INSTANTIATE_TEST_CASE_P(
        MoveGeneration, ValidateMoveStatsWithUnmake, testCasesFast, validateMoveStatsName);

INSTANTIATE_TEST_CASE_P(
        MoveGenerationSlow, ValidateMoveStats, testCasesSlow, validateMoveStatsName);

}  // namespace MoveGenerationTests
