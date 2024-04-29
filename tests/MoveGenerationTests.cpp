#include "chess-engine-lib/GameState.h"

#pragma warning(disable : 26495)
#include "gtest/gtest.h"

#include <optional>

namespace MoveGenerationTests {

struct MoveStatistics {
    std::size_t numMoves = 0;
    std::size_t numCaptures = 0;
    std::size_t numEnPassant = 0;
    std::size_t numCastle = 0;
    std::size_t numPromotions = 0;
};

struct ExpectedMoveStatistics {
    std::optional<std::size_t> numMoves = std::nullopt;
    std::optional<std::size_t> numCaptures = std::nullopt;
    std::optional<std::size_t> numEnPassant = std::nullopt;
    std::optional<std::size_t> numCastle = std::nullopt;
    std::optional<std::size_t> numPromotions = std::nullopt;
};

void updateStatistics(const std::vector<Move>& moves,
                      MoveStatistics& statistics) {
    statistics.numMoves += moves.size();
    for (const auto move : moves) {
        statistics.numCaptures += isCapture(move.flags);
        statistics.numEnPassant += isEnPassant(move.flags);
        statistics.numCastle += isCastle(move.flags);
        statistics.numPromotions += isPromotion(move.flags);
    }
}

void countMoveStatisticsAtPly(const GameState gameState, int ply,
                              MoveStatistics& statistics) {
    const std::vector<Move> moves = gameState.generateMoves();
    if (ply == 0) {
        updateStatistics(moves, statistics);
        return;
    };
    for (const auto move : moves) {
        GameState nextGameState(gameState);
        nextGameState.makeMove(move);
        countMoveStatisticsAtPly(nextGameState, ply - 1, statistics);
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
    countMoveStatisticsAtPly(gameState, config.depth, statistics);

    if (config.expectedStats.numMoves.has_value()) {
        EXPECT_EQ(statistics.numMoves, config.expectedStats.numMoves.value());
    }
    if (config.expectedStats.numCaptures.has_value()) {
        EXPECT_EQ(statistics.numCaptures,
                  config.expectedStats.numCaptures.value());
    }
    if (config.expectedStats.numEnPassant.has_value()) {
        EXPECT_EQ(statistics.numEnPassant,
                  config.expectedStats.numEnPassant.value());
    }
    if (config.expectedStats.numCastle.has_value()) {
        EXPECT_EQ(statistics.numCastle, config.expectedStats.numCastle.value());
    }
    if (config.expectedStats.numPromotions.has_value()) {
        EXPECT_EQ(statistics.numPromotions,
                  config.expectedStats.numPromotions.value());
    }
}

// Positions and statistics taken from https://www.chessprogramming.org/Perft_Results

inline const std::string kKiwipeteFen =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
inline const std::string kPosition3Fen =
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
inline const std::string kPosition4Fen =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
inline const std::string kPosition5Fen =
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";

std::string validateMoveStatsName(
        const ::testing::TestParamInfo<TestStatsConfig>& info) {
    std::string fenName = "";
    if (info.param.fen == kStartingPositionFen) {
        fenName = "root";
    } else if (info.param.fen == kKiwipeteFen) {
        fenName = "kiwipete";
    } else if (info.param.fen == kPosition3Fen) {
        fenName = "position3";
    } else if (info.param.fen == kPosition4Fen) {
        fenName = "position4";
    } else if (info.param.fen == kPosition5Fen) {
        fenName = "position5";
    }
    return fenName + "_depth" + std::to_string(info.param.depth);
}

// clang-format off
INSTANTIATE_TEST_CASE_P(
    MoveGeneration,
    ValidateMoveStats,
    ::testing::Values(
        // root
        TestStatsConfig{
            .fen = kStartingPositionFen,
            .depth = 0,
            .expectedStats = {.numMoves = 20, .numCaptures = 0, .numEnPassant = 0, .numCastle = 0, .numPromotions = 0}
        },
        TestStatsConfig{
            .fen = kStartingPositionFen,
            .depth = 1,
            .expectedStats = {.numMoves = 400, .numCaptures = 0, .numEnPassant = 0, .numCastle = 0, .numPromotions = 0}
        },
        TestStatsConfig{
            .fen = kStartingPositionFen,
            .depth = 2,
            .expectedStats = {.numMoves = 8'902, .numCaptures = 34, .numEnPassant = 0, .numCastle = 0, .numPromotions = 0}
        },
        // kiwipete
        TestStatsConfig{
            .fen = kKiwipeteFen,
            .depth = 0,
            .expectedStats = {.numMoves = 48, .numCaptures = 8, .numEnPassant = 0, .numCastle = 2, .numPromotions = 0}
        },
        TestStatsConfig{
            .fen = kKiwipeteFen,
            .depth = 1,
            .expectedStats = {.numMoves = 2'039, .numCaptures = 351, .numEnPassant = 1, .numCastle = 91, .numPromotions = 0}
        },
        TestStatsConfig{
            .fen = kKiwipeteFen,
            .depth = 2,
            .expectedStats = {.numMoves = 97'862, .numCaptures = 17'102, .numEnPassant = 45, .numCastle = 3'162, .numPromotions = 0}
        },
        // position3
        TestStatsConfig{
            .fen = kPosition3Fen,
            .depth = 0,
            .expectedStats = {.numMoves = 14, .numCaptures = 1, .numEnPassant = 0, .numCastle = 0, .numPromotions = 0}
        },
        TestStatsConfig{
            .fen = kPosition3Fen,
            .depth = 1,
            .expectedStats = {.numMoves = 191, .numCaptures = 14, .numEnPassant = 0, .numCastle = 0, .numPromotions = 0}
        },
        TestStatsConfig{
            .fen = kPosition3Fen,
            .depth = 2,
            .expectedStats = {.numMoves = 2'812, .numCaptures = 209, .numEnPassant = 2, .numCastle = 0, .numPromotions = 0}
        },
        // position4
        TestStatsConfig{
            .fen = kPosition4Fen,
            .depth = 0,
            .expectedStats = {.numMoves = 6, .numCaptures = 0, .numEnPassant = 0, .numCastle = 0, .numPromotions = 0}
        },
        TestStatsConfig{
            .fen = kPosition4Fen,
            .depth = 1,
            .expectedStats = {.numMoves = 264, .numCaptures = 87, .numEnPassant = 0, .numCastle = 6, .numPromotions = 48}
        },
        TestStatsConfig{
            .fen = kPosition4Fen,
            .depth = 2,
            .expectedStats = {.numMoves = 9'467, .numCaptures = 1'021, .numEnPassant = 4, .numCastle = 0, .numPromotions = 120}
        },
        // position5
        TestStatsConfig{
            .fen = kPosition5Fen,
            .depth = 0,
            .expectedStats = {.numMoves = 44}
        },
        TestStatsConfig{
            .fen = kPosition5Fen,
            .depth = 1,
            .expectedStats = {.numMoves = 1'486}
        },
        TestStatsConfig{
            .fen = kPosition5Fen,
            .depth = 2,
            .expectedStats = {.numMoves = 62'379}
        }
    ),
    validateMoveStatsName
);

INSTANTIATE_TEST_CASE_P(
    MoveGenerationSlow,
    ValidateMoveStats,
    ::testing::Values(
        // Slow! ~1.5s in release mode
        TestStatsConfig{
            .fen = kStartingPositionFen,
            .depth = 3,
            .expectedStats = {.numMoves = 197'281, .numCaptures = 1'576, .numEnPassant = 0, .numCastle = 0, .numPromotions = 0}
        },
        // Vey slow! ~38s in release mode
        TestStatsConfig{
            .fen = kStartingPositionFen,
            .depth = 4,
            .expectedStats = {.numMoves = 4'865'609, .numCaptures = 82'719, .numEnPassant = 258, .numCastle = 0, .numPromotions = 0}
        }
    ),
    validateMoveStatsName
);
            // clang-format on

}  // namespace MoveGenerationTests
