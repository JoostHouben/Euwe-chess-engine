#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

TEST(MoveGeneration, TestStartingPositionMoveCount) {
    const GameState startingPosition = GameState::startingPosition();
    const std::vector<Move> moves = startingPosition.generateMoves();

    EXPECT_EQ(moves.size(), 20);
}

struct MoveStatistics {
    std::size_t numMoves = 0;
    std::size_t numCaptures = 0;
    std::size_t numEnPassant = 0;
    std::size_t numCastle = 0;
    std::size_t numPromotions = 0;
};

void updateStatistics(const std::vector<Move>& moves, MoveStatistics& statistics) {
    statistics.numMoves += moves.size();
    for (const auto move : moves) {
        statistics.numCaptures += isCapture(move.flags);
        statistics.numEnPassant += isEnPassant(move.flags);
        statistics.numCastle += isCastle(move.flags);
        statistics.numPromotions += isPromotion(move.flags);
    }
}

void countMoveStatisticsAtPly(const GameState gameState, int ply, MoveStatistics& statistics) {
    const std::vector<Move> moves = gameState.generateMoves();
    if (ply == 0) {
        updateStatistics(moves, statistics);
        return;
    }
;
    for (const auto move : moves) {
        GameState nextGameState(gameState);
        nextGameState.makeMove(move);
        countMoveStatisticsAtPly(nextGameState, ply - 1, statistics);
    }
}

// Statistics taken from https://www.chessprogramming.org/Perft_Results
TEST(MoveGeneration, TestRootMoveStatsDepth1) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 1, statistics);

    EXPECT_EQ(statistics.numMoves, 400);
    EXPECT_EQ(statistics.numCaptures, 0);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestRootMoveStatsDepth2) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 2, statistics);

    EXPECT_EQ(statistics.numMoves, 8'902);
    EXPECT_EQ(statistics.numCaptures, 34);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

// Slow! ~5s in debug mode (~100 ms in release)
TEST(MoveGenerationSlow, TestRootMoveStatsDepth3) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 3, statistics);

    EXPECT_EQ(statistics.numMoves, 197'281);
    EXPECT_EQ(statistics.numCaptures, 1'576);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

// Slow! ~2s in release mode
TEST(MoveGenerationSlow, TestRootMoveStatsDepth4) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 4, statistics);

    EXPECT_EQ(statistics.numMoves, 4'865'609);
    EXPECT_EQ(statistics.numCaptures, 82'719);
    EXPECT_EQ(statistics.numEnPassant, 258);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestKiwipeteStatsDepth0) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -");
    countMoveStatisticsAtPly(gameState, 0, statistics);

    EXPECT_EQ(statistics.numMoves, 48);
    EXPECT_EQ(statistics.numCaptures, 8);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 2);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestKiwipeteStatsDepth1) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -");
    countMoveStatisticsAtPly(gameState, 1, statistics);

    EXPECT_EQ(statistics.numMoves, 2'039);
    EXPECT_EQ(statistics.numCaptures, 351);
    EXPECT_EQ(statistics.numEnPassant, 1);
    EXPECT_EQ(statistics.numCastle, 91);
    EXPECT_EQ(statistics.numPromotions, 0);
}
