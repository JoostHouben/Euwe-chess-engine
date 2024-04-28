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
};

void updateStatistics(const std::vector<Move>& moves, MoveStatistics& statistics) {
    statistics.numMoves += moves.size();
    for (const auto move : moves) {
        const bool isCapture = (int)move.flags & (int)MoveFlags::IsCapture;
        const bool isEnPassant = (int)move.flags & (int)MoveFlags::IsEnPassant;
        const bool isCastle = (int)move.flags & (int)MoveFlags::IsCastle;

        statistics.numCaptures += isCapture;
        statistics.numEnPassant += isEnPassant;
        statistics.numCastle += isCastle;
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

TEST(MoveGeneration, TestMoveStatisticsAt1Ply) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 1, statistics);
    const GameState startingPosition = GameState::startingPosition();

    EXPECT_EQ(statistics.numMoves, 400);
    EXPECT_EQ(statistics.numCaptures, 0);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
}

TEST(MoveGeneration, TestMoveStatisticsAt2Ply) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 2, statistics);
    const GameState startingPosition = GameState::startingPosition();

    EXPECT_EQ(statistics.numMoves, 8'902);
    EXPECT_EQ(statistics.numCaptures, 34);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
}

// Slow! ~5s in debug mode (~100 ms in release)
TEST(MoveGenerationSlow, TestMoveStatisticsAt3Ply) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 3, statistics);
    const GameState startingPosition = GameState::startingPosition();

    EXPECT_EQ(statistics.numMoves, 197'281);
    EXPECT_EQ(statistics.numCaptures, 1'576);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
}

// Slow! ~2s in release mode
TEST(MoveGenerationSlow, TestMoveStatisticsAt4Ply) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 4, statistics);
    const GameState startingPosition = GameState::startingPosition();

    EXPECT_EQ(statistics.numMoves, 4'865'609);
    EXPECT_EQ(statistics.numCaptures, 82'719);
    EXPECT_EQ(statistics.numEnPassant, 258);
    EXPECT_EQ(statistics.numCastle, 0);
}
