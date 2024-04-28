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

// Positions and statistics taken from https://www.chessprogramming.org/Perft_Results
inline const std::string kKiwipeteFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -";
inline const std::string kPosition3Fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -";
inline const std::string kPosition4Fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";

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

// Slow! ~2s in release mode
TEST(MoveGenerationSlow, TestRootMoveStatsDepth3) {
    MoveStatistics statistics{};
    countMoveStatisticsAtPly(GameState::startingPosition(), 3, statistics);

    EXPECT_EQ(statistics.numMoves, 197'281);
    EXPECT_EQ(statistics.numCaptures, 1'576);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

// Vey slow! ~38s in release mode
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
    GameState gameState = GameState::fromFen(kKiwipeteFen);
    countMoveStatisticsAtPly(gameState, 0, statistics);

    EXPECT_EQ(statistics.numMoves, 48);
    EXPECT_EQ(statistics.numCaptures, 8);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 2);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestKiwipeteStatsDepth1) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kKiwipeteFen);
    countMoveStatisticsAtPly(gameState, 1, statistics);

    EXPECT_EQ(statistics.numMoves, 2'039);
    EXPECT_EQ(statistics.numCaptures, 351);
    EXPECT_EQ(statistics.numEnPassant, 1);
    EXPECT_EQ(statistics.numCastle, 91);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestKiwipeteStatsDepth2) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kKiwipeteFen);
    countMoveStatisticsAtPly(gameState, 2, statistics);

    EXPECT_EQ(statistics.numMoves, 97'862);
    EXPECT_EQ(statistics.numCaptures, 17'102);
    EXPECT_EQ(statistics.numEnPassant, 45);
    EXPECT_EQ(statistics.numCastle, 3'162);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestPosition3StatsDepth0) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kPosition3Fen);
    countMoveStatisticsAtPly(gameState, 0, statistics);

    EXPECT_EQ(statistics.numMoves, 14);
    EXPECT_EQ(statistics.numCaptures, 1);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestPosition3StatsDepth1) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kPosition3Fen);
    countMoveStatisticsAtPly(gameState, 1, statistics);

    EXPECT_EQ(statistics.numMoves, 191);
    EXPECT_EQ(statistics.numCaptures, 14);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestPosition3StatsDepth2) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kPosition3Fen);
    countMoveStatisticsAtPly(gameState, 2, statistics);

    EXPECT_EQ(statistics.numMoves, 2'812);
    EXPECT_EQ(statistics.numCaptures, 209);
    EXPECT_EQ(statistics.numEnPassant, 2);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestPosition4StatsDepth0) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kPosition4Fen);
    countMoveStatisticsAtPly(gameState, 0, statistics);

    EXPECT_EQ(statistics.numMoves, 6);
    EXPECT_EQ(statistics.numCaptures, 0);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 0);
}

TEST(MoveGeneration, TestPosition4StatsDepth1) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kPosition4Fen);
    countMoveStatisticsAtPly(gameState, 1, statistics);

    EXPECT_EQ(statistics.numMoves, 264);
    EXPECT_EQ(statistics.numCaptures, 87);
    EXPECT_EQ(statistics.numEnPassant, 0);
    EXPECT_EQ(statistics.numCastle, 6);
    EXPECT_EQ(statistics.numPromotions, 48);
}

TEST(MoveGeneration, TestPosition4StatsDepth2) {
    MoveStatistics statistics{};
    GameState gameState = GameState::fromFen(kPosition4Fen);
    countMoveStatisticsAtPly(gameState, 2, statistics);

    EXPECT_EQ(statistics.numMoves, 9'467);
    EXPECT_EQ(statistics.numCaptures, 1'021);
    EXPECT_EQ(statistics.numEnPassant, 4);
    EXPECT_EQ(statistics.numCastle, 0);
    EXPECT_EQ(statistics.numPromotions, 120);
}
