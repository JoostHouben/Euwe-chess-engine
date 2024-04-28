#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

TEST(MoveGeneration, TestStartingPositionMoveCount) {
    const GameState startingPosition = GameState::startingPosition();
    const std::vector<Move> moves = startingPosition.generateMoves();

    ASSERT_EQ(moves.size(), 20);
}

std::size_t countMovesAtPly(const GameState gameState, int ply) {
    const std::vector<Move> moves = gameState.generateMoves();
    if (ply == 0) {
        return moves.size();
    }

    std::size_t numMoves = 0;
    for (const auto move : moves) {
        GameState nextGameState(gameState);
        nextGameState.makeMove(move);
        numMoves += countMovesAtPly(nextGameState, ply - 1);
    }

    return numMoves;
}

TEST(MoveGeneration, TestMoveCountAtDepth) {
    const GameState startingPosition = GameState::startingPosition();

    ASSERT_EQ(countMovesAtPly(startingPosition, 1), 400);
    ASSERT_EQ(countMovesAtPly(startingPosition, 2), 8'902);
    //ASSERT_EQ(countMovesAtPly(startingPosition, 3), 197'281);
    //ASSERT_EQ(countMovesAtPly(startingPosition, 4), 4'865'609);
}

