#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

TEST(MoveGeneration, TestStartingPositionMoveCount) {
    const GameState startingPosition = GameState::startingPosition();
    const std::vector<Move> moves = startingPosition.generateMoves();

    ASSERT_EQ(moves.size(), 20);
}

