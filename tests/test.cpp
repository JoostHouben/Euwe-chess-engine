#include "gtest/gtest.h"

#include "chess-engine-lib/GameState.h"

TEST(FenParsing, TestStartingPosition) {
    GameState startingPosition = GameState::startingPosition();
}