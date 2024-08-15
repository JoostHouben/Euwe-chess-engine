#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace GameStateTests {

TEST(GameStateTests, ThreeFoldRepetition) {
    // Position from https://en.wikipedia.org/wiki/Threefold_repetition "Fischer vs. Petrosian, 1971"
    const std::string fischerPetrosianFen = "8/pp3p1k/2p2q1p/3r1P2/5R2/7P/P1P1QP2/7K b - - 0 1";

    GameState gameState = GameState::fromFen(fischerPetrosianFen);
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(1));

    gameState.makeMove(Move::fromAlgebraic("Qe5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(1));

    gameState.makeMove(Move::fromAlgebraic("Qh5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(1));

    gameState.makeMove(Move::fromAlgebraic("Qf6", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(1));

    // First repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_TRUE(gameState.isRepetition(1));

    gameState.makeMove(Move::fromAlgebraic("Re5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(1));

    gameState.makeMove(Move::fromAlgebraic("Qd3", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(1));

    gameState.makeMove(Move::fromAlgebraic("Rd5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(1));

    // Second repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState));
    EXPECT_TRUE(gameState.isRepetition());
    EXPECT_TRUE(gameState.isRepetition(1));
}

}  // namespace GameStateTests