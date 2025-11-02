#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace GameStateTests {

TEST(GameStateTests, ThreeFoldRepetition) {
    // Position from https://en.wikipedia.org/wiki/Threefold_repetition "Fischer vs. Petrosian, 1971"
    const std::string fischerPetrosianFen = "8/pp3p1k/2p2q1p/3r1P2/5R2/7P/P1P1QP2/7K b - - 0 1";

    const auto parseResult = GameState::fromFen(fischerPetrosianFen);
    ASSERT_TRUE(parseResult.has_value());

    GameState gameState = parseResult.value();
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qe5", gameState).value());
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qh5", gameState).value());
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qf6", gameState).value());
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    // First repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState).value());
    EXPECT_FALSE(gameState.isRepetition());
    // Repetition occurred 4 plies ago
    EXPECT_TRUE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Re5", gameState).value());
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qd3", gameState).value());
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Rd5", gameState).value());
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    // Second repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState).value());
    EXPECT_TRUE(gameState.isRepetition());
    EXPECT_TRUE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());
}

}  // namespace GameStateTests
