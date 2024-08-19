#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace GameStateTests {

TEST(GameStateTests, ThreeFoldRepetition) {
    // Position from https://en.wikipedia.org/wiki/Threefold_repetition "Fischer vs. Petrosian, 1971"
    const std::string fischerPetrosianFen = "8/pp3p1k/2p2q1p/3r1P2/5R2/7P/P1P1QP2/7K b - - 0 1";

    GameState gameState = GameState::fromFen(fischerPetrosianFen);
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    EXPECT_FALSE(gameState.isRepetitionForSearch(10));

    gameState.makeMove(Move::fromAlgebraic("Qe5", gameState));
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    EXPECT_FALSE(gameState.isRepetitionForSearch(10));

    gameState.makeMove(Move::fromAlgebraic("Qh5", gameState));
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    EXPECT_FALSE(gameState.isRepetitionForSearch(10));

    gameState.makeMove(Move::fromAlgebraic("Qf6", gameState));
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    EXPECT_FALSE(gameState.isRepetitionForSearch(10));

    // First repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState));
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    // Repetition occurred 4 plies ago
    EXPECT_FALSE(gameState.isRepetitionForSearch(3));
    EXPECT_TRUE(gameState.isRepetitionForSearch(4));

    gameState.makeMove(Move::fromAlgebraic("Re5", gameState));
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    EXPECT_FALSE(gameState.isRepetitionForSearch(10));

    gameState.makeMove(Move::fromAlgebraic("Qd3", gameState));
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    EXPECT_FALSE(gameState.isRepetitionForSearch(10));

    gameState.makeMove(Move::fromAlgebraic("Rd5", gameState));
    EXPECT_FALSE(gameState.isThreeFoldRepetition());
    EXPECT_FALSE(gameState.isRepetitionForSearch(10));

    // Second repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState));
    EXPECT_TRUE(gameState.isThreeFoldRepetition());
    // Three-fold repetition, so should treat it as such even at ply 0
    EXPECT_TRUE(gameState.isRepetitionForSearch(0));
}

}  // namespace GameStateTests