#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace GameStateTests {

TEST(GameStateTests, ThreeFoldRepetition) {
    // Position from https://en.wikipedia.org/wiki/Threefold_repetition "Fischer vs. Petrosian, 1971"
    const std::string fischerPetrosianFen = "8/pp3p1k/2p2q1p/3r1P2/5R2/7P/P1P1QP2/7K b - - 0 1";

    GameState gameState = GameState::fromFen(fischerPetrosianFen);
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qe5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qh5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qf6", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    // First repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    // Repetition occurred 4 plies ago
    EXPECT_TRUE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Re5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qd3", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Rd5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    // Second repetition
    gameState.makeMove(Move::fromAlgebraic("Qe2", gameState));
    EXPECT_TRUE(gameState.isRepetition());
    EXPECT_TRUE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());
}

TEST(GameStateTests, ThreeFoldRepetitionEnPassant) {
    // Test for situation described here: https://github.com/Disservin/fastchess/issues/940
    GameState gameState = GameState::fromFen("1qr3k1/5pp1/p3p3/3nP2Q/P6R/7P/3B2PK/8 b - - 4 31");
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("f5", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qh8+", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Kf7", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qh5+", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_FALSE(gameState.hasRepeated());

    // First repetition
    gameState.makeMove(Move::fromAlgebraic("Kg8", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_TRUE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qh7+", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Kf7", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_FALSE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    gameState.makeMove(Move::fromAlgebraic("Qh5+", gameState));
    EXPECT_FALSE(gameState.isRepetition());
    EXPECT_TRUE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());

    // Second repetition
    gameState.makeMove(Move::fromAlgebraic("Kg8", gameState));
    EXPECT_TRUE(gameState.isRepetition());
    EXPECT_TRUE(gameState.isRepetition(2));
    EXPECT_TRUE(gameState.hasRepeated());
}

TEST(GameStateTests, EnPassantTargetIsSet) {
    GameState gameState =
            GameState::fromFen("rnbqkbnr/pppppppp/8/3P4/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1");
    gameState.makeMove(Move::fromAlgebraic("c5", gameState));

    EXPECT_EQ(gameState.getEnPassantTarget(), BoardPosition::C6);
}

TEST(GameStateTests, EnPassantTargetIsNotSetBecauseNoAttacker) {
    GameState gameState = GameState::startingPosition();
    gameState.makeMove(Move::fromAlgebraic("d4", gameState));

    EXPECT_EQ(gameState.getEnPassantTarget(), BoardPosition::Invalid);
}

TEST(GameStateTests, EnPassantTargetIsNotSetBecauseCaptureUncoversCheckingRook) {
    GameState gameState = GameState::fromFen("2k5/2p5/8/r2P3K/8/8/8/8 b - - 0 1");
    gameState.makeMove(Move::fromAlgebraic("c5", gameState));

    EXPECT_EQ(gameState.getEnPassantTarget(), BoardPosition::Invalid);
}

TEST(GameStateTests, EnPassantTargetIsNotSetBecauseCaptureUncoversCheckingBishop) {
    GameState gameState = GameState::fromFen("1br3k1/5pp1/p3p3/3nP2Q/P6R/7P/3B2PK/8 b - - 0 32");
    gameState.makeMove(Move::fromAlgebraic("f5", gameState));

    EXPECT_EQ(gameState.getEnPassantTarget(), BoardPosition::Invalid);
}

TEST(GameStateTests, EnPassantTargetIsNotSetBecauseItUncoveredACheckRook) {
    GameState gameState = GameState::fromFen("1k6/rp1K4/8/2P5/8/8/8/8 b - - 0 1");
    gameState.makeMove(Move::fromAlgebraic("b5+", gameState));

    EXPECT_EQ(gameState.getEnPassantTarget(), BoardPosition::Invalid);
}

TEST(GameStateTests, EnPassantTargetIsNotSetBecauseItUncoveredACheckBishop) {
    GameState gameState = GameState::fromFen("bk6/1p6/8/2PK4/8/8/8/8 b - - 0 1");
    gameState.makeMove(Move::fromAlgebraic("b5+", gameState));

    EXPECT_EQ(gameState.getEnPassantTarget(), BoardPosition::Invalid);
}

}  // namespace GameStateTests
