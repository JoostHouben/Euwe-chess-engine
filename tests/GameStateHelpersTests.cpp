#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

TEST(GameStateHelpers, TestGetPiece) {
    ASSERT_EQ(getPiece(getColoredPiece(Bishop, White)), Bishop);
    ASSERT_EQ(getPiece(getColoredPiece(Knight, Black)), Knight);
    ASSERT_EQ(getPiece(getColoredPiece(King, None)), King);
}

TEST(GameStateHelpers, TestGetSide) {
    ASSERT_EQ(getSide(getColoredPiece(Bishop, White)), White);
    ASSERT_EQ(getSide(getColoredPiece(Knight, Black)), Black);
    ASSERT_EQ(getSide(getColoredPiece(King, None)), None);
}

TEST(GameStateHelpers, TestFileRankFromPosition) {
    using FileRankT = std::pair<int, int>;
    ASSERT_EQ(fileRankFromPosition(positionFromFileRank(0, 0)), FileRankT( 0, 0 ));
    ASSERT_EQ(fileRankFromPosition(positionFromFileRank(0, 7)), FileRankT( 0, 7 ));
    ASSERT_EQ(fileRankFromPosition(positionFromFileRank(7, 0)), FileRankT( 7, 0 ));
    ASSERT_EQ(fileRankFromPosition(positionFromFileRank(7, 7)), FileRankT( 7, 7 ));
}

TEST(GameStateHelpers, TestPositionFromAlgebraic) {
    ASSERT_EQ(positionFromAlgebraic("a1"), positionFromFileRank(0, 0));
    ASSERT_EQ(positionFromAlgebraic("a8"), positionFromFileRank(0, 7));
    ASSERT_EQ(positionFromAlgebraic("h1"), positionFromFileRank(7, 0));
    ASSERT_EQ(positionFromAlgebraic("h8"), positionFromFileRank(7, 7));
}

TEST(GameStateHelpers, TestAlgebraicFromPosition) {
    ASSERT_EQ(algebraicFromPosition(positionFromFileRank(0, 0)), "a1");
    ASSERT_EQ(algebraicFromPosition(positionFromFileRank(0, 7)), "a8");
    ASSERT_EQ(algebraicFromPosition(positionFromFileRank(7, 0)), "h1");
    ASSERT_EQ(algebraicFromPosition(positionFromFileRank(7, 7)), "h8");
}
