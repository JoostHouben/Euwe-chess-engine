#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

TEST(GameStateHelpers, TestGetPiece) {
    ASSERT_EQ(getPiece(getColoredPiece(Piece::Bishop, Side::White)), Piece::Bishop);
    ASSERT_EQ(getPiece(getColoredPiece(Piece::Knight, Side::Black)), Piece::Knight);
    ASSERT_EQ(getPiece(getColoredPiece(Piece::King, Side::None)), Piece::King);
}

TEST(GameStateHelpers, TestGetSide) {
    ASSERT_EQ(getSide(getColoredPiece(Piece::Bishop, Side::White)), Side::White);
    ASSERT_EQ(getSide(getColoredPiece(Piece::Knight, Side::Black)), Side::Black);
    ASSERT_EQ(getSide(getColoredPiece(Piece::King, Side::None)), Side::None);
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
