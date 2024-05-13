#include "chess-engine-lib/BoardPosition.h"

#include "MyGTest.h"

namespace BoardPositionTests {

TEST(BoardPositionTests, TestFileRankFromPosition) {
    using FileRankT = std::pair<int, int>;
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(0, 0)), FileRankT(0, 0));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(0, 7)), FileRankT(0, 7));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(7, 0)), FileRankT(7, 0));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(7, 7)), FileRankT(7, 7));
}

TEST(BoardPositionTests, TestPositionFromAlgebraic) {
    EXPECT_EQ(positionFromAlgebraic("a1"), positionFromFileRank(0, 0));
    EXPECT_EQ(positionFromAlgebraic("a8"), positionFromFileRank(0, 7));
    EXPECT_EQ(positionFromAlgebraic("h1"), positionFromFileRank(7, 0));
    EXPECT_EQ(positionFromAlgebraic("h8"), positionFromFileRank(7, 7));
}

TEST(BoardPositionTests, TestAlgebraicFromPosition) {
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(0, 0)), "a1");
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(0, 7)), "a8");
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(7, 0)), "h1");
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(7, 7)), "h8");
}

}  // namespace BoardPositionTests