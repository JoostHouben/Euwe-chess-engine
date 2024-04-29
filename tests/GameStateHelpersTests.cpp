#include "chess-engine-lib/GameState.h"

#pragma warning(disable : 26495)
#include "gtest/gtest.h"

namespace GameStateHelperTests {

TEST(GameStateHelpers, TestGetPiece) {
    EXPECT_EQ(getPiece(getColoredPiece(Piece::Bishop, Side::White)),
              Piece::Bishop);
    EXPECT_EQ(getPiece(getColoredPiece(Piece::Knight, Side::Black)),
              Piece::Knight);
    EXPECT_EQ(getPiece(getColoredPiece(Piece::King, Side::None)), Piece::King);
}

TEST(GameStateHelpers, TestGetSide) {
    EXPECT_EQ(getSide(getColoredPiece(Piece::Bishop, Side::White)),
              Side::White);
    EXPECT_EQ(getSide(getColoredPiece(Piece::Knight, Side::Black)),
              Side::Black);
    EXPECT_EQ(getSide(getColoredPiece(Piece::King, Side::None)), Side::None);
}

TEST(GameStateHelpers, TestFileRankFromPosition) {
    using FileRankT = std::pair<int, int>;
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(0, 0)),
              FileRankT(0, 0));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(0, 7)),
              FileRankT(0, 7));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(7, 0)),
              FileRankT(7, 0));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(7, 7)),
              FileRankT(7, 7));
}

TEST(GameStateHelpers, TestPositionFromAlgebraic) {
    EXPECT_EQ(positionFromAlgebraic("a1"), positionFromFileRank(0, 0));
    EXPECT_EQ(positionFromAlgebraic("a8"), positionFromFileRank(0, 7));
    EXPECT_EQ(positionFromAlgebraic("h1"), positionFromFileRank(7, 0));
    EXPECT_EQ(positionFromAlgebraic("h8"), positionFromFileRank(7, 7));
}

TEST(GameStateHelpers, TestAlgebraicFromPosition) {
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(0, 0)), "a1");
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(0, 7)), "a8");
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(7, 0)), "h1");
    EXPECT_EQ(algebraicFromPosition(positionFromFileRank(7, 7)), "h8");
}

TEST(GameStateHelpers, TestToVisualString) {
    GameState startingPosition = GameState::startingPosition();
    std::string startingVisual = startingPosition.toVisualString();
    std::string expectedStartingVisual =
            "  ---------------------------------\n"
            "8 | r | n | b | q | k | b | n | r |\n"
            "  |-------------------------------|\n"
            "7 | p | p | p | p | p | p | p | p |\n"
            "  |-------------------------------|\n"
            "6 |   |   |   |   |   |   |   |   |\n"
            "  |-------------------------------|\n"
            "5 |   |   |   |   |   |   |   |   |\n"
            "  |-------------------------------|\n"
            "4 |   |   |   |   |   |   |   |   |\n"
            "  |-------------------------------|\n"
            "3 |   |   |   |   |   |   |   |   |\n"
            "  |-------------------------------|\n"
            "2 | P | P | P | P | P | P | P | P |\n"
            "  |-------------------------------|\n"
            "1 | R | N | B | Q | K | B | N | R |\n"
            "  ---------------------------------\n"
            "    a   b   c   d   e   f   g   h\n";

    EXPECT_EQ(startingVisual, expectedStartingVisual);
}

TEST(GameStateHelpers, TestGetFlags) {
    EXPECT_EQ(
            getFlags(MoveFlags::IsCapture, MoveFlags::IsCastle),
            (MoveFlags)((int)MoveFlags::IsCapture | (int)MoveFlags::IsCastle));

    EXPECT_EQ(getFlags(MoveFlags::IsCapture, Piece::Bishop),
              (MoveFlags)((int)MoveFlags::IsCapture | (int)Piece::Bishop));
}

}  // namespace GameStateHelperTests
