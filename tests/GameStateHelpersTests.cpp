#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace GameStateHelperTests {

TEST(GameStateHelpers, TestGetPiece) {
    EXPECT_EQ(getPiece(getColoredPiece(Piece::Bishop, Side::White)), Piece::Bishop);
    EXPECT_EQ(getPiece(getColoredPiece(Piece::Knight, Side::Black)), Piece::Knight);
}

TEST(GameStateHelpers, TestGetSide) {
    EXPECT_EQ(getSide(getColoredPiece(Piece::Bishop, Side::White)), Side::White);
    EXPECT_EQ(getSide(getColoredPiece(Piece::Knight, Side::Black)), Side::Black);
}

TEST(GameStateHelpers, TestFileRankFromPosition) {
    using FileRankT = std::pair<int, int>;
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(0, 0)), FileRankT(0, 0));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(0, 7)), FileRankT(0, 7));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(7, 0)), FileRankT(7, 0));
    EXPECT_EQ(fileRankFromPosition(positionFromFileRank(7, 7)), FileRankT(7, 7));
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
    const GameState startingPosition = GameState::startingPosition();
    const std::string startingVisual = startingPosition.toVisualString();
    const std::string expectedStartingVisual =
            "  .-------------------------------.\n"
            "8 | r*| n | b | q | k | b | n | r*|\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "7 | p | p | p | p | p | p | p | p |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "6 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "5 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "4 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "3 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "2 | P | P | P | P | P | P | P | P |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "1 | R*| N | B | Q | K | B | N | R*| w\n"
            "  '-------------------------------'\n"
            "    a   b   c   d   e   f   g   h\n";

    EXPECT_EQ(startingVisual, expectedStartingVisual);
}

TEST(GameStateHelpers, TestToVisualStringDoublePush) {
    GameState startingPosition = GameState::startingPosition();

    const Move doublePush{
            .pieceToMove = Piece::Pawn,
            .from        = positionFromAlgebraic("e2"),
            .to          = positionFromAlgebraic("e4")};
    (void)startingPosition.makeMove(doublePush);

    const std::string visual = startingPosition.toVisualString();
    const std::string expectedVisual =
            "  .-------------------------------.\n"
            "8 | r*| n | b | q | k | b | n | r*|\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "7 | p | p | p | p | p | p | p | p |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "6 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "5 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "4 |   |   |   |   | P |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "3 |   |   |   |   | * |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "2 | P | P | P | P |   | P | P | P |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "1 | R*| N | B | Q | K | B | N | R*| b\n"
            "  '-------------------------------'\n"
            "    a   b   c   d   e   f   g   h\n";

    EXPECT_EQ(visual, expectedVisual);
}

TEST(GameStateHelpers, TestToVisualStringPosition4) {
    const std::string fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";

    GameState gameState = GameState::fromFen(fen);

    const std::string visual = gameState.toVisualString();
    const std::string expectedVisual =
            "  .-------------------------------.\n"
            "8 | r*|   |   |   | k |   |   | r*|\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "7 | P | p | p | p |   | p | p | p |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "6 |   | b |   |   |   | n | b | N |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "5 | n | P |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "4 | B | B | P |   | P |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "3 | q |   |   |   |   | N |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "2 | P | p |   | P |   |   | P | P |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "1 | R |   |   | Q |   | R | K |   | w\n"
            "  '-------------------------------'\n"
            "    a   b   c   d   e   f   g   h\n";

    EXPECT_EQ(visual, expectedVisual);

    // Make a pawn move for white, then move one of the black rooks to test changing castling rights
    (void)gameState.makeMove(
            {Piece::Pawn, positionFromAlgebraic("h2"), positionFromAlgebraic("h3")});
    (void)gameState.makeMove(
            {Piece::Rook, positionFromAlgebraic("h8"), positionFromAlgebraic("h7")});

    const std::string visual2 = gameState.toVisualString();
    const std::string expectedVisual2 =
            "  .-------------------------------.\n"
            "8 | r*|   |   |   | k |   | r |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "7 | P | p | p | p |   | p | p | p |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "6 |   | b |   |   |   | n | b | N |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "5 | n | P |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "4 | B | B | P |   | P |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "3 | q |   |   |   |   | N |   | P |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "2 | P | p |   | P |   |   | P |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "1 | R |   |   | Q |   | R | K |   | w\n"
            "  '-------------------------------'\n"
            "    a   b   c   d   e   f   g   h\n";
}

TEST(GameStateHelpers, TestGetFlags) {
    EXPECT_EQ(
            getFlags(MoveFlags::IsCapture, MoveFlags::IsCastle),
            (MoveFlags)((int)MoveFlags::IsCapture | (int)MoveFlags::IsCastle));

    EXPECT_EQ(
            getFlags(MoveFlags::IsCapture, Piece::Bishop),
            (MoveFlags)((int)MoveFlags::IsCapture | (int)Piece::Bishop));
}

}  // namespace GameStateHelperTests
