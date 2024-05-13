#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace GameStateHelperTests {

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
            .pieceToMove = Piece::Pawn, .from = BoardPosition::E2, .to = BoardPosition::E4};
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
    (void)gameState.makeMove({Piece::Pawn, BoardPosition::H2, BoardPosition::H3});
    (void)gameState.makeMove({Piece::Rook, BoardPosition::H8, BoardPosition::H7});

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

}  // namespace GameStateHelperTests
