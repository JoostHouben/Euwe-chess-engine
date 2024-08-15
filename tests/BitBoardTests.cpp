#include "chess-engine-lib/BitBoard.h"

#include "MyGTest.h"

namespace BitBoardTests {

TEST(BitBoardTests, TestBitBoardToVisualString) {
    BitBoard bitBoard = BitBoard::Empty;

    const std::string emptyBitBoardVisualString = bitBoardToVisualString(bitBoard);
    const std::string expectedEmptyBoardVisual =
            "  .-------------------------------.\n"
            "8 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "7 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "6 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "5 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "4 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "3 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "2 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "1 |   |   |   |   |   |   |   |   |\n"
            "  '-------------------------------'\n"
            "    a   b   c   d   e   f   g   h\n";

    EXPECT_EQ(emptyBitBoardVisualString, expectedEmptyBoardVisual);

    bitBoard |= BoardPosition::C4;

    const std::string bitBoardVisualString = bitBoardToVisualString(bitBoard);
    const std::string expectedVisualWithC4 =
            "  .-------------------------------.\n"
            "8 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "7 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "6 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "5 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "4 |   |   | X |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "3 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "2 |   |   |   |   |   |   |   |   |\n"
            "  |---+---+---+---+---+---+---+---|\n"
            "1 |   |   |   |   |   |   |   |   |\n"
            "  '-------------------------------'\n"
            "    a   b   c   d   e   f   g   h\n";

    EXPECT_EQ(bitBoardVisualString, expectedVisualWithC4);

    bitBoard &= ~BoardPosition::C4;

    const std::string bitBoardVisualStringAfterClear = bitBoardToVisualString(bitBoard);

    EXPECT_EQ(bitBoardVisualStringAfterClear, expectedEmptyBoardVisual);
}

}  // namespace BitBoardTests
