#include "chess-engine-lib/Move.h"

#include "MyGTest.h"

namespace MoveTests {

TEST(GameStateHelpers, TestGetFlags) {
    EXPECT_EQ(
            getFlags(MoveFlags::IsCapture, MoveFlags::IsCastle),
            (MoveFlags)((int)MoveFlags::IsCapture | (int)MoveFlags::IsCastle));

    EXPECT_EQ(
            getFlags(MoveFlags::IsCapture, Piece::Bishop),
            (MoveFlags)((int)MoveFlags::IsCapture | (int)Piece::Bishop));
}

}  // namespace MoveTests