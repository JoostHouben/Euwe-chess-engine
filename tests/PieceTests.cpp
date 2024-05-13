#include "chess-engine-lib/Piece.h"

#include "MyGTest.h"

namespace PieceTests {

TEST(GameStateHelpers, TestGetPiece) {
    EXPECT_EQ(getPiece(getColoredPiece(Piece::Bishop, Side::White)), Piece::Bishop);
    EXPECT_EQ(getPiece(getColoredPiece(Piece::Knight, Side::Black)), Piece::Knight);
}

TEST(GameStateHelpers, TestGetSide) {
    EXPECT_EQ(getSide(getColoredPiece(Piece::Bishop, Side::White)), Side::White);
    EXPECT_EQ(getSide(getColoredPiece(Piece::Knight, Side::Black)), Side::Black);
}

}  // namespace PieceTests