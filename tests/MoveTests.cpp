#include "chess-engine-lib/Move.h"

#include "MyGTest.h"

namespace MoveTests {

TEST(MoveTests, TestGetFlags) {
    EXPECT_EQ(
            getFlags(MoveFlags::IsCapture, MoveFlags::IsCastle),
            (MoveFlags)((int)MoveFlags::IsCapture | (int)MoveFlags::IsCastle));

    EXPECT_EQ(
            getFlags(MoveFlags::IsCapture, Piece::Bishop),
            (MoveFlags)((int)MoveFlags::IsCapture | (int)Piece::Bishop));
}

TEST(MoveTests, TestMoveToExtendedString) {
    EXPECT_EQ(moveToExtendedString({Piece::Pawn, BoardPosition::E2, BoardPosition::E4}), "Pe2-e4");

    EXPECT_EQ(
            moveToExtendedString(
                    {Piece::Rook, BoardPosition::D3, BoardPosition::D7, MoveFlags::IsCapture}),
            "Rd3xd7");

    EXPECT_EQ(
            moveToExtendedString(
                    {Piece::Pawn,
                     BoardPosition::E5,
                     BoardPosition::E6,
                     getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)}),
            "Pe5xe6 e.p.");

    EXPECT_EQ(
            moveToExtendedString(
                    {Piece::Pawn,
                     BoardPosition::B7,
                     BoardPosition::C8,
                     getFlags(MoveFlags::IsCapture, Piece::Queen)}),
            "Pb7xc8=Q");

    EXPECT_EQ(
            moveToExtendedString(
                    {Piece::King, BoardPosition::E8, BoardPosition::G8, MoveFlags::IsCastle}),
            "O-O");

    EXPECT_EQ(
            moveToExtendedString(
                    {Piece::King, BoardPosition::E1, BoardPosition::C1, MoveFlags::IsCastle}),
            "O-O-O");
}

TEST(MoveTests, TestMoveToUciString) {
    EXPECT_EQ(moveToUciString({Piece::Pawn, BoardPosition::E2, BoardPosition::E4}), "e2e4");

    EXPECT_EQ(
            moveToUciString(
                    {Piece::Rook, BoardPosition::D3, BoardPosition::D7, MoveFlags::IsCapture}),
            "d3d7");

    EXPECT_EQ(
            moveToUciString(
                    {Piece::Pawn,
                     BoardPosition::E5,
                     BoardPosition::E6,
                     getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)}),
            "e5e6");

    EXPECT_EQ(
            moveToUciString(
                    {Piece::Pawn,
                     BoardPosition::B7,
                     BoardPosition::C8,
                     getFlags(MoveFlags::IsCapture, Piece::Queen)}),
            "b7c8q");

    EXPECT_EQ(
            moveToUciString(
                    {Piece::King, BoardPosition::E8, BoardPosition::G8, MoveFlags::IsCastle}),
            "e8g8");

    EXPECT_EQ(
            moveToUciString(
                    {Piece::King, BoardPosition::E1, BoardPosition::C1, MoveFlags::IsCastle}),
            "e1c1");
}

}  // namespace MoveTests