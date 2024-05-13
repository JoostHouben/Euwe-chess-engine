#include "chess-engine-lib/Move.h"

#include "chess-engine-lib/GameState.h"

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

TEST(MoveTests, TestMoveFromUciString) {
    // Position 5 from https://www.chessprogramming.org/Perft_Results
    const GameState gameState =
            GameState::fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

    const Move parsedPromotionCapture = moveFromUciString("d7c8q", gameState);
    const Move expectedPromotionCapture =
            Move{Piece::Pawn,
                 BoardPosition::D7,
                 BoardPosition::C8,
                 getFlags(MoveFlags::IsCapture, Piece::Queen)};

    EXPECT_EQ(parsedPromotionCapture, expectedPromotionCapture);

    const Move parsedCastle = moveFromUciString("e1g1", gameState);
    const Move expectedCastle =
            Move{Piece::King, BoardPosition::E1, BoardPosition::G1, MoveFlags::IsCastle};

    EXPECT_EQ(parsedCastle, expectedCastle);
}

TEST(MoveTests, TestMoveFromUciStringEnPassant) {
    // Position 3 from https://www.chessprogramming.org/Perft_Results
    GameState gameState = GameState::fromFen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1");

    gameState.makeMove({Piece::Pawn, BoardPosition::E2, BoardPosition::E4});

    const Move parsedEnPassantCapture = moveFromUciString("f4e3", gameState);
    const Move expectedEnPassantCapture =
            Move{Piece::Pawn,
                 BoardPosition::F4,
                 BoardPosition::E3,
                 getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)};

    EXPECT_EQ(parsedEnPassantCapture, expectedEnPassantCapture);
}

}  // namespace MoveTests