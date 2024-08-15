#include "chess-engine-lib/Move.h"

#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace MoveTests {

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
                     MoveFlags::IsCapture | MoveFlags::IsEnPassant}),
            "Pe5xe6 e.p.");

    EXPECT_EQ(
            moveToExtendedString(
                    {Piece::Pawn,
                     BoardPosition::B7,
                     BoardPosition::C8,
                     MoveFlags::IsCapture | Piece::Queen}),
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
                     MoveFlags::IsCapture | MoveFlags::IsEnPassant}),
            "e5e6");

    EXPECT_EQ(
            moveToUciString(
                    {Piece::Pawn,
                     BoardPosition::B7,
                     BoardPosition::C8,
                     MoveFlags::IsCapture | Piece::Queen}),
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

    const Move parsedPromotionCapture   = moveFromUciString("d7c8q", gameState);
    const Move expectedPromotionCapture = Move{
            Piece::Pawn, BoardPosition::D7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};

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
                 MoveFlags::IsCapture | MoveFlags::IsEnPassant};

    EXPECT_EQ(parsedEnPassantCapture, expectedEnPassantCapture);
}

TEST(AlgebraicNotation, TestAlgebraicFromMove) {
    const GameState startingPosition = GameState::startingPosition();

    std::string pawnPush = algebraicFromMove(
            {Piece::Pawn, BoardPosition::E2, BoardPosition::E3}, startingPosition);
    EXPECT_EQ(pawnPush, "e3");

    std::string pawnDoublePush = algebraicFromMove(
            {Piece::Pawn, BoardPosition::E2, BoardPosition::E4}, startingPosition);
    EXPECT_EQ(pawnDoublePush, "e4");

    std::string knightMove = algebraicFromMove(
            {Piece::Knight, BoardPosition::G1, BoardPosition::F3}, startingPosition);
    EXPECT_EQ(knightMove, "Nf3");
}

TEST(AlgebraicNotation, TestAlgebraicFromMovePosition5) {
    // Position 5 from https://www.chessprogramming.org/Perft_Results
    const GameState gameState =
            GameState::fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

    const Move promotionCapture{
            Piece::Pawn, BoardPosition::D7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};
    const std::string promotionCaptureAlgebraic = algebraicFromMove(promotionCapture, gameState);
    EXPECT_EQ(promotionCaptureAlgebraic, "dxc8=Q");

    const Move castle{Piece::King, BoardPosition::E1, BoardPosition::G1, MoveFlags::IsCastle};
    const std::string castleAlgebraic = algebraicFromMove(castle, gameState);
    EXPECT_EQ(castleAlgebraic, "O-O");

    const Move ambiguousKnightMove{Piece::Knight, BoardPosition::B1, BoardPosition::C3};
    const std::string ambiguousKnightMoveAlgebraic =
            algebraicFromMove(ambiguousKnightMove, gameState);
    EXPECT_EQ(ambiguousKnightMoveAlgebraic, "Nbc3");

    const Move ambiguousKnightMove2{Piece::Knight, BoardPosition::E2, BoardPosition::C3};
    const std::string ambiguousKnightMoveAlgebraic2 =
            algebraicFromMove(ambiguousKnightMove2, gameState);
    EXPECT_EQ(ambiguousKnightMoveAlgebraic2, "Nec3");
}

TEST(AlgebraicNotation, AlgebraicFromMoveNotAmbiguousBecauseOfPin) {
    // From https://chess.stackexchange.com/questions/1864/is-this-case-considered-an-ambiguity-or-not
    const GameState gameState = GameState::fromFen("4k3/8/8/8/8/8/8/q2NKN2 w - - 0 1");

    const Move knightMove{Piece::Knight, BoardPosition::F1, BoardPosition::E3};
    const std::string knightMoveAlgebraic = algebraicFromMove(knightMove, gameState);
    EXPECT_EQ(knightMoveAlgebraic, "Ne3");
}

TEST(AlgebraicNotation, AlgebraicFromMoveAmbiguousEvenWithCheck) {
    // From https://chess.stackexchange.com/questions/2777/parsing-pgn-files-when-can-a-move-be-shortened
    const GameState gameStateKnights = GameState::fromFen("1q5k/1n6/8/8/1K2n3/8/8/8 b - - 0 1");

    const Move knightMoveCheck{Piece::Knight, BoardPosition::B7, BoardPosition::C5};
    const std::string knightMoveCheckAlgebraic =
            algebraicFromMove(knightMoveCheck, gameStateKnights);
    EXPECT_EQ(knightMoveCheckAlgebraic, "Nbc5+");

    const Move knightMoveNoCheck{Piece::Knight, BoardPosition::E4, BoardPosition::C5};
    const std::string knightMoveNoCheckAlgebraic =
            algebraicFromMove(knightMoveNoCheck, gameStateKnights);
    EXPECT_EQ(knightMoveNoCheckAlgebraic, "Nec5");
}

TEST(AlgebraicNotation, AlgebraicFromMoveAmbiguousEvenWithCheckMate) {
    // From https://chess.stackexchange.com/questions/2777/parsing-pgn-files-when-can-a-move-be-shortened
    const GameState gameStateRooks = GameState::fromFen("6r1/7K/q7/8/8/8/8/k5r1 b - - 0 1");

    const Move rookMoveCheck{Piece::Rook, BoardPosition::G8, BoardPosition::G7};
    const std::string rookMoveCheckAlgebraic = algebraicFromMove(rookMoveCheck, gameStateRooks);
    EXPECT_EQ(rookMoveCheckAlgebraic, "R8g7+");

    const Move rookMoveCheckMate{Piece::Rook, BoardPosition::G1, BoardPosition::G7};
    const std::string rookMoveCheckMateAlgebraic =
            algebraicFromMove(rookMoveCheckMate, gameStateRooks);
    EXPECT_EQ(rookMoveCheckMateAlgebraic, "R1g7#");
}

TEST(AlgebraicNotation, AlgebraicFromMoveRankFileAmbiguous) {
    const GameState gameState = GameState::fromFen("k7/8/8/2Q1Q3/8/2Q1Q3/8/K7 w - - 0 1");

    const Move queenMoveC3{Piece::Queen, BoardPosition::C3, BoardPosition::D4};
    const std::string queenMoveC3Algebraic = algebraicFromMove(queenMoveC3, gameState);
    EXPECT_EQ(queenMoveC3Algebraic, "Qc3d4");

    const Move queenMoveC5{Piece::Queen, BoardPosition::C5, BoardPosition::D4};
    const std::string queenMoveC5Algebraic = algebraicFromMove(queenMoveC5, gameState);
    EXPECT_EQ(queenMoveC5Algebraic, "Qc5d4");

    const Move queenMoveE3{Piece::Queen, BoardPosition::E3, BoardPosition::D4};
    const std::string queenMoveE3Algebraic = algebraicFromMove(queenMoveE3, gameState);
    EXPECT_EQ(queenMoveE3Algebraic, "Qe3d4");

    const Move queenMoveE5{Piece::Queen, BoardPosition::E5, BoardPosition::D4};
    const std::string queenMoveE5Algebraic = algebraicFromMove(queenMoveE5, gameState);
    EXPECT_EQ(queenMoveE5Algebraic, "Qe5d4");
}

TEST(AlgebraicNotation, TestMoveFromAlgebraicPosition5) {
    // Position 5 from https://www.chessprogramming.org/Perft_Results
    const GameState gameState =
            GameState::fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

    const Move promotionCapture = moveFromAlgebraic("dxc8=Q", gameState);
    const Move expectedPromotionCapture{
            Piece::Pawn, BoardPosition::D7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};

    EXPECT_EQ(promotionCapture, expectedPromotionCapture);

    const Move castle = moveFromAlgebraic("O-O", gameState);
    const Move expectedCastle{
            Piece::King, BoardPosition::E1, BoardPosition::G1, MoveFlags::IsCastle};
    EXPECT_EQ(castle, expectedCastle);

    const Move ambiguousKnightMove = moveFromAlgebraic("Nbc3", gameState);
    const Move expectedAmbiguousKnightMove{Piece::Knight, BoardPosition::B1, BoardPosition::C3};
    EXPECT_EQ(ambiguousKnightMove, expectedAmbiguousKnightMove);

    const Move ambiguousKnightMove2 = moveFromAlgebraic("Nec3", gameState);
    const Move expectedAmbiguousKnightMove2{Piece::Knight, BoardPosition::E2, BoardPosition::C3};
    EXPECT_EQ(ambiguousKnightMove2, expectedAmbiguousKnightMove2);
}

}  // namespace MoveTests