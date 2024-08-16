#include "chess-engine-lib/Move.h"

#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

namespace MoveTests {

TEST(MoveTests, TestMoveToExtendedString) {
    Move move = Move{Piece::Pawn, BoardPosition::E2, BoardPosition::E4};
    EXPECT_EQ(move.toExtendedString(), "Pe2-e4");

    move = Move{Piece::Rook, BoardPosition::D3, BoardPosition::D7, MoveFlags::IsCapture};
    EXPECT_EQ(move.toExtendedString(), "Rd3xd7");

    move =
            Move{Piece::Pawn,
                 BoardPosition::E5,
                 BoardPosition::E6,
                 MoveFlags::IsCapture | MoveFlags::IsEnPassant};
    EXPECT_EQ(move.toExtendedString(), "Pe5xe6 e.p.");

    move = Move{
            Piece::Pawn, BoardPosition::B7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};
    EXPECT_EQ(move.toExtendedString(), "Pb7xc8=Q");

    move = Move{Piece::King, BoardPosition::E8, BoardPosition::G8, MoveFlags::IsCastle};
    EXPECT_EQ(move.toExtendedString(), "O-O");

    move = Move{Piece::King, BoardPosition::E1, BoardPosition::C1, MoveFlags::IsCastle};
    EXPECT_EQ(move.toExtendedString(), "O-O-O");
}

TEST(MoveTests, TestMoveToUciString) {
    Move move = Move{Piece::Pawn, BoardPosition::E2, BoardPosition::E4};
    EXPECT_EQ(move.toUci(), "e2e4");

    move = Move{Piece::Rook, BoardPosition::D3, BoardPosition::D7, MoveFlags::IsCapture};
    EXPECT_EQ(move.toUci(), "d3d7");

    move =
            Move{Piece::Pawn,
                 BoardPosition::E5,
                 BoardPosition::E6,
                 MoveFlags::IsCapture | MoveFlags::IsEnPassant};
    EXPECT_EQ(move.toUci(), "e5e6");

    move = Move{
            Piece::Pawn, BoardPosition::B7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};
    EXPECT_EQ(move.toUci(), "b7c8q");

    move = Move{Piece::King, BoardPosition::E8, BoardPosition::G8, MoveFlags::IsCastle};
    EXPECT_EQ(move.toUci(), "e8g8");

    move = Move{Piece::King, BoardPosition::E1, BoardPosition::C1, MoveFlags::IsCastle};
    EXPECT_EQ(move.toUci(), "e1c1");
}

TEST(MoveTests, TestMoveFromUciString) {
    // Position 5 from https://www.chessprogramming.org/Perft_Results
    const GameState gameState =
            GameState::fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

    const Move parsedPromotionCapture   = Move::fromUci("d7c8q", gameState);
    const Move expectedPromotionCapture = Move{
            Piece::Pawn, BoardPosition::D7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};

    EXPECT_EQ(parsedPromotionCapture, expectedPromotionCapture);

    const Move parsedCastle = Move::fromUci("e1g1", gameState);
    const Move expectedCastle =
            Move{Piece::King, BoardPosition::E1, BoardPosition::G1, MoveFlags::IsCastle};

    EXPECT_EQ(parsedCastle, expectedCastle);
}

TEST(MoveTests, TestMoveFromUciStringErrorHandling) {
    const GameState gameState = GameState::startingPosition();

    // string too short
    EXPECT_THROW((void)Move::fromUci("e2", gameState), std::invalid_argument);

    // string too long
    EXPECT_THROW((void)Move::fromUci("e2e4e4", gameState), std::invalid_argument);

    // invalid square
    EXPECT_THROW((void)Move::fromUci("e9e4", gameState), std::invalid_argument);
}

TEST(MoveTests, TestBasicSanityChecks) {
    GameState gameState = GameState::startingPosition();

    // valid
    {
        const Move move = Move::fromUci("e2e4", gameState);
        EXPECT_NO_THROW(doBasicSanityChecks(move, gameState));
    }

    // no piece on position
    {
        const Move move = Move::fromUci("e3e4", gameState);
        EXPECT_THROW(doBasicSanityChecks(move, gameState), std::invalid_argument);
    }

    // side not to move
    {
        const Move move = Move::fromUci("e7e5", gameState);
        EXPECT_THROW(doBasicSanityChecks(move, gameState), std::invalid_argument);
    }

    // trying to capture own piece
    {
        const Move move{Piece::King, BoardPosition::E1, BoardPosition::E2, MoveFlags::IsCapture};
        EXPECT_THROW(doBasicSanityChecks(move, gameState), std::invalid_argument);
    }

    // blocked by own piece
    {
        const Move move{Piece::King, BoardPosition::E1, BoardPosition::E2, MoveFlags::None};
        EXPECT_THROW(doBasicSanityChecks(move, gameState), std::invalid_argument);
    }

    // nothing to capture
    {
        const Move move{Piece::Pawn, BoardPosition::E2, BoardPosition::D3, MoveFlags::IsCapture};
        EXPECT_THROW(doBasicSanityChecks(move, gameState), std::invalid_argument);
    }

    // promoting non-pawn
    {
        const Move move = Move::fromUci("b2b3q", gameState);
        EXPECT_THROW(doBasicSanityChecks(move, gameState), std::invalid_argument);
    }

    // promoting pawn not on last rank
    {
        const Move move = Move::fromUci("e2e4q", gameState);
        EXPECT_THROW(doBasicSanityChecks(move, gameState), std::invalid_argument);
    }

    // Valid capture and promotion
    {
        // Position 5 from https://www.chessprogramming.org/Perft_Results
        const GameState position5 =
                GameState::fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
        const Move move = Move::fromUci("d7c8q", position5);
        EXPECT_NO_THROW(doBasicSanityChecks(move, position5));
    }
}

TEST(MoveTests, TestMoveFromUciStringEnPassant) {
    // Position 3 from https://www.chessprogramming.org/Perft_Results
    GameState gameState = GameState::fromFen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1");

    gameState.makeMove({Piece::Pawn, BoardPosition::E2, BoardPosition::E4});

    const Move parsedEnPassantCapture = Move::fromUci("f4e3", gameState);
    const Move expectedEnPassantCapture =
            Move{Piece::Pawn,
                 BoardPosition::F4,
                 BoardPosition::E3,
                 MoveFlags::IsCapture | MoveFlags::IsEnPassant};

    EXPECT_EQ(parsedEnPassantCapture, expectedEnPassantCapture);
}

TEST(AlgebraicNotation, TestAlgebraicFromMove) {
    const GameState startingPosition = GameState::startingPosition();

    std::string pawnPush =
            Move{Piece::Pawn, BoardPosition::E2, BoardPosition::E3}.toAlgebraic(startingPosition);
    EXPECT_EQ(pawnPush, "e3");

    std::string pawnDoublePush =
            Move{Piece::Pawn, BoardPosition::E2, BoardPosition::E4}.toAlgebraic(startingPosition);
    EXPECT_EQ(pawnDoublePush, "e4");

    std::string knightMove =
            Move{Piece::Knight, BoardPosition::G1, BoardPosition::F3}.toAlgebraic(startingPosition);
    EXPECT_EQ(knightMove, "Nf3");
}

TEST(AlgebraicNotation, TestAlgebraicFromMovePosition5) {
    // Position 5 from https://www.chessprogramming.org/Perft_Results
    const GameState gameState =
            GameState::fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

    const Move promotionCapture{
            Piece::Pawn, BoardPosition::D7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};
    const std::string promotionCaptureAlgebraic = promotionCapture.toAlgebraic(gameState);
    EXPECT_EQ(promotionCaptureAlgebraic, "dxc8=Q");

    const Move castle{Piece::King, BoardPosition::E1, BoardPosition::G1, MoveFlags::IsCastle};
    const std::string castleAlgebraic = castle.toAlgebraic(gameState);
    EXPECT_EQ(castleAlgebraic, "O-O");

    const Move ambiguousKnightMove{Piece::Knight, BoardPosition::B1, BoardPosition::C3};
    const std::string ambiguousKnightMoveAlgebraic = ambiguousKnightMove.toAlgebraic(gameState);
    EXPECT_EQ(ambiguousKnightMoveAlgebraic, "Nbc3");

    const Move ambiguousKnightMove2{Piece::Knight, BoardPosition::E2, BoardPosition::C3};
    const std::string ambiguousKnightMoveAlgebraic2 = ambiguousKnightMove2.toAlgebraic(gameState);
    EXPECT_EQ(ambiguousKnightMoveAlgebraic2, "Nec3");
}

TEST(AlgebraicNotation, AlgebraicFromMoveNotAmbiguousBecauseOfPin) {
    // From https://chess.stackexchange.com/questions/1864/is-this-case-considered-an-ambiguity-or-not
    const GameState gameState = GameState::fromFen("4k3/8/8/8/8/8/8/q2NKN2 w - - 0 1");

    const Move knightMove{Piece::Knight, BoardPosition::F1, BoardPosition::E3};
    const std::string knightMoveAlgebraic = knightMove.toAlgebraic(gameState);
    EXPECT_EQ(knightMoveAlgebraic, "Ne3");
}

TEST(AlgebraicNotation, AlgebraicFromMoveAmbiguousEvenWithCheck) {
    // From https://chess.stackexchange.com/questions/2777/parsing-pgn-files-when-can-a-move-be-shortened
    const GameState gameStateKnights = GameState::fromFen("1q5k/1n6/8/8/1K2n3/8/8/8 b - - 0 1");

    const Move knightMoveCheck{Piece::Knight, BoardPosition::B7, BoardPosition::C5};
    const std::string knightMoveCheckAlgebraic = knightMoveCheck.toAlgebraic(gameStateKnights);
    EXPECT_EQ(knightMoveCheckAlgebraic, "Nbc5+");

    const Move knightMoveNoCheck{Piece::Knight, BoardPosition::E4, BoardPosition::C5};
    const std::string knightMoveNoCheckAlgebraic = knightMoveNoCheck.toAlgebraic(gameStateKnights);
    EXPECT_EQ(knightMoveNoCheckAlgebraic, "Nec5");
}

TEST(AlgebraicNotation, AlgebraicFromMoveAmbiguousEvenWithCheckMate) {
    // From https://chess.stackexchange.com/questions/2777/parsing-pgn-files-when-can-a-move-be-shortened
    const GameState gameStateRooks = GameState::fromFen("6r1/7K/q7/8/8/8/8/k5r1 b - - 0 1");

    const Move rookMoveCheck{Piece::Rook, BoardPosition::G8, BoardPosition::G7};
    const std::string rookMoveCheckAlgebraic = rookMoveCheck.toAlgebraic(gameStateRooks);
    EXPECT_EQ(rookMoveCheckAlgebraic, "R8g7+");

    const Move rookMoveCheckMate{Piece::Rook, BoardPosition::G1, BoardPosition::G7};
    const std::string rookMoveCheckMateAlgebraic = rookMoveCheckMate.toAlgebraic(gameStateRooks);
    EXPECT_EQ(rookMoveCheckMateAlgebraic, "R1g7#");
}

TEST(AlgebraicNotation, AlgebraicFromMoveRankFileAmbiguous) {
    const GameState gameState = GameState::fromFen("k7/8/8/2Q1Q3/8/2Q1Q3/8/K7 w - - 0 1");

    const Move queenMoveC3{Piece::Queen, BoardPosition::C3, BoardPosition::D4};
    const std::string queenMoveC3Algebraic = queenMoveC3.toAlgebraic(gameState);
    EXPECT_EQ(queenMoveC3Algebraic, "Qc3d4");

    const Move queenMoveC5{Piece::Queen, BoardPosition::C5, BoardPosition::D4};
    const std::string queenMoveC5Algebraic = queenMoveC5.toAlgebraic(gameState);
    EXPECT_EQ(queenMoveC5Algebraic, "Qc5d4");

    const Move queenMoveE3{Piece::Queen, BoardPosition::E3, BoardPosition::D4};
    const std::string queenMoveE3Algebraic = queenMoveE3.toAlgebraic(gameState);
    EXPECT_EQ(queenMoveE3Algebraic, "Qe3d4");

    const Move queenMoveE5{Piece::Queen, BoardPosition::E5, BoardPosition::D4};
    const std::string queenMoveE5Algebraic = queenMoveE5.toAlgebraic(gameState);
    EXPECT_EQ(queenMoveE5Algebraic, "Qe5d4");
}

TEST(AlgebraicNotation, TestMoveFromAlgebraicPosition5) {
    // Position 5 from https://www.chessprogramming.org/Perft_Results
    const GameState gameState =
            GameState::fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");

    const Move promotionCapture = Move::fromAlgebraic("dxc8=Q", gameState);
    const Move expectedPromotionCapture{
            Piece::Pawn, BoardPosition::D7, BoardPosition::C8, MoveFlags::IsCapture | Piece::Queen};

    EXPECT_EQ(promotionCapture, expectedPromotionCapture);

    const Move castle = Move::fromAlgebraic("O-O", gameState);
    const Move expectedCastle{
            Piece::King, BoardPosition::E1, BoardPosition::G1, MoveFlags::IsCastle};
    EXPECT_EQ(castle, expectedCastle);

    const Move ambiguousKnightMove = Move::fromAlgebraic("Nbc3", gameState);
    const Move expectedAmbiguousKnightMove{Piece::Knight, BoardPosition::B1, BoardPosition::C3};
    EXPECT_EQ(ambiguousKnightMove, expectedAmbiguousKnightMove);

    const Move ambiguousKnightMove2 = Move::fromAlgebraic("Nec3", gameState);
    const Move expectedAmbiguousKnightMove2{Piece::Knight, BoardPosition::E2, BoardPosition::C3};
    EXPECT_EQ(ambiguousKnightMove2, expectedAmbiguousKnightMove2);

    // Not a valid move
    EXPECT_THROW((void)Move::fromAlgebraic("a5", gameState), std::invalid_argument);
    EXPECT_THROW((void)Move::fromAlgebraic("dxc8=Q+", gameState), std::invalid_argument);
}

}  // namespace MoveTests