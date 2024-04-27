#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

#include <set>

TEST(FenParsing, TestStartingPosition) {
    GameState startingPosition = GameState::startingPosition();

    std::set<std::pair<ColoredPiece, std::string>> expectedPiecesAlgebraic {
        {Rook | White, "a1"},
        {Knight | White, "b1"},
        {Bishop | White, "c1"},
        {Queen | White, "d1"},
        {King | White, "e1"},
        {Bishop | White, "f1"},
        {Knight | White, "g1"},
        {Rook | White, "h1"},

        {Pawn | White, "a2"},
        {Pawn | White, "b2"},
        {Pawn | White, "c2"},
        {Pawn | White, "d2"},
        {Pawn | White, "e2"},
        {Pawn | White, "f2"},
        {Pawn | White, "g2"},
        {Pawn | White, "h2"},

        {Pawn | Black, "a7"},
        {Pawn | Black, "b7"},
        {Pawn | Black, "c7"},
        {Pawn | Black, "d7"},
        {Pawn | Black, "e7"},
        {Pawn | Black, "f7"},
        {Pawn | Black, "g7"},
        {Pawn | Black, "h7"},

        {Rook | Black, "a8"},
        {Knight | Black, "b8"},
        {Bishop | Black, "c8"},
        {Queen | Black, "d8"},
        {King | Black, "e8"},
        {Bishop | Black, "f8"},
        {Knight | Black, "g8"},
        {Rook | Black, "h8"},
    };

    std::set<PiecePosition> expectedPieces;
    for (const auto& [piece, algebraic] : expectedPiecesAlgebraic) {
        expectedPieces.insert({ piece, positionFromAlgebraic(algebraic) });
    }

    std::set<PiecePosition> actualPieces;
    for (const auto& piecePosition : startingPosition.getPieces()) {
        actualPieces.insert(piecePosition);
    }

    ASSERT_EQ(expectedPieces, actualPieces);

    ASSERT_EQ(startingPosition.getSideToMove(), White);

    ASSERT_EQ(startingPosition.canCastleKingSide(White), true);
    ASSERT_EQ(startingPosition.canCastleQueenSide(White), true);
    ASSERT_EQ(startingPosition.canCastleKingSide(Black), true);
    ASSERT_EQ(startingPosition.canCastleQueenSide(Black), true);

    ASSERT_EQ(startingPosition.getEnPassantTarget(), kInvalidPosition);

    ASSERT_EQ(startingPosition.getPlySinceCaptureOrPawn(), 0);
}