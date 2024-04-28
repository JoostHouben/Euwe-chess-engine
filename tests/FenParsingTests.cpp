#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

#include <set>
#include <string>
#include <vector>

TEST(FenParsing, TestStartingPosition) {
    GameState startingPosition = GameState::startingPosition();

    std::set<std::pair<ColoredPiece, std::string>> expectedPiecesAlgebraic {
        {getColoredPiece(Rook, White), "a1"},
        {getColoredPiece(Knight, White), "b1"},
        {getColoredPiece(Bishop, White), "c1"},
        {getColoredPiece(Queen, White), "d1"},
        {getColoredPiece(King, White), "e1"},
        {getColoredPiece(Bishop, White), "f1"},
        {getColoredPiece(Knight, White), "g1"},
        {getColoredPiece(Rook, White), "h1"},

        {getColoredPiece(Pawn, White), "a2"},
        {getColoredPiece(Pawn, White), "b2"},
        {getColoredPiece(Pawn, White), "c2"},
        {getColoredPiece(Pawn, White), "d2"},
        {getColoredPiece(Pawn, White), "e2"},
        {getColoredPiece(Pawn, White), "f2"},
        {getColoredPiece(Pawn, White), "g2"},
        {getColoredPiece(Pawn, White), "h2"},

        {getColoredPiece(Pawn, Black), "a7"},
        {getColoredPiece(Pawn, Black), "b7"},
        {getColoredPiece(Pawn, Black), "c7"},
        {getColoredPiece(Pawn, Black), "d7"},
        {getColoredPiece(Pawn, Black), "e7"},
        {getColoredPiece(Pawn, Black), "f7"},
        {getColoredPiece(Pawn, Black), "g7"},
        {getColoredPiece(Pawn, Black), "h7"},

        {getColoredPiece(Rook, Black), "a8"},
        {getColoredPiece(Knight, Black), "b8"},
        {getColoredPiece(Bishop, Black), "c8"},
        {getColoredPiece(Queen, Black), "d8"},
        {getColoredPiece(King, Black), "e8"},
        {getColoredPiece(Bishop, Black), "f8"},
        {getColoredPiece(Knight, Black), "g8"},
        {getColoredPiece(Rook, Black), "h8"},
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

TEST(FenParsing, DontCrash) {
    // Call the parsing function on some arbitrary FEN strings found online
    std::vector<std::string> fenStrings{
        "8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - - 1 49",
        "5r1k/6pp/4Qpb1/p7/8/6PP/P4PK1/3q4 b - - 4 37",
        "8/8/2P5/4B3/1Q6/4K3/6P1/3k4 w - - 5 67",
        "r2q1rk1/pp2ppbp/2p2np1/6B1/3PP1b1/Q1P2N2/P4PPP/3RKB1R b K - 0 13"
    };

    for (const auto& fenString : fenStrings) {
        (void)GameState::fromFen(fenString);
    }
}

TEST(FenParsing, CastlingRights) {
    std::string noCastlingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1";
    std::string blackCastlingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w kq - 0 1";
    std::string whiteCastlingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1";
    std::string kingCastlingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Kk - 0 1";
    std::string queenCastlingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Qq - 0 1";
    std::string blackQueenCastlingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w q - 0 1";

    GameState noCastling = GameState::fromFen(noCastlingFen);
    ASSERT_FALSE(noCastling.canCastleKingSide(White));
    ASSERT_FALSE(noCastling.canCastleQueenSide(White));
    ASSERT_FALSE(noCastling.canCastleKingSide(Black));
    ASSERT_FALSE(noCastling.canCastleQueenSide(Black));

    GameState blackCastling = GameState::fromFen(blackCastlingFen);
    ASSERT_FALSE(blackCastling.canCastleKingSide(White));
    ASSERT_FALSE(blackCastling.canCastleQueenSide(White));
    ASSERT_TRUE(blackCastling.canCastleKingSide(Black));
    ASSERT_TRUE(blackCastling.canCastleQueenSide(Black));

    GameState whiteCastling = GameState::fromFen(whiteCastlingFen);
    ASSERT_TRUE(whiteCastling.canCastleKingSide(White));
    ASSERT_TRUE(whiteCastling.canCastleQueenSide(White));
    ASSERT_FALSE(whiteCastling.canCastleKingSide(Black));
    ASSERT_FALSE(whiteCastling.canCastleQueenSide(Black));

    GameState kingCastling = GameState::fromFen(kingCastlingFen);
    ASSERT_TRUE(kingCastling.canCastleKingSide(White));
    ASSERT_FALSE(kingCastling.canCastleQueenSide(White));
    ASSERT_TRUE(kingCastling.canCastleKingSide(Black));
    ASSERT_FALSE(kingCastling.canCastleQueenSide(Black));

    GameState queenCastling = GameState::fromFen(queenCastlingFen);
    ASSERT_FALSE(queenCastling.canCastleKingSide(White));
    ASSERT_TRUE(queenCastling.canCastleQueenSide(White));
    ASSERT_FALSE(queenCastling.canCastleKingSide(Black));
    ASSERT_TRUE(queenCastling.canCastleQueenSide(Black));

    GameState blackQueenCastling = GameState::fromFen(blackQueenCastlingFen);
    ASSERT_FALSE(blackQueenCastling.canCastleKingSide(White));
    ASSERT_FALSE(blackQueenCastling.canCastleQueenSide(White));
    ASSERT_FALSE(blackQueenCastling.canCastleKingSide(Black));
    ASSERT_TRUE(blackQueenCastling.canCastleQueenSide(Black));
}

TEST(FenParsing, EnPassantTarget) {
    std::string enPassantFen = "rnbqkbnr/1ppppppp/8/p7/8/8/PPPPPPPP/RNBQKBNR w KQkq a3 0 1";
    GameState gameState = GameState::fromFen(enPassantFen);
    ASSERT_EQ(gameState.getEnPassantTarget(), positionFromAlgebraic("a3"));
}

TEST(FenParsing, HalfMoveClock) {
    std::string fen9 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 9 1";
    std::string fen42 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 42 1";
    std::string fen314 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 314 1";

    ASSERT_EQ(GameState::fromFen(fen9).getPlySinceCaptureOrPawn(), 9);
    ASSERT_EQ(GameState::fromFen(fen42).getPlySinceCaptureOrPawn(), 42);
    ASSERT_EQ(GameState::fromFen(fen314).getPlySinceCaptureOrPawn(), 314);
}
