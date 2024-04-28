#include "chess-engine-lib/GameState.h"

#include "gtest/gtest.h"

#include <set>
#include <string>
#include <vector>

TEST(FenParsing, TestStartingPosition) {
    GameState startingPosition = GameState::startingPosition();

    std::set<std::pair<ColoredPiece, std::string>> expectedPiecesAlgebraic {
        {getColoredPiece(Piece::Rook, Side::White), "a1"},
        {getColoredPiece(Piece::Knight, Side::White), "b1"},
        {getColoredPiece(Piece::Bishop, Side::White), "c1"},
        {getColoredPiece(Piece::Queen, Side::White), "d1"},
        {getColoredPiece(Piece::King, Side::White), "e1"},
        {getColoredPiece(Piece::Bishop, Side::White), "f1"},
        {getColoredPiece(Piece::Knight, Side::White), "g1"},
        {getColoredPiece(Piece::Rook, Side::White), "h1"},

        {getColoredPiece(Piece::Pawn, Side::White), "a2"},
        {getColoredPiece(Piece::Pawn, Side::White), "b2"},
        {getColoredPiece(Piece::Pawn, Side::White), "c2"},
        {getColoredPiece(Piece::Pawn, Side::White), "d2"},
        {getColoredPiece(Piece::Pawn, Side::White), "e2"},
        {getColoredPiece(Piece::Pawn, Side::White), "f2"},
        {getColoredPiece(Piece::Pawn, Side::White), "g2"},
        {getColoredPiece(Piece::Pawn, Side::White), "h2"},

        {getColoredPiece(Piece::Pawn, Side::Black), "a7"},
        {getColoredPiece(Piece::Pawn, Side::Black), "b7"},
        {getColoredPiece(Piece::Pawn, Side::Black), "c7"},
        {getColoredPiece(Piece::Pawn, Side::Black), "d7"},
        {getColoredPiece(Piece::Pawn, Side::Black), "e7"},
        {getColoredPiece(Piece::Pawn, Side::Black), "f7"},
        {getColoredPiece(Piece::Pawn, Side::Black), "g7"},
        {getColoredPiece(Piece::Pawn, Side::Black), "h7"},

        {getColoredPiece(Piece::Rook, Side::Black), "a8"},
        {getColoredPiece(Piece::Knight, Side::Black), "b8"},
        {getColoredPiece(Piece::Bishop, Side::Black), "c8"},
        {getColoredPiece(Piece::Queen, Side::Black), "d8"},
        {getColoredPiece(Piece::King, Side::Black), "e8"},
        {getColoredPiece(Piece::Bishop, Side::Black), "f8"},
        {getColoredPiece(Piece::Knight, Side::Black), "g8"},
        {getColoredPiece(Piece::Rook, Side::Black), "h8"},
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

    ASSERT_EQ(startingPosition.getSideToMove(), Side::White);

    ASSERT_EQ(startingPosition.canCastleKingSide(Side::White), true);
    ASSERT_EQ(startingPosition.canCastleQueenSide(Side::White), true);
    ASSERT_EQ(startingPosition.canCastleKingSide(Side::Black), true);
    ASSERT_EQ(startingPosition.canCastleQueenSide(Side::Black), true);

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
    ASSERT_FALSE(noCastling.canCastleKingSide(Side::White));
    ASSERT_FALSE(noCastling.canCastleQueenSide(Side::White));
    ASSERT_FALSE(noCastling.canCastleKingSide(Side::Black));
    ASSERT_FALSE(noCastling.canCastleQueenSide(Side::Black));

    GameState blackCastling = GameState::fromFen(blackCastlingFen);
    ASSERT_FALSE(blackCastling.canCastleKingSide(Side::White));
    ASSERT_FALSE(blackCastling.canCastleQueenSide(Side::White));
    ASSERT_TRUE(blackCastling.canCastleKingSide(Side::Black));
    ASSERT_TRUE(blackCastling.canCastleQueenSide(Side::Black));

    GameState whiteCastling = GameState::fromFen(whiteCastlingFen);
    ASSERT_TRUE(whiteCastling.canCastleKingSide(Side::White));
    ASSERT_TRUE(whiteCastling.canCastleQueenSide(Side::White));
    ASSERT_FALSE(whiteCastling.canCastleKingSide(Side::Black));
    ASSERT_FALSE(whiteCastling.canCastleQueenSide(Side::Black));

    GameState kingCastling = GameState::fromFen(kingCastlingFen);
    ASSERT_TRUE(kingCastling.canCastleKingSide(Side::White));
    ASSERT_FALSE(kingCastling.canCastleQueenSide(Side::White));
    ASSERT_TRUE(kingCastling.canCastleKingSide(Side::Black));
    ASSERT_FALSE(kingCastling.canCastleQueenSide(Side::Black));

    GameState queenCastling = GameState::fromFen(queenCastlingFen);
    ASSERT_FALSE(queenCastling.canCastleKingSide(Side::White));
    ASSERT_TRUE(queenCastling.canCastleQueenSide(Side::White));
    ASSERT_FALSE(queenCastling.canCastleKingSide(Side::Black));
    ASSERT_TRUE(queenCastling.canCastleQueenSide(Side::Black));

    GameState blackQueenCastling = GameState::fromFen(blackQueenCastlingFen);
    ASSERT_FALSE(blackQueenCastling.canCastleKingSide(Side::White));
    ASSERT_FALSE(blackQueenCastling.canCastleQueenSide(Side::White));
    ASSERT_FALSE(blackQueenCastling.canCastleKingSide(Side::Black));
    ASSERT_TRUE(blackQueenCastling.canCastleQueenSide(Side::Black));
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
