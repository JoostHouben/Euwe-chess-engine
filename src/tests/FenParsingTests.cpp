#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace FenParsingTests {

TEST(FenParsing, TestStartingPosition) {
    const GameState startingPosition = GameState::startingPosition();

    std::set<std::pair<ColoredPiece, std::string>> expectedPiecesAlgebraic{
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

    using PiecePosition = std::pair<ColoredPiece, BoardPosition>;

    std::set<PiecePosition> expectedPieces;
    for (const auto& [piece, algebraic] : expectedPiecesAlgebraic) {
        expectedPieces.insert({piece, positionFromAlgebraic(algebraic).value()});
    }

    std::set<PiecePosition> actualPieces;
    for (int pieceIdx = (int)Piece::Pawn; pieceIdx <= (int)Piece::King; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;
        for (int sideIdx = (int)Side::White; sideIdx <= (int)Side::Black; ++sideIdx) {
            BitBoard pieceBitBoard = startingPosition.getPieceBitBoard((Side)sideIdx, piece);

            while (pieceBitBoard != BitBoard::Empty) {
                const BoardPosition position = getFirstSetPosition(pieceBitBoard);
                pieceBitBoard &= ~position;
                actualPieces.insert({getColoredPiece(piece, (Side)sideIdx), position});
            }
        }
    }

    EXPECT_EQ(expectedPieces, actualPieces);

    EXPECT_EQ(startingPosition.getSideToMove(), Side::White);

    EXPECT_EQ(startingPosition.canCastleKingSide(Side::White), true);
    EXPECT_EQ(startingPosition.canCastleQueenSide(Side::White), true);
    EXPECT_EQ(startingPosition.canCastleKingSide(Side::Black), true);
    EXPECT_EQ(startingPosition.canCastleQueenSide(Side::Black), true);

    EXPECT_EQ(startingPosition.getEnPassantTarget(), BoardPosition::Invalid);

    EXPECT_EQ(startingPosition.getPlySinceCaptureOrPawn(), 0);

    std::string startingPositionFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    EXPECT_EQ(startingPosition.toFen(), startingPositionFen);
}

TEST(FenParsing, RoundTrip) {
    std::vector<std::string> fenStrings{
            "8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - - 1 49",
            "5r1k/6pp/4Qpb1/p7/8/6PP/P4PK1/3q4 b - - 4 37",
            "8/8/2P5/4B3/1Q6/4K3/6P1/3k4 w - - 5 67",
            "r2q1rk1/pp2ppbp/2p2np1/6B1/3PP1b1/Q1P2N2/P4PPP/3RKB1R b K - 0 13"};

    for (const auto& fenString : fenStrings) {
        auto res = GameState::fromFen(fenString);
        ASSERT_TRUE(res.has_value());
        GameState gameState      = res.value();
        std::string newFenString = gameState.toFen();
        EXPECT_EQ(newFenString, fenString);
    }
}

TEST(FenParsing, CastlingRights) {
    std::string noCastlingFen         = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1";
    std::string blackCastlingFen      = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w kq - 0 1";
    std::string whiteCastlingFen      = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1";
    std::string kingCastlingFen       = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Kk - 0 1";
    std::string queenCastlingFen      = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Qq - 0 1";
    std::string blackQueenCastlingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w q - 0 1";

    {
        auto res = GameState::fromFen(noCastlingFen);
        ASSERT_TRUE(res.has_value());
        GameState noCastling = res.value();
        EXPECT_FALSE(noCastling.canCastleKingSide(Side::White));
        EXPECT_FALSE(noCastling.canCastleQueenSide(Side::White));
        EXPECT_FALSE(noCastling.canCastleKingSide(Side::Black));
        EXPECT_FALSE(noCastling.canCastleQueenSide(Side::Black));
    }

    {
        auto res = GameState::fromFen(blackCastlingFen);
        ASSERT_TRUE(res.has_value());
        GameState blackCastling = res.value();
        EXPECT_FALSE(blackCastling.canCastleKingSide(Side::White));
        EXPECT_FALSE(blackCastling.canCastleQueenSide(Side::White));
        EXPECT_TRUE(blackCastling.canCastleKingSide(Side::Black));
        EXPECT_TRUE(blackCastling.canCastleQueenSide(Side::Black));
    }

    {
        auto res = GameState::fromFen(whiteCastlingFen);
        ASSERT_TRUE(res.has_value());
        GameState whiteCastling = res.value();
        EXPECT_TRUE(whiteCastling.canCastleKingSide(Side::White));
        EXPECT_TRUE(whiteCastling.canCastleQueenSide(Side::White));
        EXPECT_FALSE(whiteCastling.canCastleKingSide(Side::Black));
        EXPECT_FALSE(whiteCastling.canCastleQueenSide(Side::Black));
    }

    {
        auto res = GameState::fromFen(kingCastlingFen);
        ASSERT_TRUE(res.has_value());
        GameState kingCastling = res.value();
        EXPECT_TRUE(kingCastling.canCastleKingSide(Side::White));
        EXPECT_FALSE(kingCastling.canCastleQueenSide(Side::White));
        EXPECT_TRUE(kingCastling.canCastleKingSide(Side::Black));
        EXPECT_FALSE(kingCastling.canCastleQueenSide(Side::Black));
    }

    {
        auto res = GameState::fromFen(queenCastlingFen);
        ASSERT_TRUE(res.has_value());
        GameState queenCastling = res.value();
        EXPECT_FALSE(queenCastling.canCastleKingSide(Side::White));
        EXPECT_TRUE(queenCastling.canCastleQueenSide(Side::White));
        EXPECT_FALSE(queenCastling.canCastleKingSide(Side::Black));
        EXPECT_TRUE(queenCastling.canCastleQueenSide(Side::Black));
    }

    {
        auto res = GameState::fromFen(blackQueenCastlingFen);
        ASSERT_TRUE(res.has_value());
        GameState blackQueenCastling = res.value();
        EXPECT_FALSE(blackQueenCastling.canCastleKingSide(Side::White));
        EXPECT_FALSE(blackQueenCastling.canCastleQueenSide(Side::White));
        EXPECT_FALSE(blackQueenCastling.canCastleKingSide(Side::Black));
        EXPECT_TRUE(blackQueenCastling.canCastleQueenSide(Side::Black));
    }
}

TEST(FenParsing, EnPassantTarget) {
    std::string enPassantFen = "rnbqkbnr/1ppppppp/8/p7/8/8/PPPPPPPP/RNBQKBNR w KQkq a3 0 1";
    auto res                 = GameState::fromFen(enPassantFen);
    ASSERT_TRUE(res.has_value());
    GameState gameState = res.value();
    EXPECT_EQ(gameState.getEnPassantTarget(), BoardPosition::A3);
}

TEST(FenParsing, HalfMoveClock) {
    std::string fen9   = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 9 1";
    std::string fen42  = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 42 1";
    std::string fen314 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 75 1";

    {
        auto res = GameState::fromFen(fen9);
        ASSERT_TRUE(res.has_value());
        EXPECT_EQ(res->getPlySinceCaptureOrPawn(), 9);
    }
    {
        auto res = GameState::fromFen(fen42);
        ASSERT_TRUE(res.has_value());
        EXPECT_EQ(res->getPlySinceCaptureOrPawn(), 42);
    }
    {
        auto res = GameState::fromFen(fen314);
        ASSERT_TRUE(res.has_value());
        EXPECT_EQ(res->getPlySinceCaptureOrPawn(), 75);
    }
}

TEST(FenParsing, ImplicitMoveClocks) {
    const std::string withMoveClocks             = "8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - - 0 1";
    const std::string withoutMoveClocks          = "8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - -";
    const std::string withoutMoveClocksWithSpace = "8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - - ";
    const std::string onlyCaptureClock           = "8/4npk1/5p1p/1Q5P/1p4P1/4r3/7q/3K1R2 b - - 0";

    {
        auto res = GameState::fromFen(withoutMoveClocks);
        ASSERT_TRUE(res.has_value());
        EXPECT_EQ(res->toFen(), withMoveClocks);
    }
    {
        auto res = GameState::fromFen(withoutMoveClocksWithSpace);
        ASSERT_TRUE(res.has_value());
        EXPECT_EQ(res->toFen(), withMoveClocks);
    }
    {
        auto res = GameState::fromFen(onlyCaptureClock);
        ASSERT_TRUE(res.has_value());
        EXPECT_EQ(res->toFen(), withMoveClocks);
    }
}

TEST(FenParsing, ErrorHandling) {
    // string too short
    EXPECT_FALSE(GameState::fromFen("").has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr").has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP").has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP ").has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR ").has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w").has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w ").has_value());
    EXPECT_FALSE(
            GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq").has_value());
    EXPECT_FALSE(
            GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq ").has_value());

    // string has extra stuff
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 blah")
                         .has_value());

    // invalid board configuration
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/ppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                         .has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/ppppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                         .has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/xppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                         .has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/1/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                         .has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/9/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                         .has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/8 w KQkq - 0 1")
                         .has_value());

    // invalid side to move
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1")
                         .has_value());

    // invalid castling rights
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KBkq - 0 1")
                         .has_value());

    // invalid en passant target
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq x9 0 1")
                         .has_value());

    // invalid move clocks
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - x 1")
                         .has_value());
    EXPECT_FALSE(GameState::fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 x")
                         .has_value());
}

}  // namespace FenParsingTests
