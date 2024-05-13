#include "ClangDiagnosticIgnore.h"

#pragma once

#include "MyAssert.h"
#include "Side.h"

#include <array>
#include <string>

#include <cstdint>

enum class Piece : std::uint8_t { Pawn, Knight, Bishop, Rook, Queen, King, Invalid = 7 };

inline constexpr std::array kPromotionPieces = {
        Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen};

[[nodiscard]] constexpr std::string pieceToString(Piece piece) {
    switch (piece) {
        case Piece::Pawn:
            return "P";
        case Piece::Knight:
            return "N";
        case Piece::Bishop:
            return "B";
        case Piece::Rook:
            return "R";
        case Piece::Queen:
            return "Q";
        case Piece::King:
            return "K";
        case Piece::Invalid:
            return "!";
    }
    UNREACHABLE;
}

[[nodiscard]] constexpr bool isSlidingPiece(Piece piece) {
    return piece == Piece::Bishop || piece == Piece::Rook || piece == Piece::Queen;
}

[[nodiscard]] constexpr bool isPinningPiece(Piece piece) {
    return piece == Piece::Bishop || piece == Piece::Rook || piece == Piece::Queen;
}

[[nodiscard]] Piece pieceFromFenChar(char c);
[[nodiscard]] char toFenChar(Piece piece);
[[nodiscard]] char toLowerCaseFenChar(Piece piece);

enum class ColoredPiece : std::uint8_t {
    WhitePawn   = (std::uint8_t)Piece::Pawn,
    WhiteKnight = (std::uint8_t)Piece::Knight,
    WhiteBishop = (std::uint8_t)Piece::Bishop,
    WhiteRook   = (std::uint8_t)Piece::Rook,
    WhiteQueen  = (std::uint8_t)Piece::Queen,
    WhiteKing   = (std::uint8_t)Piece::King,

    BlackPawn   = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Pawn,
    BlackKnight = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Knight,
    BlackBishop = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Bishop,
    BlackRook   = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Rook,
    BlackQueen  = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Queen,
    BlackKing   = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::King,

    Invalid = (std::uint8_t)Piece::Invalid,
};

[[nodiscard]] constexpr ColoredPiece getColoredPiece(Piece piece, Side side) {
    return static_cast<ColoredPiece>(((std::uint8_t)side << 3) | (std::uint8_t)piece);
}

[[nodiscard]] constexpr Piece getPiece(ColoredPiece coloredPiece) {
    constexpr std::uint8_t kPieceMask = 7;
    return static_cast<Piece>((std::uint8_t)coloredPiece & kPieceMask);
}

[[nodiscard]] constexpr Side getSide(ColoredPiece coloredPiece) {
    return static_cast<Side>((std::uint8_t)coloredPiece >> 3);
}

[[nodiscard]] ColoredPiece coloredPieceFromFenChar(char c);
[[nodiscard]] char toFenChar(ColoredPiece coloredPiece);
