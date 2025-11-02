#include "Piece.h"

#include "MyAssert.h"

#include <format>

namespace {

constexpr char kLowerCaseBit = 1 << 5;

}  // namespace

std::expected<Piece, std::string> pieceFromFenChar(const char c) {
    const char upperCase = (char)(c & ~kLowerCaseBit);
    switch (upperCase) {
        case 'P':
            return Piece::Pawn;
        case 'N':
            return Piece::Knight;
        case 'B':
            return Piece::Bishop;
        case 'R':
            return Piece::Rook;
        case 'Q':
            return Piece::Queen;
        case 'K':
            return Piece::King;
        default:
            return std::unexpected(std::format("Invalid FEN piece character: {}", c));
    }
}

std::expected<ColoredPiece, std::string> coloredPieceFromFenChar(const char c) {
    auto pieceRes = pieceFromFenChar(c);
    if (!pieceRes) {
        return std::unexpected(pieceRes.error());
    }
    return getColoredPiece(*pieceRes, sideFromFenChar(c));
}

char toFenChar(const Piece piece) {
    switch (piece) {
        case Piece::Pawn:
            return 'P';
        case Piece::Knight:
            return 'N';
        case Piece::Bishop:
            return 'B';
        case Piece::Rook:
            return 'R';
        case Piece::Queen:
            return 'Q';
        case Piece::King:
            return 'K';
        default:
            UNREACHABLE;
    }
}

char toLowerCaseFenChar(const Piece piece) {
    return (char)(toFenChar(piece) | kLowerCaseBit);
}

char toFenChar(const ColoredPiece coloredPiece) {
    char c = toFenChar(getPiece(coloredPiece));
    if (getSide(coloredPiece) == Side::Black) {
        c |= kLowerCaseBit;
    }
    return c;
}
