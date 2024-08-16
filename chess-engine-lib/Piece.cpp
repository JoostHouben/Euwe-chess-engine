#include "Piece.h"

#include "MyAssert.h"

#include <format>
#include <stdexcept>

namespace {

constexpr char kLowerCaseBit = 1 << 5;

}  // namespace

Piece pieceFromFenChar(const char c) {
    const char upperCase = c & ~kLowerCaseBit;
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
            throw std::invalid_argument(std::format("Invalid FEN piece character: {}", c));
    }
}

ColoredPiece coloredPieceFromFenChar(const char c) {
    return getColoredPiece(pieceFromFenChar(c), sideFromFenChar(c));
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
    return toFenChar(piece) | kLowerCaseBit;
}

char toFenChar(const ColoredPiece coloredPiece) {
    char c = toFenChar(getPiece(coloredPiece));
    if (getSide(coloredPiece) == Side::Black) {
        c |= kLowerCaseBit;
    }
    return c;
}
