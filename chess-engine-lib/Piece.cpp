#include "Piece.h"

#include "MyAssert.h"

namespace {

constexpr char kLowerCaseBit = 1 << 5;

}  // namespace

Piece pieceFromFenChar(char c) {
    char upperCase = c & ~kLowerCaseBit;
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
    }
    UNREACHABLE;
}

ColoredPiece coloredPieceFromFenChar(char c) {
    return getColoredPiece(pieceFromFenChar(c), sideFromFenChar(c));
}

char toFenChar(Piece piece) {
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

char toLowerCaseFenChar(Piece piece) {
    return toFenChar(piece) | kLowerCaseBit;
}

char toFenChar(ColoredPiece coloredPiece) {
    char c = toFenChar(getPiece(coloredPiece));
    if (getSide(coloredPiece) == Side::Black) {
        c |= kLowerCaseBit;
    }
    return c;
}
