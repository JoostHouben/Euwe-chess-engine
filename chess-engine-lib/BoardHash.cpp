#include "BoardHash.h"

#include "BoardConstants.h"

#include <array>
#include <random>

namespace {

std::mt19937_64 gRandomGenerator;

using PiecePositionHashesT =
        std::array<std::array<std::array<HashT, kNumPieceTypes>, kSquares>, kNumSides>;

PiecePositionHashesT getPiecePositionHashes() {
    PiecePositionHashesT piecePositionhashes{};

    for (int side = 0; side < kNumSides; ++side) {
        for (int square = 0; square < kSquares; ++square) {
            for (int piece = 0; piece < kNumPieceTypes; ++piece) {
                piecePositionhashes[side][square][piece] = gRandomGenerator();
            }
        }
    }

    return piecePositionhashes;
}

PiecePositionHashesT gPiecePositionHashes = getPiecePositionHashes();

HashT gSideToMoveHash = gRandomGenerator();

using EnPassantHashesT = std::array<HashT, kFiles>;

EnPassantHashesT getEnPassantHashes() {
    EnPassantHashesT enPassantHashes{};

    for (int file = 0; file < kFiles; ++file) {
        enPassantHashes[file] = gRandomGenerator();
    }

    return enPassantHashes;
}

EnPassantHashesT gEnPassantHashes = getEnPassantHashes();

using CastlingRightsHashesT = std::array<std::array<HashT, 2>, kNumSides>;

CastlingRightsHashesT getCastlingRightsHashes() {
    CastlingRightsHashesT castlingRightsHashes{};

    for (int side = 0; side < kNumSides; ++side) {
        castlingRightsHashes[side][0] = gRandomGenerator();
        castlingRightsHashes[side][1] = gRandomGenerator();
    }

    return castlingRightsHashes;
}

CastlingRightsHashesT gCastlingRightsHashes = getCastlingRightsHashes();

}  // namespace

void updateHashForPiecePosition(
        const Side side, const Piece piece, const BoardPosition position, HashT& hash) {
    hash ^= gPiecePositionHashes[(int)side][(int)position][(int)piece];
}

void updateHashForPiecePosition(
        const ColoredPiece coloredPiece, const BoardPosition position, HashT& hash) {
    updateHashForPiecePosition(getSide(coloredPiece), getPiece(coloredPiece), position, hash);
}

void updateHashForSideToMove(HashT& hash) {
    hash ^= gSideToMoveHash;
}

void updateHashForEnPassantFile(const int enPassantFile, HashT& hash) {
    hash ^= gEnPassantHashes[enPassantFile];
}

void updateHashForCastlingRights(const Side side, const bool isQueenSide, HashT& hash) {
    hash ^= gCastlingRightsHashes[(int)side][(int)isQueenSide];
}
