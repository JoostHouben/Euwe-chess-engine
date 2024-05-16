#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BoardPosition.h"
#include "Piece.h"

#include <cstdint>

using HashT = std::uint64_t;

void updateHashForPiecePosition(Side side, Piece piece, BoardPosition position, HashT& hash);
void updateHashForPiecePosition(ColoredPiece coloredPiece, BoardPosition position, HashT& hash);

void updateHashForSideToMove(HashT& hash);

void updateHashForEnPassantFile(int enPassantFile, HashT& hash);

void updateHashForCastlingRights(Side side, bool isQueenSide, HashT& hash);

inline void updateHashForKingSideCastlingRights(const Side side, HashT& hash) {
    updateHashForCastlingRights(side, false, hash);
}

inline void updateHashForQueenSideCastlingRights(const Side side, HashT& hash) {
    updateHashForCastlingRights(side, true, hash);
}
