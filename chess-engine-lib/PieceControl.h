#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BitBoard.h"
#include "BoardPosition.h"
#include "Piece.h"
#include "Side.h"

BitBoard getPawnControlledSquares(BitBoard pawnBitBoard, Side side);

BitBoard getKingControlledSquares(BoardPosition position);

BitBoard getRookAttack(BoardPosition position, BitBoard occupancy);
BitBoard getRookXRay(BoardPosition position, BitBoard occupancy);
BitBoard getBishopAttack(BoardPosition position, BitBoard occupancy);
BitBoard getBishopXRay(BoardPosition position, BitBoard occupancy);

std::uint64_t getFullRay(BoardPosition position, int fileIncrement, int rankIncrement);

BitBoard getPieceControlledSquares(Piece piece, BoardPosition position, BitBoard anyPiece);
BitBoard getPieceXRays(Piece piece, BoardPosition position, BitBoard anyPiece);
