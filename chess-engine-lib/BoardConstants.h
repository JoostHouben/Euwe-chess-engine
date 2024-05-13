#include "ClangDiagnosticIgnore.h"

#pragma once

constexpr int kNumSides = 2;

constexpr int kRanks   = 8;
constexpr int kFiles   = 8;
constexpr int kSquares = kRanks * kFiles;

constexpr int kNumPawns         = 8;
constexpr int kNumNonPawns      = 8;
constexpr int kNumPiecesPerSide = kNumPawns + kNumNonPawns;
constexpr int kNumTotalPieces   = kNumPiecesPerSide * kNumSides;

constexpr int kNumPieceTypes = 6;
