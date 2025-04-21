#pragma once

#include <array>

inline constexpr int kNumSides = 2;

inline constexpr int kRanks   = 8;
inline constexpr int kFiles   = 8;
inline constexpr int kSquares = kRanks * kFiles;

inline constexpr int kNumPawns         = 8;
inline constexpr int kNumNonPawns      = 8;
inline constexpr int kNumPiecesPerSide = kNumPawns + kNumNonPawns;
inline constexpr int kNumTotalPieces   = kNumPiecesPerSide * kNumSides;

inline constexpr int kNumPieceTypes = 6;

inline constexpr std::array kSigns = {+1, -1};
