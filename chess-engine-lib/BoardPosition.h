#pragma once

#include "Macros.h"

#include <stdexcept>
#include <string>

#include <cstdint>

// clang-format off
enum class BoardPosition : std::uint8_t {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
    Invalid };
// clang-format on

[[nodiscard]] FORCE_INLINE constexpr BoardPosition positionFromFileRank(int file, int rank) {
    return static_cast<BoardPosition>(rank * 8 + file);
}

[[nodiscard]] FORCE_INLINE constexpr int fileFromPosition(BoardPosition position) {
    return (int)position % 8;
}

[[nodiscard]] FORCE_INLINE constexpr int rankFromPosition(BoardPosition position) {
    return (int)position / 8;
}

[[nodiscard]] FORCE_INLINE constexpr std::pair<int, int> fileRankFromPosition(
        BoardPosition position) {
    return {fileFromPosition(position), rankFromPosition(position)};
}

[[nodiscard]] constexpr BoardPosition positionFromAlgebraic(std::string_view algebraic) {
    const int file = algebraic[0] - 'a';
    const int rank = algebraic[1] - '1';

    if (file < 0 || file > 7 || rank < 0 || rank > 7) [[unlikely]] {
        throw std::invalid_argument("Invalid algebraic position: " + std::string(algebraic));
    }

    return positionFromFileRank(file, rank);
}

[[nodiscard]] constexpr std::string algebraicFromPosition(BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);
    return {(char)('a' + file), (char)('1' + rank)};
}

[[nodiscard]] FORCE_INLINE constexpr BoardPosition getVerticalReflection(BoardPosition position) {
    return (BoardPosition)((int)position ^ 56);
}

[[nodiscard]] FORCE_INLINE constexpr int getSquareColor(BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);
    return (file + rank) & 1;
}
