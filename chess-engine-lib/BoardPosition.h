#include "ClangDiagnosticIgnore.h"

#pragma once

#include <string>

#include <cstdint>

enum class BoardPosition : std::uint8_t { Invalid = 1 << 6 };

[[nodiscard]] constexpr BoardPosition positionFromFileRank(int file, int rank) {
    return static_cast<BoardPosition>(rank * 8 + file);
}

[[nodiscard]] constexpr int fileFromPosition(BoardPosition position) {
    return (int)position % 8;
}

[[nodiscard]] constexpr int rankFromPosition(BoardPosition position) {
    return (int)position / 8;
}

[[nodiscard]] constexpr std::pair<int, int> fileRankFromPosition(BoardPosition position) {
    return {fileFromPosition(position), rankFromPosition(position)};
}

[[nodiscard]] constexpr BoardPosition positionFromAlgebraic(std::string_view algebraic) {
    const int file = algebraic[0] - 'a';
    const int rank = algebraic[1] - '1';
    return positionFromFileRank(file, rank);
}

[[nodiscard]] constexpr std::string algebraicFromPosition(BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);
    return {(char)('a' + file), (char)('1' + rank)};
}
