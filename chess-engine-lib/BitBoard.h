#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BoardPosition.h"

#include <bit>
#include <string>

#include <cstdint>

enum class BitBoard : std::uint64_t {
    Empty = 0,
    Full  = ~0ULL,
};

[[nodiscard]] std::string bitBoardToVisualString(BitBoard bitboard);

[[nodiscard]] constexpr bool isSet(BitBoard bitboard, BoardPosition position) {
    return (std::uint64_t)bitboard & (1ULL << (int)position);
}

constexpr void set(BitBoard& bitboard, BoardPosition position) {
    bitboard = (BitBoard)((std::uint64_t)bitboard | 1ULL << (int)position);
}

constexpr void clear(BitBoard& bitboard, BoardPosition position) {
    bitboard = (BitBoard)((std::uint64_t)bitboard & ~(1ULL << (int)position));
}

template <typename... BitBoardTs>
[[nodiscard]] constexpr BitBoard any(BitBoardTs... bitboards) {
    static_assert((std::is_same_v<BitBoardTs, BitBoard> && ...));
    return (BitBoard)((std::uint64_t)bitboards | ...);
}

template <typename... BitBoardTs>
[[nodiscard]] constexpr BitBoard intersection(BitBoardTs... bitboards) {
    static_assert((std::is_same_v<BitBoardTs, BitBoard> && ...));
    return (BitBoard)((std::uint64_t)bitboards & ...);
}

[[nodiscard]] constexpr BitBoard subtract(BitBoard lhs, BitBoard rhs) {
    return (BitBoard)((std::uint64_t)lhs & ~(std::uint64_t)rhs);
}

[[nodiscard]] constexpr BoardPosition getFirstSetPosition(BitBoard bitBoard) {
    return (BoardPosition)std::countr_zero((std::uint64_t)bitBoard);
}

[[nodiscard]] constexpr BoardPosition popFirstSetPosition(BitBoard& bitBoard) {
    const BoardPosition position = getFirstSetPosition(bitBoard);
    bitBoard = (BitBoard)((std::uint64_t)bitBoard & ((std::uint64_t)bitBoard - 1ull));
    return position;
}
