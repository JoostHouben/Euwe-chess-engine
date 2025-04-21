#pragma once

#include "BoardPosition.h"

#include <array>
#include <bit>
#include <string>

#include <cstdint>

enum class BitBoard : std::uint64_t {
    Empty = 0,
    Full  = ~0ULL,
};

[[nodiscard]] std::string bitBoardToVisualString(BitBoard bitboard);

// Operators between bitboards

[[nodiscard]] constexpr BitBoard operator|(const BitBoard lhs, const BitBoard rhs) {
    return (BitBoard)((std::uint64_t)lhs | (std::uint64_t)rhs);
}

constexpr BitBoard& operator|=(BitBoard& lhs, const BitBoard rhs) {
    lhs = lhs | rhs;
    return lhs;
}

[[nodiscard]] constexpr BitBoard operator&(const BitBoard lhs, const BitBoard rhs) {
    return (BitBoard)((std::uint64_t)lhs & (std::uint64_t)rhs);
}

constexpr BitBoard& operator&=(BitBoard& lhs, const BitBoard rhs) {
    lhs = lhs & rhs;
    return lhs;
}

[[nodiscard]] constexpr BitBoard operator~(const BitBoard bitboard) {
    return (BitBoard) ~(std::uint64_t)bitboard;
}

// Operators between bitboards and positions

[[nodiscard]] constexpr BitBoard operator|(const BitBoard bitboard, const BoardPosition position) {
    return (BitBoard)((std::uint64_t)bitboard | (1ULL << (int)position));
}

constexpr BitBoard& operator|=(BitBoard& bitboard, const BoardPosition position) {
    bitboard = bitboard | position;
    return bitboard;
}

[[nodiscard]] constexpr bool operator&(const BitBoard bitboard, const BoardPosition position) {
    return (std::uint64_t)bitboard & (1ULL << (int)position);
}

enum class InverseBoardPosition : std::uint8_t {};

[[nodiscard]] constexpr InverseBoardPosition operator~(const BoardPosition position) {
    return static_cast<InverseBoardPosition>(position);
}

[[nodiscard]] constexpr BitBoard operator&(
        const BitBoard bitboard, const InverseBoardPosition position) {
    return (BitBoard)((std::uint64_t)bitboard & ~(1ULL << (int)position));
}

constexpr BitBoard& operator&=(BitBoard& bitboard, const InverseBoardPosition position) {
    bitboard = bitboard & position;
    return bitboard;
}

[[nodiscard]] constexpr BoardPosition getFirstSetPosition(const BitBoard bitBoard) {
    return (BoardPosition)std::countr_zero((std::uint64_t)bitBoard);
}

constexpr BoardPosition popFirstSetPosition(BitBoard& bitBoard) {
    const BoardPosition position = getFirstSetPosition(bitBoard);
    bitBoard = (BitBoard)((std::uint64_t)bitBoard & ((std::uint64_t)bitBoard - 1ull));
    return position;
}

[[nodiscard]] constexpr BitBoard getFirstSetBitBoard(const BitBoard bitBoard) {
    return (BitBoard)std::bit_floor((std::uint64_t)bitBoard);
}

[[nodiscard]] constexpr int popCount(const BitBoard bitBoard) {
    return std::popcount((std::uint64_t)bitBoard);
}

inline constexpr std::uint64_t kNorthRankMask = 0xffULL << (7 * 8);
inline constexpr std::uint64_t kWestFileMask  = 0x0101010101010101ULL;
inline constexpr std::uint64_t kSouthRankMask = 0xffULL;
inline constexpr std::uint64_t kEastFileMask  = 0x8080808080808080ULL;

inline constexpr std::uint64_t kNotNorthRankMask = ~kNorthRankMask;
inline constexpr std::uint64_t kNotWestFileMask  = ~kWestFileMask;
inline constexpr std::uint64_t kNotSouthRankMask = ~kSouthRankMask;
inline constexpr std::uint64_t kNotEastFileMask  = ~kEastFileMask;

inline constexpr std::uint64_t kAllMask = ~0ULL;

inline constexpr BitBoard kNotNorthRankBb = (BitBoard)kNotNorthRankMask;
inline constexpr BitBoard kNotWestFileBb  = (BitBoard)kNotWestFileMask;
inline constexpr BitBoard kNotSouthRankBb = (BitBoard)kNotSouthRankMask;
inline constexpr BitBoard kNotEastFileBb  = (BitBoard)kNotEastFileMask;

inline constexpr BitBoard kDarkSquareBitBoard  = (BitBoard)0xaa55aa55aa55aa55ULL;
inline constexpr BitBoard kLightSquareBitBoard = (BitBoard)~0xaa55aa55aa55aa55ULL;

inline constexpr std::array kSquareColorBitBoards = {
        kDarkSquareBitBoard,
        kLightSquareBitBoard,
};

[[nodiscard]] constexpr BitBoard getSquareColorBitBoard(int squareColor) {
    return kSquareColorBitBoards[squareColor];
}

[[nodiscard]] BitBoard getFileBitBoard(BoardPosition position);
