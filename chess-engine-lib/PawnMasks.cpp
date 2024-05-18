#include "PawnMasks.h"

#include "BoardConstants.h"

#include <array>

namespace {

[[nodiscard]] constexpr std::uint64_t computeFileMask(const int file) {
    const std::uint64_t fileMask = westFileMask << file;
    return fileMask;
}

[[nodiscard]] constexpr std::uint64_t computeTripleFileMask(const int file) {
    const std::uint64_t fileMask      = westFileMask << file;
    const std::uint64_t fileMaskLeft  = westFileMask << std::max(0, file - 1);
    const std::uint64_t fileMaskRight = westFileMask << std::min(7, file + 1);
    return fileMask | fileMaskLeft | fileMaskRight;
}

[[nodiscard]] constexpr BitBoard computeWhitePassedPawnOpponentMask(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    const std::uint64_t tripleFileMask = computeTripleFileMask(file);
    const std::uint64_t forwardMask    = allMask << (rank + 1);

    return (BitBoard)(forwardMask & tripleFileMask);
}

[[nodiscard]] constexpr BitBoard computeWhitePassedPawnOwnMask(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    const std::uint64_t fileMask    = computeFileMask(file);
    const std::uint64_t forwardMask = allMask << (rank + 1);

    return (BitBoard)(forwardMask & fileMask);
}

[[nodiscard]] constexpr BitBoard computeBlackPassedPawnOpponentMask(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    const std::uint64_t tripleFileMask = computeTripleFileMask(file);
    const std::uint64_t forwardMask    = allMask >> (8 - rank);

    return (BitBoard)(forwardMask & tripleFileMask);
}

[[nodiscard]] constexpr BitBoard computeBlackPassedPawnOwnMask(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    const std::uint64_t fileMask    = computeFileMask(file);
    const std::uint64_t forwardMask = allMask >> (8 - rank);

    return (BitBoard)(forwardMask & fileMask);
}

[[nodiscard]] constexpr std::array<BitBoard, kSquares> computeWhitePassedPawnOpponentMasks() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computeWhitePassedPawnOpponentMask((BoardPosition)i);
    }
    return masks;
}

[[nodiscard]] constexpr std::array<BitBoard, kSquares> computeWhitePassedPawnOwnMasks() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computeWhitePassedPawnOwnMask((BoardPosition)i);
    }
    return masks;
}

[[nodiscard]] constexpr std::array<BitBoard, kSquares> computeBlackPassedPawnOpponentMasks() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computeBlackPassedPawnOpponentMask((BoardPosition)i);
    }
    return masks;
}

[[nodiscard]] constexpr std::array<BitBoard, kSquares> computeBlackPassedPawnOwnMasks() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computeBlackPassedPawnOwnMask((BoardPosition)i);
    }
    return masks;
}

constexpr std::array<std::array<BitBoard, kSquares>, kNumSides> passedPawnOpponentMasks = {
        computeWhitePassedPawnOpponentMasks(),
        computeBlackPassedPawnOpponentMasks(),
};

constexpr std::array<std::array<BitBoard, kSquares>, kNumSides> passedPawnOwnMasks = {
        computeWhitePassedPawnOwnMasks(),
        computeBlackPassedPawnOwnMasks(),
};

}  // namespace

BitBoard getPassedPawnOpponentMask(BoardPosition position, Side side) {
    return passedPawnOpponentMasks[(int)side][(int)position];
}

BitBoard getPassedPawnOwnMask(BoardPosition position, Side side) {
    return passedPawnOwnMasks[(int)side][(int)position];
}
