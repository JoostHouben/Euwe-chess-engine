#include "PawnMasks.h"

#include "BoardConstants.h"

#include <array>

namespace {

[[nodiscard]] constexpr std::uint64_t computeFileMask(const int file) {
    const std::uint64_t fileMask = westFileMask << file;
    return fileMask;
}

[[nodiscard]] constexpr BitBoard computeWhitePawnForwardMask(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    const std::uint64_t fileMask    = computeFileMask(file);
    const std::uint64_t forwardMask = allMask << (rank + 1);

    return (BitBoard)(forwardMask & fileMask);
}

[[nodiscard]] constexpr BitBoard computeBlackPawnForwardMask(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    const std::uint64_t fileMask    = computeFileMask(file);
    const std::uint64_t forwardMask = allMask >> (8 - rank);

    return (BitBoard)(forwardMask & fileMask);
}

[[nodiscard]] constexpr std::array<BitBoard, kSquares> computeWhitePawnForwardMasks() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computeWhitePawnForwardMask((BoardPosition)i);
    }
    return masks;
}

[[nodiscard]] constexpr std::array<BitBoard, kSquares> computeBlackPawnForwardMasks() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computeBlackPawnForwardMask((BoardPosition)i);
    }
    return masks;
}

constexpr std::array<std::array<BitBoard, kSquares>, kNumSides> kForwardMasks = {
        computeWhitePawnForwardMasks(),
        computeBlackPawnForwardMasks(),
};

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

[[nodiscard]] constexpr BitBoard computeBlackPassedPawnOpponentMask(const BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);

    const std::uint64_t tripleFileMask = computeTripleFileMask(file);
    const std::uint64_t forwardMask    = allMask >> (8 - rank);

    return (BitBoard)(forwardMask & tripleFileMask);
}

[[nodiscard]] constexpr std::array<BitBoard, kSquares> computeWhitePassedPawnOpponentMasks() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computeWhitePassedPawnOpponentMask((BoardPosition)i);
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

constexpr std::array<std::array<BitBoard, kSquares>, kNumSides> kPassedPawnOpponentMasks = {
        computeWhitePassedPawnOpponentMasks(),
        computeBlackPassedPawnOpponentMasks(),
};

[[nodiscard]] constexpr BitBoard computePawnNeighborFileMask(BoardPosition position) {
    const auto [file, _] = fileRankFromPosition(position);

    std::uint64_t result = 0;

    if (file > 0) {
        result |= computeFileMask(file - 1);
    }
    if (file < 7) {
        result |= computeFileMask(file + 1);
    }

    return (BitBoard)result;
}

constexpr std::array<BitBoard, kSquares> kPawnNeighborFileMasks = []() {
    std::array<BitBoard, kSquares> masks{};
    for (int i = 0; i < kSquares; ++i) {
        masks[i] = computePawnNeighborFileMask((BoardPosition)i);
    }
    return masks;
}();

}  // namespace

BitBoard getPassedPawnOpponentMask(BoardPosition position, Side side) {
    return kPassedPawnOpponentMasks[(int)side][(int)position];
}

BitBoard getPawnForwardMask(BoardPosition position, Side side) {
    return kForwardMasks[(int)side][(int)position];
}

BitBoard getPawnNeighborFileMask(BoardPosition position) {
    return kPawnNeighborFileMasks[(int)position];
}
