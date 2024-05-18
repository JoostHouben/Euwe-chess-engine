#include "BoardPosition.h"

#include "BoardConstants.h"

#include <array>

namespace {

[[nodiscard]] constexpr BoardPosition calculateVerticalReflection(BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);
    return positionFromFileRank(7 - file, 7 - rank);
}

[[nodiscard]] constexpr std::array<BoardPosition, kSquares> calculateVerticalReflections() {
    std::array<BoardPosition, kSquares> result;
    for (int positionIdx = 0; positionIdx < kSquares; ++positionIdx) {
        result[positionIdx] = calculateVerticalReflection((BoardPosition)positionIdx);
    }
    return result;
}

constexpr std::array<BoardPosition, kSquares> kVerticalReflections = calculateVerticalReflections();

}  // namespace

BoardPosition getVerticalReflection(BoardPosition position) {
    return kVerticalReflections[static_cast<int>(position)];
}
