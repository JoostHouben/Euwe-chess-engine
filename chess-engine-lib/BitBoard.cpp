#include "BitBoard.h"

#include "BoardConstants.h"

#include <array>
#include <sstream>
#include <string>

namespace {

constexpr BitBoard computeFileBitBoard(const BoardPosition position) {
    const int file = fileFromPosition(position);
    return (BitBoard)(westFileMask << file);
}

constexpr std::array<BitBoard, kSquares> computeFileBitBoards() {
    std::array<BitBoard, kSquares> fileBitBoards;
    for (int position = 0; position < kSquares; ++position) {
        fileBitBoards[position] = computeFileBitBoard((BoardPosition)position);
    }
    return fileBitBoards;
}

constexpr std::array<BitBoard, kSquares> kFileBitBoards = computeFileBitBoards();

}  // namespace

std::string bitBoardToVisualString(BitBoard bitboard) {
    const std::string boardTop = "  .-------------------------------.\n";
    const std::string boardSep = "  |---+---+---+---+---+---+---+---|\n";
    const std::string boardBot = "  '-------------------------------'\n";

    std::ostringstream ss;
    ss << boardTop;

    for (int rank = 7; rank >= 0; --rank) {
        ss << rank + 1 << " |";
        for (int file = 0; file < 8; ++file) {
            ss << ' ';
            BoardPosition position = positionFromFileRank(file, rank);
            if (isSet(bitboard, position)) {
                ss << 'X';
            } else {
                ss << ' ';
            }
            ss << " |";
        }
        ss << "\n";
        if (rank > 0) {
            ss << boardSep;
        }
    }
    ss << boardBot;
    ss << "    a   b   c   d   e   f   g   h\n";

    return ss.str();
}

BitBoard getFileBitBoard(BoardPosition position) {
    return kFileBitBoards[(int)position];
}
