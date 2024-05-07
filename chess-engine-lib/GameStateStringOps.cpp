#include "GameState.h"

#include "MyAssert.h"

#include <cstdlib>

#include <bit>
#include <map>
#include <sstream>

namespace {

constexpr char kLowerCaseBit = 1 << 5;

constexpr bool isNumber(char c) {
    return c >= '0' && c <= '9';
}

constexpr Piece pieceFromFenChar(char c) {
    char upperCase = c & ~kLowerCaseBit;
    switch (upperCase) {
        case 'P':
            return Piece::Pawn;
        case 'N':
            return Piece::Knight;
        case 'B':
            return Piece::Bishop;
        case 'R':
            return Piece::Rook;
        case 'Q':
            return Piece::Queen;
        case 'K':
            return Piece::King;
    }
    UNREACHABLE;
}

constexpr Side sideFromFenChar(char c) {
    bool isUpperCase = !(c & kLowerCaseBit);
    if (isUpperCase) {
        return Side::White;
    } else {
        return Side::Black;
    }
}

constexpr char toFenChar(Side side) {
    switch (side) {
        case Side::White:
            return 'w';
        case Side::Black:
            return 'b';
        case Side::None:
            return 'x';
    }
    UNREACHABLE;
}

constexpr ColoredPiece coloredPieceFromFenChar(char c) {
    return getColoredPiece(pieceFromFenChar(c), sideFromFenChar(c));
}

constexpr char toFenChar(Piece piece) {
    switch (piece) {
        case Piece::Pawn:
            return 'P';
        case Piece::Knight:
            return 'N';
        case Piece::Bishop:
            return 'B';
        case Piece::Rook:
            return 'R';
        case Piece::Queen:
            return 'Q';
        case Piece::King:
            return 'K';
        default:
            UNREACHABLE;
    }
}

constexpr char toFenChar(ColoredPiece coloredPiece) {
    char c = toFenChar(getPiece(coloredPiece));
    if (getSide(coloredPiece) == Side::Black) {
        c |= kLowerCaseBit;
    }
    return c;
}

using BoardConfigurationInfo = std::array<std::array<BitBoard, kNumPieceTypes>, kNumSides>;

BoardConfigurationInfo parseBoardConfigurationFromFen(std::string::const_iterator& strIt) {
    BoardConfigurationInfo boardConfiguration = {};

    for (int rank = 7; rank >= 0; --rank) {
        for (int file = 0; file < 8; ++strIt) {
            if (isNumber(*strIt)) {
                file += (*strIt - '0');
                continue;
            }
            const ColoredPiece coloredPiece = coloredPieceFromFenChar(*strIt);
            const BoardPosition position    = positionFromFileRank(file, rank);
            const Side side                 = getSide(coloredPiece);
            const Piece piece               = getPiece(coloredPiece);

            set(boardConfiguration[(int)side][(int)piece], position);

            file += 1;
        }
        MY_ASSERT((rank > 0 && *strIt == '/') || (rank == 0 && *strIt == ' '));
        if (rank > 0) {
            ++strIt;
        }
    }

    return boardConfiguration;
}

Side parseSideToMoveFromFen(std::string::const_iterator& strIt) {
    switch (*(strIt++)) {
        case 'w':
            return Side::White;
        case 'b':
            return Side::Black;
    }
    UNREACHABLE;
}

void parseCastlingRightsFromFen(
        std::string::const_iterator& strIt, GameState::CastlingRights& castlingRights) {
    if (*strIt == '-') {
        ++strIt;
        return;
    }

    for (; *strIt != ' '; ++strIt) {
        Side side   = sideFromFenChar(*strIt);
        Piece piece = pieceFromFenChar(*strIt);

        int bit;
        switch (piece) {
            case Piece::King:
                bit = (int)GameState::CastlingRights::KingSide << ((int)side * 2);
                break;
            case Piece::Queen:
                bit = (int)GameState::CastlingRights::QueenSide << ((int)side * 2);
                break;
            default:
                UNREACHABLE;
        }

        castlingRights = (GameState::CastlingRights)((int)castlingRights | bit);
    }
}

BoardPosition parseEnPassantTargetFromFen(std::string::const_iterator& strIt) {
    if (*strIt == '-') {
        ++strIt;
        return BoardPosition::Invalid;
    }

    BoardPosition enPassantTarget = positionFromAlgebraic({strIt, strIt + 2});
    strIt += 2;
    return enPassantTarget;
}

std::uint8_t parsePlySinceCaptureOrPawnFromFen(std::string::const_iterator& strIt) {
    int plySinceCaptureOrPawn = std::atoi(&*strIt);
    do {
        ++strIt;
    } while (*strIt != ' ');
    return static_cast<std::uint8_t>(plySinceCaptureOrPawn);
}

std::map<BoardPosition, ColoredPiece> getPositionToPieceMap(
        const BoardConfigurationInfo& boardConfig) {
    std::map<BoardPosition, ColoredPiece> positionToPiece;
    for (int side = 0; side < kNumSides; ++side) {
        for (int piece = 0; piece < kNumPieceTypes; ++piece) {
            BitBoard pieceBitBoard = boardConfig[side][piece];
            while (pieceBitBoard != BitBoard::Empty) {
                BoardPosition position =
                        (BoardPosition)std::countr_zero((std::uint64_t)pieceBitBoard);
                clear(pieceBitBoard, position);
                positionToPiece.emplace(position, getColoredPiece((Piece)piece, (Side)side));
            }
        }
    }

    return positionToPiece;
}

void boardConfigurationToFen(const BoardConfigurationInfo& boardConfig, std::ostream& out) {
    std::map<BoardPosition, ColoredPiece> positionToPiece = getPositionToPieceMap(boardConfig);
    for (int rank = 7; rank >= 0; --rank) {
        int numEmptyTiles = 0;
        for (int file = 0; file < 8; ++file) {
            auto pieceIt = positionToPiece.find(positionFromFileRank(file, rank));
            if (pieceIt == positionToPiece.end()) {
                ++numEmptyTiles;
                continue;
            }

            if (numEmptyTiles) {
                out << numEmptyTiles;
                numEmptyTiles = 0;
            }

            out << toFenChar(pieceIt->second);
        }

        if (numEmptyTiles) {
            out << numEmptyTiles;
        }

        if (rank > 0) {
            out << "/";
        }
    }
}

void sideToMoveToFen(Side side, std::ostream& out) {
    out << toFenChar(side);
}

void castlingRightsToFen(const GameState& gameState, std::ostream& out) {
    bool any = false;
    for (auto side : {Side::White, Side::Black}) {
        if (gameState.canCastleKingSide(side)) {
            any = true;
            out << toFenChar(getColoredPiece(Piece::King, side));
        }
        if (gameState.canCastleQueenSide(side)) {
            any = true;
            out << toFenChar(getColoredPiece(Piece::Queen, side));
        }
    }
    if (!any) {
        out << '-';
    }
}

void enPassantTargetToFen(BoardPosition enPassantTarget, std::ostream& out) {
    if (enPassantTarget == BoardPosition::Invalid) {
        out << '-';
    } else {
        out << algebraicFromPosition(enPassantTarget);
    }
}

PieceOccupancyBitBoards getPieceOccupancyBitBoards(
        BoardConfigurationInfo configuration, const Side ownSide) {
    PieceOccupancyBitBoards occupancy{};

    for (int piece = 0; piece < kNumPieceTypes; ++piece) {
        occupancy.ownPiece = any(occupancy.ownPiece, configuration[(int)ownSide][piece]);
        occupancy.enemyPiece =
                any(occupancy.enemyPiece, configuration[(int)nextSide(ownSide)][piece]);
    }

    return occupancy;
}

}  // namespace

GameState GameState::fromFen(const std::string& fenString) {
    GameState gameState{};

    auto strIt = fenString.begin();

    gameState.pieceBitBoards_ = parseBoardConfigurationFromFen(strIt);
    MY_ASSERT(*strIt == ' ');
    ++strIt;

    gameState.sideToMove_ = parseSideToMoveFromFen(strIt);
    MY_ASSERT(*strIt == ' ');
    ++strIt;

    parseCastlingRightsFromFen(strIt, gameState.castlingRights_);
    MY_ASSERT(*strIt == ' ');
    ++strIt;

    gameState.enPassantTarget_ = parseEnPassantTargetFromFen(strIt);
    MY_ASSERT(*strIt == ' ');
    ++strIt;

    gameState.plySinceCaptureOrPawn_ = parsePlySinceCaptureOrPawnFromFen(strIt);
    MY_ASSERT(*strIt == ' ');
    ++strIt;

    // Ignore: move counter

    MY_ASSERT(strIt < fenString.end());

    gameState.occupancy_ =
            getPieceOccupancyBitBoards(gameState.pieceBitBoards_, gameState.sideToMove_);

    return gameState;
}

std::string GameState::toFen(int moveCounter) const {
    std::ostringstream ss;

    boardConfigurationToFen(pieceBitBoards_, ss);
    ss << ' ';
    sideToMoveToFen(sideToMove_, ss);
    ss << ' ';
    castlingRightsToFen(*this, ss);
    ss << ' ';
    enPassantTargetToFen(enPassantTarget_, ss);
    ss << ' ';
    ss << (unsigned)plySinceCaptureOrPawn_;
    ss << ' ';
    ss << moveCounter;

    return ss.str();
}

std::string GameState::toVisualString() const {
    std::map<BoardPosition, ColoredPiece> positionToPiece = getPositionToPieceMap(pieceBitBoards_);

    const std::string boardTop = "  .-------------------------------.\n";
    const std::string boardSep = "  |---+---+---+---+---+---+---+---|\n";
    const std::string boardBot = "  '-------------------------------'\n";

    std::ostringstream ss;
    ss << boardTop;

    for (int rank = 7; rank >= 0; --rank) {
        ss << rank + 1 << " |";
        for (int file = 0; file < 8; ++file) {
            ss << ' ';
            auto pieceIt = positionToPiece.find(positionFromFileRank(file, rank));
            if (pieceIt == positionToPiece.end()) {
                ss << ' ';
            } else {
                ss << toFenChar(pieceIt->second);
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
