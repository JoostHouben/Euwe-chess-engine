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

struct BoardConfigurationInfo {
    std::array<std::array<BitBoard, kNumPieceTypes>, kNumSides> pieceBitBoards = {};
    std::array<ColoredPiece, kSquares> pieceOnSquare                           = {};
};

BoardConfigurationInfo parseBoardConfigurationFromFen(std::string::const_iterator& strIt) {
    BoardConfigurationInfo boardConfiguration = {};
    boardConfiguration.pieceOnSquare.fill(ColoredPiece::Invalid);

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

            set(boardConfiguration.pieceBitBoards[(int)side][(int)piece], position);

            boardConfiguration.pieceOnSquare[(int)position] = coloredPiece;

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

void boardConfigurationToFen(const BoardConfigurationInfo& boardConfig, std::ostream& out) {
    for (int rank = 7; rank >= 0; --rank) {
        int numEmptyTiles = 0;
        for (int file = 0; file < 8; ++file) {
            const ColoredPiece coloredPiece =
                    boardConfig.pieceOnSquare[(int)positionFromFileRank(file, rank)];
            if (coloredPiece == ColoredPiece::Invalid) {
                ++numEmptyTiles;
                continue;
            }

            if (numEmptyTiles) {
                out << numEmptyTiles;
                numEmptyTiles = 0;
            }

            out << toFenChar(coloredPiece);
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
        occupancy.ownPiece =
                any(occupancy.ownPiece, configuration.pieceBitBoards[(int)ownSide][piece]);
        occupancy.enemyPiece = any(
                occupancy.enemyPiece, configuration.pieceBitBoards[(int)nextSide(ownSide)][piece]);
    }

    return occupancy;
}

}  // namespace

GameState GameState::fromFen(const std::string& fenString) {
    GameState gameState{};

    auto strIt = fenString.begin();

    BoardConfigurationInfo boardConfig = parseBoardConfigurationFromFen(strIt);
    gameState.pieceBitBoards_          = boardConfig.pieceBitBoards;
    gameState.pieceOnSquare_           = boardConfig.pieceOnSquare;
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

    gameState.occupancy_ = getPieceOccupancyBitBoards(boardConfig, gameState.sideToMove_);

    return gameState;
}

std::string GameState::toFen(int moveCounter) const {
    std::ostringstream ss;

    BoardConfigurationInfo boardConfig = {
            .pieceBitBoards = pieceBitBoards_,
            .pieceOnSquare  = pieceOnSquare_,
    };

    boardConfigurationToFen(boardConfig, ss);
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
    const std::string boardTop = "  .-------------------------------.\n";
    const std::string boardSep = "  |---+---+---+---+---+---+---+---|\n";
    const std::string boardBot = "  '-------------------------------'\n";

    std::ostringstream ss;
    ss << boardTop;

    for (int rank = 7; rank >= 0; --rank) {
        ss << rank + 1 << " |";
        for (int file = 0; file < 8; ++file) {
            ss << ' ';
            const ColoredPiece coloredPiece = pieceOnSquare_[(int)positionFromFileRank(file, rank)];
            if (coloredPiece == ColoredPiece::Invalid) {
                ss << ' ';
            } else {
                ss << toFenChar(coloredPiece);
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
