#include "GameState.h"

#include "MyAssert.h"

#include <cstdlib>

#include <sstream>

namespace {

constexpr char kLowerCaseBit = 1 << 5;

constexpr bool isNumber(char c) {
    return c >= '0' && c <= '9';
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

std::uint16_t parseHalfMoveClockFromFen(std::string::const_iterator& strIt) {
    const int moveClock = std::atoi(&*strIt);
    // multiply by two to convert to half move clock; minus one because the fen counter starts at 1
    return static_cast<std::uint16_t>(moveClock - 1) * 2;
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

GameState::PieceOccupancyBitBoards getPieceOccupancyBitBoards(
        BoardConfigurationInfo configuration, const Side ownSide) {
    GameState::PieceOccupancyBitBoards occupancy{};

    for (int piece = 0; piece < kNumPieceTypes; ++piece) {
        occupancy.ownPiece =
                any(occupancy.ownPiece, configuration.pieceBitBoards[(int)ownSide][piece]);
        occupancy.enemyPiece = any(
                occupancy.enemyPiece, configuration.pieceBitBoards[(int)nextSide(ownSide)][piece]);
    }

    return occupancy;
}

HashT computeBoardHash(const GameState& gameState) {
    HashT hash = 0;

    for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
        const Side side = (Side)sideIdx;
        for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
            const Piece piece      = (Piece)pieceIdx;
            BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);
            while (pieceBitBoard != BitBoard::Empty) {
                const BoardPosition position = popFirstSetPosition(pieceBitBoard);
                updateHashForPiecePosition(side, piece, position, hash);
            }
        }

        if (gameState.canCastleKingSide(side)) {
            updateHashForKingSideCastlingRights(side, hash);
        }
        if (gameState.canCastleQueenSide(side)) {
            updateHashForQueenSideCastlingRights(side, hash);
        }
    }

    if (gameState.getSideToMove() == Side::Black) {
        updateHashForSideToMove(hash);
    }

    if (BoardPosition enPassantTarget = gameState.getEnPassantTarget();
        enPassantTarget != BoardPosition::Invalid) {
        const int enPassantFile = fileFromPosition(enPassantTarget);
        updateHashForEnPassantFile(enPassantFile, hash);
    }

    return hash;
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

    gameState.halfMoveClock_ = parseHalfMoveClockFromFen(strIt);

    gameState.occupancy_ = getPieceOccupancyBitBoards(boardConfig, gameState.sideToMove_);

    gameState.boardHash_ = computeBoardHash(gameState);

    gameState.previousHashes_.reserve(500);
    gameState.previousHashes_.push_back(gameState.boardHash_);
    gameState.lastReversiblePositionHashIdx_ = 0;

    return gameState;
}

std::string GameState::toFen() const {
    std::ostringstream ss;

    BoardConfigurationInfo boardConfig = {
            .pieceBitBoards = pieceBitBoards_,
            .pieceOnSquare  = pieceOnSquare_,
    };

    const int moveCounter = halfMoveClock_ / 2 + 1;

    ss << toFenNoMoveCounters();
    ss << ' ';
    ss << (unsigned)plySinceCaptureOrPawn_;
    ss << ' ';
    ss << moveCounter;

    return ss.str();
}

std::string GameState::toFenNoMoveCounters() const {
    std::ostringstream ss;

    BoardConfigurationInfo boardConfig = {
            .pieceBitBoards = pieceBitBoards_,
            .pieceOnSquare  = pieceOnSquare_,
    };

    const int moveCounter = halfMoveClock_ / 2 + 1;

    boardConfigurationToFen(boardConfig, ss);
    ss << ' ';
    sideToMoveToFen(sideToMove_, ss);
    ss << ' ';
    castlingRightsToFen(*this, ss);
    ss << ' ';
    enPassantTargetToFen(enPassantTarget_, ss);

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
            const BoardPosition position = positionFromFileRank(file, rank);

            if (position == enPassantTarget_) {
                ss << " * |";
                continue;
            }

            ss << ' ';
            const ColoredPiece coloredPiece = pieceOnSquare_[(int)position];
            if (coloredPiece == ColoredPiece::Invalid) {
                ss << ' ';
            } else {
                ss << toFenChar(coloredPiece);
            }

            bool castleCharacter = false;
            if (coloredPiece == ColoredPiece::WhiteRook) {
                if (canCastleKingSide(Side::White) && position == positionFromFileRank(7, 0)) {
                    castleCharacter = true;
                } else if (
                        canCastleQueenSide(Side::White) && position == positionFromFileRank(0, 0)) {
                    castleCharacter = true;
                }
            } else if (coloredPiece == ColoredPiece::BlackRook) {
                if (canCastleKingSide(Side::Black) && position == positionFromFileRank(7, 7)) {
                    castleCharacter = true;
                } else if (
                        canCastleQueenSide(Side::Black) && position == positionFromFileRank(0, 7)) {
                    castleCharacter = true;
                }
            }

            if (castleCharacter) {
                ss << '*';
            } else {
                ss << ' ';
            }

            ss << '|';
        }

        if (rank == 0) {
            ss << " " << toFenChar(sideToMove_);
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
