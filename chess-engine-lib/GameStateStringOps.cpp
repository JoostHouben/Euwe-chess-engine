#include "GameState.h"

#include "MyAssert.h"

#include <charconv>
#include <sstream>
#include <stdexcept>

#include <cstdlib>

namespace {

template <typename IteratorT, typename EndIteratorT>
bool safeAdvance(IteratorT& it, const EndIteratorT end) {
    if (it == end) {
        return false;
    }
    ++it;
    return true;
}

template <typename IteratorT, typename EndIteratorT>
void tryAdvance(IteratorT& it, const EndIteratorT end) {
    if (!safeAdvance(it, end)) {
        throw std::range_error("Unexpected end of input");
    }
}

[[nodiscard]] int parseIntInFenString(
        std::string_view::const_iterator& strIt,
        const std::string_view::const_iterator endIt,
        std::string_view valueDescription) {
    int value{};
    const std::size_t distanceToEnd = endIt - strIt;
    const char* strStart            = &*strIt;
    const auto result               = std::from_chars(strStart, strStart + distanceToEnd, value);

    if (result.ec != std::errc{}) {
        throw std::invalid_argument(std::format(
                "Invalid {} in FEN string: unable to parse integer from: {}",
                valueDescription,
                std::string_view(strIt, endIt)));
    }

    const std::size_t charsRead = result.ptr - &*strIt;
    for (std::size_t i = 0; i < charsRead; ++i) {
        tryAdvance(strIt, endIt);
    }

    return value;
}

constexpr bool isNumber(char c) {
    return c >= '0' && c <= '9';
}

struct BoardConfigurationInfo {
    std::array<std::array<BitBoard, kNumPieceTypes>, kNumSides> pieceBitBoards = {};
    std::array<ColoredPiece, kSquares> pieceOnSquare                           = {};
};

BoardConfigurationInfo parseBoardConfigurationFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    BoardConfigurationInfo boardConfiguration = {};
    boardConfiguration.pieceOnSquare.fill(ColoredPiece::Invalid);

    for (int rank = 7; rank >= 0; --rank) {
        for (int file = 0; file < 8; tryAdvance(strIt, endIt)) {
            if (isNumber(*strIt)) {
                file += (*strIt - '0');
                if (file > 8) {
                    throw std::invalid_argument("Invalid FEN string: too many pieces in rank");
                }
                continue;
            }
            const ColoredPiece coloredPiece = coloredPieceFromFenChar(*strIt);
            const BoardPosition position    = positionFromFileRank(file, rank);
            const Side side                 = getSide(coloredPiece);
            const Piece piece               = getPiece(coloredPiece);

            boardConfiguration.pieceBitBoards[(int)side][(int)piece] |= position;

            boardConfiguration.pieceOnSquare[(int)position] = coloredPiece;

            file += 1;
        }

        const bool validChar = (rank > 0 && *strIt == '/') || (rank == 0 && *strIt == ' ');
        if (!validChar) {
            throw std::invalid_argument(std::format(
                    "Unexpected character {} in FEN string, starting at: {}",
                    *strIt,
                    std::string_view(strIt, endIt)));
        }

        if (rank > 0) {
            tryAdvance(strIt, endIt);
        }
    }

    return boardConfiguration;
}

Side parseSideToMoveFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    const char c = *strIt;
    tryAdvance(strIt, endIt);
    switch (c) {
        case 'w':
            return Side::White;
        case 'b':
            return Side::Black;
        default:
            throw std::invalid_argument(std::format("Invalid side to move in FEN string: {}", c));
    }
}

void parseCastlingRightsFromFen(
        std::string_view::const_iterator& strIt,
        const std::string_view::const_iterator endIt,
        GameState::CastlingRights& castlingRights) {
    if (*strIt == '-') {
        tryAdvance(strIt, endIt);
        return;
    }

    for (; *strIt != ' '; tryAdvance(strIt, endIt)) {
        const Side side   = sideFromFenChar(*strIt);
        const Piece piece = pieceFromFenChar(*strIt);

        int bit{};
        switch (piece) {
            case Piece::King:
                bit = (int)GameState::CastlingRights::KingSide << ((int)side * 2);
                break;
            case Piece::Queen:
                bit = (int)GameState::CastlingRights::QueenSide << ((int)side * 2);
                break;
            default:
                throw std::invalid_argument(
                        std::format("Invalid character for castling rights: {}", *strIt));
        }

        castlingRights = (GameState::CastlingRights)((int)castlingRights | bit);
    }
}

BoardPosition parseEnPassantTargetFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    if (*strIt == '-') {
        tryAdvance(strIt, endIt);
        return BoardPosition::Invalid;
    }

    const std::size_t charsRemaining = endIt - strIt;
    if (charsRemaining < 2) {
        throw std::invalid_argument("Unexpected end of FEN string.");
    }

    const BoardPosition enPassantTarget = positionFromAlgebraic({strIt, strIt + 2});
    tryAdvance(strIt, endIt);
    tryAdvance(strIt, endIt);

    return enPassantTarget;
}

std::uint8_t parsePlySinceCaptureOrPawnFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    const int plySinceCaptureOrPawn =
            parseIntInFenString(strIt, endIt, "ply since capture or pawn");

    return static_cast<std::uint8_t>(plySinceCaptureOrPawn);
}

std::uint16_t parseHalfMoveClockFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    const int moveClock = parseIntInFenString(strIt, endIt, "move clock");

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
        occupancy.ownPiece = occupancy.ownPiece | configuration.pieceBitBoards[(int)ownSide][piece];
        occupancy.enemyPiece =
                occupancy.enemyPiece | configuration.pieceBitBoards[(int)nextSide(ownSide)][piece];
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

GameState GameState::fromFen(std::string_view fenString) {
    if (fenString.empty()) {
        throw std::invalid_argument("FEN string invalid: empty");
    }

    GameState gameState{};

    auto strIt       = fenString.begin();
    const auto endIt = fenString.end();

    const auto advanceWordEnd = [&]() {
        const std::size_t position = (strIt - fenString.begin()) + 1;
        if ((strIt != endIt) && *strIt != ' ') {
            throw std::invalid_argument(
                    std::format("Invalid FEN string: expected space at character #{}", position));
        }
        if (!safeAdvance(strIt, endIt)) {
            throw std::invalid_argument(std::format(
                    "Invalid FEN string: unexpected end of string at character #{}", position));
        }
    };

    BoardConfigurationInfo boardConfig = parseBoardConfigurationFromFen(strIt, endIt);
    gameState.pieceBitBoards_          = boardConfig.pieceBitBoards;
    gameState.pieceOnSquare_           = boardConfig.pieceOnSquare;
    advanceWordEnd();

    gameState.sideToMove_ = parseSideToMoveFromFen(strIt, endIt);
    advanceWordEnd();

    parseCastlingRightsFromFen(strIt, endIt, gameState.castlingRights_);
    advanceWordEnd();

    gameState.enPassantTarget_ = parseEnPassantTargetFromFen(strIt, endIt);
    advanceWordEnd();

    gameState.plySinceCaptureOrPawn_ = parsePlySinceCaptureOrPawnFromFen(strIt, endIt);
    advanceWordEnd();

    gameState.halfMoveClock_ = parseHalfMoveClockFromFen(strIt, endIt);

    if (strIt != endIt) {
        throw std::invalid_argument(std::format(
                "Invalid FEN string: unexpected characters at end of string: {}",
                std::string_view(strIt, endIt)));
    }

    gameState.occupancy_ = getPieceOccupancyBitBoards(boardConfig, gameState.sideToMove_);

    gameState.boardHash_ = computeBoardHash(gameState);

    gameState.previousHashes_.reserve(500);
    gameState.previousHashes_.push_back(gameState.boardHash_);
    gameState.lastReversiblePositionHashIdx_ = 0;

    return gameState;
}

std::string GameState::toFen() const {
    std::ostringstream ss;

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
