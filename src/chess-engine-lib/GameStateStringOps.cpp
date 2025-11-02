#include "GameState.h"

#include "MyAssert.h"

#include <charconv>
#include <expected>
#include <format>
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
std::expected<void, std::string> tryAdvance(IteratorT& it, const EndIteratorT end) {
    if (!safeAdvance(it, end)) {
        return std::unexpected(std::string("Unexpected end of input"));
    }
    return {};
}

template <typename IteratorT, typename EndIteratorT>
std::expected<void, std::string> checkStrItValid(const IteratorT it, const EndIteratorT end) {
    if (it == end) {
        return std::unexpected(std::string("Unexpected end of input"));
    }
    return {};
}

[[nodiscard]] std::expected<std::optional<int>, std::string> parseIntInFenString(
        std::string_view::const_iterator& strIt,
        const std::string_view::const_iterator endIt,
        std::string_view valueDescription) {
    if (strIt == endIt) {
        return std::nullopt;
    }

    int value{};
    // Note that dereferencing endIt is unsafe. So we need to use pointer arithmetic here in order
    // to use from_chars.
    const std::size_t distanceToEnd = endIt - strIt;
    const char* strStart            = &*strIt;
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    const auto result = std::from_chars(strStart, strStart + distanceToEnd, value);

    if (result.ec != std::errc{}) {
        return std::unexpected(std::format(
                "Invalid {} in FEN string: unable to parse integer from: {}",
                valueDescription,
                std::string_view(strIt, endIt)));
    }

    const std::size_t charsRead = result.ptr - &*strIt;
    for (std::size_t i = 0; i < charsRead; ++i) {
        ++strIt;
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

std::expected<BoardConfigurationInfo, std::string> parseBoardConfigurationFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    BoardConfigurationInfo boardConfiguration = {};
    boardConfiguration.pieceOnSquare.fill(ColoredPiece::Invalid);

    for (int rank = 7; rank >= 0; --rank) {
        for (int file = 0; file < 8;) {
            if (strIt == endIt)
                return std::unexpected(std::string("Unexpected end of input"));
            if (isNumber(*strIt)) {
                file += (*strIt - '0');
                if (file > 8) {
                    return std::unexpected(
                            std::format("Invalid FEN string: too many pieces in rank"));
                }
                // advance iterator
                auto r = tryAdvance(strIt, endIt);
                if (!r)
                    return std::unexpected(r.error());
                continue;
            }
            const auto coloredPiece = coloredPieceFromFenChar(*strIt);
            if (!coloredPiece) {
                return std::unexpected(coloredPiece.error());
            }
            const BoardPosition position = positionFromFileRank(file, rank);
            const Side side              = getSide(*coloredPiece);
            const Piece piece            = getPiece(*coloredPiece);

            boardConfiguration.pieceBitBoards[(int)side][(int)piece] |= position;

            boardConfiguration.pieceOnSquare[(int)position] = *coloredPiece;

            file += 1;
            auto r = tryAdvance(strIt, endIt);
            if (!r)
                return std::unexpected(r.error());
        }

        const bool validChar = (rank > 0 && strIt != endIt && *strIt == '/')
                            || (rank == 0 && (strIt == endIt || *strIt == ' '));
        if (!validChar) {
            if (strIt == endIt)
                return std::unexpected(std::string("Unexpected end of input"));
            return std::unexpected(std::format(
                    "Unexpected character {} in FEN string, starting at: {}",
                    *strIt,
                    std::string_view(strIt, endIt)));
        }

        if (rank > 0) {
            auto r = tryAdvance(strIt, endIt);
            if (!r)
                return std::unexpected(r.error());
        }
    }

    return boardConfiguration;
}

std::expected<Side, std::string> parseSideToMoveFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    if (auto r = checkStrItValid(strIt, endIt); !r)
        return std::unexpected(r.error());
    const char c = *strIt;
    if (auto r = tryAdvance(strIt, endIt); !r)
        return std::unexpected(r.error());
    switch (c) {
        case 'w':
            return Side::White;
        case 'b':
            return Side::Black;
        default:
            return std::unexpected(std::format("Invalid side to move in FEN string: {}", c));
    }
}

std::expected<void, std::string> parseCastlingRightsFromFen(
        std::string_view::const_iterator& strIt,
        const std::string_view::const_iterator endIt,
        GameState::CastlingRights& castlingRights) {
    if (auto r = checkStrItValid(strIt, endIt); !r)
        return r;

    if (*strIt == '-') {
        if (auto r = tryAdvance(strIt, endIt); !r)
            return r;
        return {};
    }

    for (; strIt != endIt && *strIt != ' ';) {
        const Side side  = sideFromFenChar(*strIt);
        const auto piece = pieceFromFenChar(*strIt);
        if (!piece) {
            return std::unexpected(piece.error());
        }

        int bit{};
        switch (*piece) {
            case Piece::King:
                bit = (int)GameState::CastlingRights::KingSide << ((int)side * 2);
                break;
            case Piece::Queen:
                bit = (int)GameState::CastlingRights::QueenSide << ((int)side * 2);
                break;
            default:
                return std::unexpected(
                        std::format("Invalid character for castling rights: {}", *strIt));
        }

        castlingRights = (GameState::CastlingRights)((int)castlingRights | bit);

        if (auto r = tryAdvance(strIt, endIt); !r)
            return r;
    }
    return {};
}

std::expected<BoardPosition, std::string> parseEnPassantTargetFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    if (auto r = checkStrItValid(strIt, endIt); !r)
        return std::unexpected(r.error());

    if (*strIt == '-') {
        if (auto r = tryAdvance(strIt, endIt); !r)
            return std::unexpected(r.error());
        return BoardPosition::Invalid;
    }

    const std::size_t charsRemaining = endIt - strIt;
    if (charsRemaining < 2) {
        return std::unexpected(std::string("Unexpected end of FEN string."));
    }

    const auto enPassantTarget = positionFromAlgebraic({strIt, strIt + 2});
    if (!enPassantTarget) {
        return std::unexpected(enPassantTarget.error());
    }
    if (auto r = tryAdvance(strIt, endIt); !r)
        return std::unexpected(r.error());
    if (auto r = tryAdvance(strIt, endIt); !r)
        return std::unexpected(r.error());

    return enPassantTarget;
}

std::expected<std::uint8_t, std::string> parsePlySinceCaptureOrPawnFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    const auto parseResult = parseIntInFenString(strIt, endIt, "ply since capture or pawn");
    if (!parseResult.has_value()) {
        return std::unexpected(parseResult.error());
    }
    const int plySinceCaptureOrPawn = parseResult->value_or(0);
    return static_cast<std::uint8_t>(plySinceCaptureOrPawn);
}

std::expected<std::uint16_t, std::string> parseHalfMoveClockFromFen(
        std::string_view::const_iterator& strIt, const std::string_view::const_iterator endIt) {
    const auto parseResult = parseIntInFenString(strIt, endIt, "move clock");
    if (!parseResult.has_value()) {
        return std::unexpected(parseResult.error());
    }
    const int moveClock = parseResult->value_or(1);
    // multiply by two to convert to half move clock; minus one because the fen counter starts at 1
    return static_cast<std::uint16_t>((moveClock - 1) * 2);
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

std::array<BitBoard, kNumSides> getPieceOccupancyBitBoards(BoardConfigurationInfo configuration) {
    std::array<BitBoard, kNumSides> occupancy{};

    for (int piece = 0; piece < kNumPieceTypes; ++piece) {
        for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
            occupancy[sideIdx] |= configuration.pieceBitBoards[sideIdx][piece];
        }
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

HashT computePawnKingHash(const GameState& gameState) {
    HashT hash = 0;

    for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
        const Side side = (Side)sideIdx;
        for (const Piece& piece : {Piece::Pawn, Piece::King}) {
            BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, piece);
            while (pieceBitBoard != BitBoard::Empty) {
                const BoardPosition position = popFirstSetPosition(pieceBitBoard);
                updateHashForPiecePosition(side, piece, position, hash);
            }
        }
    }

    if (gameState.getSideToMove() == Side::Black) {
        updateHashForSideToMove(hash);
    }

    return hash;
}

}  // namespace

std::expected<GameState, std::string> GameState::fromFen(std::string_view fenString) {
    if (fenString.empty()) {
        return std::unexpected(std::string("FEN string invalid: empty"));
    }

    GameState gameState{};

    auto strIt       = fenString.begin();
    const auto endIt = fenString.end();

    const auto advanceWordEnd = [&](const bool allowEnd =
                                            false) -> std::expected<void, std::string> {
        //const std::size_t position = (strIt - fenString.begin()) + 1;
        if ((strIt != endIt) && *strIt != ' ') {
            return std::unexpected(
                    std::string("Invalid FEN string: expected space at word boundary"));
        }
        if (!safeAdvance(strIt, endIt) && !allowEnd) {
            return std::unexpected(std::string("Invalid FEN string: unexpected end of string"));
        }
        return {};
    };

    auto boardConfigRes = parseBoardConfigurationFromFen(strIt, endIt);
    if (!boardConfigRes)
        return std::unexpected(boardConfigRes.error());
    BoardConfigurationInfo boardConfig = boardConfigRes.value();

    gameState.pieceBitBoards_ = boardConfig.pieceBitBoards;
    gameState.pieceOnSquare_  = boardConfig.pieceOnSquare;

    if (auto r = advanceWordEnd(); !r)
        return std::unexpected(r.error());

    auto sideRes = parseSideToMoveFromFen(strIt, endIt);
    if (!sideRes)
        return std::unexpected(sideRes.error());
    gameState.sideToMove_ = sideRes.value();

    if (auto r = advanceWordEnd(); !r)
        return std::unexpected(r.error());

    if (auto r = parseCastlingRightsFromFen(strIt, endIt, gameState.castlingRights_); !r)
        return std::unexpected(r.error());
    if (auto r = advanceWordEnd(); !r)
        return std::unexpected(r.error());

    auto epRes = parseEnPassantTargetFromFen(strIt, endIt);
    if (!epRes)
        return std::unexpected(epRes.error());
    gameState.enPassantTarget_ = epRes.value();

    if (auto r = advanceWordEnd(/*allowEnd =*/true); !r)
        return std::unexpected(r.error());

    auto plyRes = parsePlySinceCaptureOrPawnFromFen(strIt, endIt);
    if (!plyRes)
        return std::unexpected(plyRes.error());
    gameState.plySinceCaptureOrPawn_ = plyRes.value();

    if (auto r = advanceWordEnd(/*allowEnd =*/true); !r)
        return std::unexpected(r.error());

    auto halfRes = parseHalfMoveClockFromFen(strIt, endIt);
    if (!halfRes)
        return std::unexpected(halfRes.error());
    gameState.halfMoveClock_ = halfRes.value();

    if (strIt != endIt) {
        return std::unexpected(std::format(
                "Invalid FEN string: unexpected characters at end of string: {}",
                std::string_view(strIt, endIt)));
    }

    gameState.occupancy_ = getPieceOccupancyBitBoards(boardConfig);

    gameState.boardHash_    = computeBoardHash(gameState);
    gameState.pawnKingHash_ = computePawnKingHash(gameState);

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
                if (canCastleKingSide(Side::White) && position == BoardPosition::H1) {
                    castleCharacter = true;
                } else if (canCastleQueenSide(Side::White) && position == BoardPosition::A1) {
                    castleCharacter = true;
                }
            } else if (coloredPiece == ColoredPiece::BlackRook) {
                if (canCastleKingSide(Side::Black) && position == BoardPosition::H8) {
                    castleCharacter = true;
                } else if (canCastleQueenSide(Side::Black) && position == BoardPosition::A8) {
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
