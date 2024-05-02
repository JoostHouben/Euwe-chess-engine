#include "GameState.h"

#include <cassert>
#include <cstdlib>

#include <map>
#include <sstream>

namespace {

constexpr char kLowerCaseBit = 1 << 5;

std::map<BoardPosition, ColoredPiece> getPositionToPieceMap(
        const std::array<GameState::PieceInfo, kNumTotalPieces>& pieces) {
    std::map<BoardPosition, ColoredPiece> positionToPiece;
    for (const auto& pieceInfo : pieces) {
        if (pieceInfo.captured) {
            continue;
        }
        positionToPiece.emplace(pieceInfo.position, pieceInfo.coloredPiece);
    }
    return positionToPiece;
}

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
    std::unreachable();
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
    std::unreachable();
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
            std::unreachable();
    }
}

constexpr char toFenChar(ColoredPiece coloredPiece) {
    char c = toFenChar(getPiece(coloredPiece));
    if (getSide(coloredPiece) == Side::Black) {
        c |= kLowerCaseBit;
    }
    return c;
}

std::array<GameState::PieceInfo, kNumTotalPieces> parseBoardConfigurationFromFen(
        std::string::const_iterator& strIt) {
    std::array<GameState::PieceInfo, kNumTotalPieces> pieces;

    int whitePawnIdx = (int)PieceIndex::WhitePawn0;
    int whiteKnightIdx = (int)PieceIndex::WhiteKnight0;
    int whiteBishopIdx = (int)PieceIndex::WhiteBishop0;
    int whiteRookIdx = (int)PieceIndex::WhiteRook0;

    int blackPawnIdx = (int)PieceIndex::BlackPawn0;
    int blackKnightIdx = (int)PieceIndex::BlackKnight0;
    int blackBishopIdx = (int)PieceIndex::BlackBishop0;
    int blackRookIdx = (int)PieceIndex::BlackRook0;

    for (int rank = 7; rank >= 0; --rank) {
        for (int file = 0; file < 8; ++strIt) {
            if (isNumber(*strIt)) {
                file += (*strIt - '0');
                continue;
            }
            ColoredPiece piece = coloredPieceFromFenChar(*strIt);
            BoardPosition position = positionFromFileRank(file, rank);

            int index;
            if (getSide(piece) == Side::White) {
                switch (getPiece(piece)) {
                    case Piece::Pawn:
                        index = whitePawnIdx++;
                        break;
                    case Piece::Knight:
                        index = whiteKnightIdx++;
                        break;
                    case Piece::Bishop:
                        index = whiteBishopIdx++;
                        break;
                    case Piece::Rook:
                        index = whiteRookIdx++;
                        break;
                    case Piece::Queen:
                        index = (int)PieceIndex::WhiteQueen;
                        break;
                    case Piece::King:
                        index = (int)PieceIndex::WhiteKing;
                        break;
                    default:
                        std::unreachable();
                }
            } else {
                switch (getPiece(piece)) {
                    case Piece::Pawn:
                        index = blackPawnIdx++;
                        break;
                    case Piece::Knight:
                        index = blackKnightIdx++;
                        break;
                    case Piece::Bishop:
                        index = blackBishopIdx++;
                        break;
                    case Piece::Rook:
                        index = blackRookIdx++;
                        break;
                    case Piece::Queen:
                        index = (int)PieceIndex::BlackQueen;
                        break;
                    case Piece::King:
                        index = (int)PieceIndex::BlackKing;
                        break;
                    default:
                        std::unreachable();
                }
            }

            pieces[index].coloredPiece = piece;
            pieces[index].position = position;
            pieces[index].captured = false;

            file += 1;
        }
        assert((rank > 0 && *strIt == '/') || (rank == 0 && *strIt == ' '));
        if (rank > 0) {
            ++strIt;
        }
    }

    return pieces;
}

Side parseSideToMoveFromFen(std::string::const_iterator& strIt) {
    switch (*(strIt++)) {
        case 'w':
            return Side::White;
        case 'b':
            return Side::Black;
    }
    std::unreachable();
}

void parseCastlingRightsFromFen(
        std::string::const_iterator& strIt, GameState::CastlingRights& castlingRights) {
    if (*strIt == '-') {
        ++strIt;
        return;
    }

    for (; *strIt != ' '; ++strIt) {
        Side side = sideFromFenChar(*strIt);
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
                std::unreachable();
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

void boardConfigurationToFen(
        const std::array<GameState::PieceInfo, kNumTotalPieces>& pieces, std::ostream& out) {
    std::map<BoardPosition, ColoredPiece> positionToPiece = getPositionToPieceMap(pieces);
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

PieceOccupationBitBoards getPieceOccupationBitBoards(
        const std::array<GameState::PieceInfo, kNumTotalPieces>& pieces, const Side ownSide) {
    PieceOccupationBitBoards occupation;
    for (const auto& pieceInfo : pieces) {
        if (pieceInfo.captured) {
            continue;
        }
        if (getSide(pieceInfo.coloredPiece) == ownSide) {
            set(occupation.ownPiece, pieceInfo.position);
        } else {
            set(occupation.enemyPiece, pieceInfo.position);
        }
    }
    return occupation;
}

}  // namespace

GameState GameState::fromFen(const std::string& fenString) {
    GameState gameState{};

    auto strIt = fenString.begin();

    gameState.pieces_ = parseBoardConfigurationFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    gameState.sideToMove_ = parseSideToMoveFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    parseCastlingRightsFromFen(strIt, gameState.castlingRights_);
    assert(*strIt == ' ');
    ++strIt;

    gameState.enPassantTarget_ = parseEnPassantTargetFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    gameState.plySinceCaptureOrPawn_ = parsePlySinceCaptureOrPawnFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    // Ignore: move counter

    assert(strIt < fenString.end());

    gameState.occupation_ = getPieceOccupationBitBoards(gameState.pieces_, gameState.sideToMove_);
    for (auto& pieceInfo : gameState.pieces_) {
        if (pieceInfo.captured) {
            continue;
        }
        gameState.recalculateControlledSquares(pieceInfo);
    }
    for (int i = 0; i < kNumTotalPieces; ++i) {
        gameState.pinOrKingAttackBitBoards_[i] = BitBoard::Empty;
    }
    gameState.recalculatePinOrKingAttackBitBoards(Side::White);
    gameState.recalculatePinOrKingAttackBitBoards(Side::Black);

    return gameState;
}

std::string GameState::toFen(int moveCounter) const {
    std::ostringstream ss;

    boardConfigurationToFen(pieces_, ss);
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
    std::map<BoardPosition, ColoredPiece> positionToPiece = getPositionToPieceMap(pieces_);

    std::string boardTopBottom = "  ---------------------------------\n";
    std::string rowSeparator = "  |-------------------------------|\n";

    std::ostringstream ss;
    ss << boardTopBottom;

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
            ss << rowSeparator;
        }
    }
    ss << boardTopBottom;
    ss << "    a   b   c   d   e   f   g   h\n";

    return ss.str();
}

std::string bitBoardToVisualString(BitBoard bitboard) {

    std::string boardTopBottom = "  ---------------------------------\n";
    std::string rowSeparator = "  |-------------------------------|\n";

    std::ostringstream ss;
    ss << boardTopBottom;

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
            ss << rowSeparator;
        }
    }
    ss << boardTopBottom;
    ss << "    a   b   c   d   e   f   g   h\n";

    return ss.str();
}
