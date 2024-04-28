#include "GameState.h"

#include <cassert>
#include <cstdlib>

#include <map>
#include <sstream>

namespace {
    bool isNumber(char c) {
        return c >= '0' && c <= '9';
    }

    constexpr char kLowerCaseBit = 1 << 5;

    Piece pieceFromFenChar(char c) {
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
        default:
            assert(false);
            return Piece{};
        }
    }

    Side sideFromFenChar(char c) {
        bool isUpperCase = !(c & kLowerCaseBit);
        if (isUpperCase) {
            return Side::White;
        }
        else {
            return Side::Black;
        }
    }

    char toFenChar(Side side) {
        switch (side) {
        case Side::White:
            return 'w';
        case Side::Black:
            return 'b';
        case Side::None:
            return 'x';
        default:
            assert(false);
            return '\0';
        }
    }

    ColoredPiece coloredPieceFromFenChar(char c) {
        return getColoredPiece(pieceFromFenChar(c), sideFromFenChar(c));
    }

    char toFenChar(Piece piece) {
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
            assert(false);
            return '\0';
        }

    }

    char toFenChar(ColoredPiece coloredPiece) {
        char c = toFenChar(getPiece(coloredPiece));
        if (getSide(coloredPiece) == Side::Black) {
            c |= kLowerCaseBit;
        }
        return c;
    }

    std::vector<PiecePosition> parseBoardConfigurationFromFen(
        std::string::const_iterator& strIt
    ) {
        std::vector<PiecePosition> pieces;

        for (int rank = 7; rank >= 0; --rank) {
            for (int file = 0; file < 8; ++strIt) {
                if (isNumber(*strIt)) {
                    file += (*strIt - '0');
                    continue;
                }
                ColoredPiece piece = coloredPieceFromFenChar(*strIt);
                BoardPosition position = positionFromFileRank(file, rank);
                pieces.emplace_back(piece, position);
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
        default:
            assert(false);
            return Side::None;
        }
    }

    void parseCastlingRightsFromFen(
        std::string::const_iterator& strIt,
        std::array<bool, kNumSides>& mayCastleKingSide,
        std::array<bool, kNumSides>& mayCastleQueenSide
    ) {
        if (*strIt == '-') {
            ++strIt;
            return;
        }

        for (; *strIt != ' '; ++strIt) {
            Side side = sideFromFenChar(*strIt);
            Piece piece = pieceFromFenChar(*strIt);
            switch (piece) {
            case Piece::King:
                mayCastleKingSide[(std::size_t)side] = true;
                break;
            case Piece::Queen:
                mayCastleQueenSide[(std::size_t)side] = true;
                break;
            default:
                assert(false);
            }
        }
    }

    BoardPosition parseEnPassantTargetFromFen(std::string::const_iterator& strIt) {
        if (*strIt == '-') {
            ++strIt;
            return BoardPosition::Invalid;
        }

        BoardPosition enPassantTarget = positionFromAlgebraic({ &*strIt, 2 });
        strIt += 2;
        return enPassantTarget;
    }

    std::uint16_t parsePlySinceCaptureOrPawnFromFen(std::string::const_iterator& strIt) {
        int plySinceCaptureOrPawn = std::atoi(&*strIt);
        do {
            ++strIt;
        } while (*strIt != ' ');
        return static_cast<std::uint16_t>(plySinceCaptureOrPawn);
    }

    std::map<BoardPosition, ColoredPiece> getPositionToPieceMap(const std::vector<PiecePosition>& pieces) {
        std::map<BoardPosition, ColoredPiece> positionToPiece;
        for (const auto [piece, position] : pieces) {
            positionToPiece.emplace(position, piece);
        }
        return positionToPiece;
    }

    void boardConfigurationToFen(const std::vector<PiecePosition>& pieces, std::ostream& out) {
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
        for (auto side : { Side::White, Side::Black }) {
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
        }
        else {
            out << algebraicFromPosition(enPassantTarget);
        }
    }
}

BoardPosition positionFromAlgebraic(std::string_view algebraic) {
    int file = algebraic[0] - 'a';
    int rank = algebraic[1] - '1';
    return positionFromFileRank(file, rank);
}

std::string algebraicFromPosition(BoardPosition position) {
    auto [file, rank] = fileRankFromPosition(position);
    std::string algebraic(2, '\0');
    algebraic[0] = 'a' + file;
    algebraic[1] = '1' + rank;
    return algebraic;
}

GameState GameState::fromFen(const std::string& fenString) {
    GameState gameState{};

    auto strIt = fenString.begin();

    gameState.pieces_ = parseBoardConfigurationFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    gameState.sideToMove_ = parseSideToMoveFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    parseCastlingRightsFromFen(strIt, gameState.mayCastleKingSide_, gameState.mayCastleQueenSide_);
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

    return gameState;
}

GameState GameState::startingPosition() {
    std::string startingPositionFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    return fromFen(startingPositionFen);
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
    ss << plySinceCaptureOrPawn_;
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
            }
            else {
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
