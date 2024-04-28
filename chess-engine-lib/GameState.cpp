#include "GameState.h"

#include <cassert>
#include <cstdlib>

namespace {
    bool isNumber(char c) {
        return c >= '0' && c <= '9';
    }

    constexpr char kCaseBit = 1 << 5;

    Piece pieceFromFenChar(char c) {
        char upperCase = c & ~kCaseBit;
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
        bool isUpperCase = !(c & kCaseBit);
        if (isUpperCase) {
            return Side::White;
        }
        else {
            return Side::Black;
        }
    }

    ColoredPiece coloredPieceFromFenChar(char c) {
        return getColoredPiece(pieceFromFenChar(c), sideFromFenChar(c));
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
