#pragma once

#pragma warning(disable : 26812)

#include <cstdint>

#include <array>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

enum Side : std::uint8_t {
    White,
    Black,
    None
};
inline constexpr int kNumSides = 2;

enum Piece : std::uint8_t {
    Pawn = 0 << 2,
    Knight = 1 << 2,
    Bishop = 2 << 2,
    Rook = 3 << 2,
    Queen = 4 << 2,
    King = 5 << 2
};

using ColoredPiece = std::uint8_t;

inline constexpr ColoredPiece kSideMask = White | Black | None;
inline constexpr ColoredPiece kPieceMask = 7 << 2;

constexpr ColoredPiece getColoredPiece(Piece piece, Side side) {
    return static_cast<ColoredPiece>(piece | side);
}

constexpr Piece getPiece(ColoredPiece coloredPiece) {
    return static_cast<Piece>(coloredPiece & kPieceMask);
}

constexpr Side getSide(ColoredPiece coloredPiece) {
    return static_cast<Side>(coloredPiece & kSideMask);
}

using BoardPosition = std::uint8_t;
inline constexpr BoardPosition kInvalidPosition = 1 << 6;

constexpr BoardPosition positionFromFileRank(int file, int rank) {
    return static_cast<BoardPosition>(rank * 8 + file);
}

constexpr std::pair<int, int> fileRankFromPosition(BoardPosition position) {
    return { position % 8, position / 8 };
}

BoardPosition positionFromAlgebraic(std::string_view algebraic);

std::string algebraicFromPosition(BoardPosition position);

using PiecePosition = std::pair<ColoredPiece, BoardPosition>;

struct Move {
    BoardPosition from;
    BoardPosition to;
};

class GameState {
public:
    static GameState fromFen(const std::string& fenString);
    static GameState startingPosition();

    void makeMove(Move move); // TODO

    const std::vector<PiecePosition>& getPieces() const {
        return pieces_;
    }

    Side getSideToMove() const {
        return sideToMove_;
    }

    bool canCastleKingSide(Side side) const {
        return mayCastleKingSide_[side];
    }

    bool canCastleQueenSide(Side side) const {
        return mayCastleQueenSide_[side];
    }

    BoardPosition getEnPassantTarget() const {
        return enPassantTarget_;
    }

    std::uint8_t getPlySinceCaptureOrPawn() const {
        return plySinceCaptureOrPawn_;
    }

private:
    GameState() = default;

    std::array<bool, kNumSides> mayCastleKingSide_ = { false, false };
    std::array<bool, kNumSides> mayCastleQueenSide_ = { false, false };

    Side sideToMove_ = None;

    BoardPosition enPassantTarget_ = kInvalidPosition;

    std::uint8_t plySinceCaptureOrPawn_ = 0;

    std::vector<PiecePosition> pieces_ = {};
};

Move moveFromAlgebraic(std::string_view algebraic, const GameState& gameState); // TODO

std::string algebraicFromMove(Move move, const GameState& gameState); // TODO

std::vector<Move> generateMoves(const GameState& gameState); // TODO
