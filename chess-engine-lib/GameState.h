#pragma once

#pragma warning(disable : 26812)

#include <array>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <cstdint>

enum class Side : std::uint8_t {
    White,
    Black,
    None
};
inline constexpr int kNumSides = 2;

enum class Piece : std::uint8_t {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King
};

enum class ColoredPiece : std::uint8_t {};

constexpr ColoredPiece getColoredPiece(Piece piece, Side side) {
    return static_cast<ColoredPiece>(((std::uint8_t)side << 3) | (std::uint8_t)piece);
}

constexpr Piece getPiece(ColoredPiece coloredPiece) {
    constexpr std::uint8_t kPieceMask = 7;
    return static_cast<Piece>((std::uint8_t)coloredPiece & kPieceMask);
}

constexpr Side getSide(ColoredPiece coloredPiece) {
    return static_cast<Side>((std::uint8_t)coloredPiece >> 3);
}

enum class BoardPosition : std::uint8_t {
    Invalid = 1 << 6
};

constexpr BoardPosition positionFromFileRank(int file, int rank) {
    return static_cast<BoardPosition>(rank * 8 + file);
}

constexpr std::pair<int, int> fileRankFromPosition(BoardPosition position) {
    return { (int)position % 8, (int)position / 8 };
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

    std::string toFen(int moveCounter) const;
    std::string toVisualString() const;

    void makeMove(Move move); // TODO

    const std::vector<PiecePosition>& getPieces() const {
        return pieces_;
    }

    Side getSideToMove() const {
        return sideToMove_;
    }

    bool canCastleKingSide(Side side) const {
        return mayCastleKingSide_[(std::size_t)side];
    }

    bool canCastleQueenSide(Side side) const {
        return mayCastleQueenSide_[(std::size_t)side];
    }

    BoardPosition getEnPassantTarget() const {
        return enPassantTarget_;
    }

    std::uint16_t getPlySinceCaptureOrPawn() const {
        return plySinceCaptureOrPawn_;
    }

private:
    GameState() = default;

    Side sideToMove_ = Side::None;

    BoardPosition enPassantTarget_ = BoardPosition::Invalid;

    std::uint16_t plySinceCaptureOrPawn_ = 0;

    std::array<bool, kNumSides> mayCastleKingSide_ = { false, false };
    std::array<bool, kNumSides> mayCastleQueenSide_ = { false, false };

    std::vector<PiecePosition> pieces_ = {};
};

Move moveFromAlgebraic(std::string_view algebraic, const GameState& gameState); // TODO

std::string algebraicFromMove(Move move, const GameState& gameState); // TODO

std::vector<Move> generateMoves(const GameState& gameState); // TODO
