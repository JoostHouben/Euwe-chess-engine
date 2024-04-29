#pragma once

#include <array>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <cassert>
#include <cstdint>

enum class Side : std::uint8_t { White, Black, None };
inline constexpr int kNumSides = 2;

constexpr Side nextSide(Side side) {
    switch (side) {
        case Side::White:
            return Side::Black;
        case Side::Black:
            return Side::White;
        case Side::None:
            return Side::None;
    }
    std::unreachable();
}

enum class Piece : std::uint8_t {
    None,
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King
};

enum class ColoredPiece : std::uint8_t {};

constexpr ColoredPiece getColoredPiece(Piece piece, Side side) {
    return static_cast<ColoredPiece>(((std::uint8_t)side << 3) |
                                     (std::uint8_t)piece);
}

constexpr Piece getPiece(ColoredPiece coloredPiece) {
    constexpr std::uint8_t kPieceMask = 7;
    return static_cast<Piece>((std::uint8_t)coloredPiece & kPieceMask);
}

constexpr Side getSide(ColoredPiece coloredPiece) {
    return static_cast<Side>((std::uint8_t)coloredPiece >> 3);
}

enum class BoardPosition : std::uint8_t { Invalid = 1 << 6 };

constexpr BoardPosition positionFromFileRank(int file, int rank) {
    return static_cast<BoardPosition>(rank * 8 + file);
}

constexpr std::pair<int, int> fileRankFromPosition(BoardPosition position) {
    return {(int)position % 8, (int)position / 8};
}

constexpr BoardPosition positionFromAlgebraic(std::string_view algebraic) {
    const int file = algebraic[0] - 'a';
    const int rank = algebraic[1] - '1';
    return positionFromFileRank(file, rank);
}

constexpr std::string algebraicFromPosition(BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);
    return {(char)('a' + file), (char)('1' + rank)};
}

using PiecePosition = std::pair<ColoredPiece, BoardPosition>;

std::map<BoardPosition, ColoredPiece> getPositionToPieceMap(
        const std::vector<PiecePosition>& pieces);

enum class MoveFlags : std::uint8_t {
    None = 0,
    // Lowest 3 bits: Piece if promoting
    IsCapture = 1 << 3,
    IsEnPassant = 1 << 4,
    IsCastle = 1 << 5,
};

constexpr Piece getPromotionPiece(MoveFlags flags) {
    return static_cast<Piece>((int)flags & 7);
}

constexpr bool isPromotion(MoveFlags flags) {
    return getPromotionPiece(flags) != Piece::None;
}

constexpr bool isCapture(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsCapture;
}

constexpr bool isEnPassant(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsEnPassant;
}

constexpr bool isCastle(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsCastle;
}

template <typename... FlagTs>
constexpr MoveFlags getFlags(FlagTs... flags) {
    static_assert(((std::is_same_v<FlagTs, MoveFlags> ||
                    std::is_same_v<FlagTs, Piece>)&&...));
    return static_cast<MoveFlags>((static_cast<int>(flags) | ...));
}

struct Move {
    BoardPosition from;
    BoardPosition to;
    MoveFlags flags = MoveFlags::None;
};

inline const std::string kStartingPositionFen =
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

class GameState {
   public:
    static GameState fromFen(const std::string& fenString);
    static GameState startingPosition();

    std::string toFen(int moveCounter) const;
    std::string toVisualString() const;

    bool isInCheck() const;

    std::vector<Move> generateMoves() const;

    void makeMove(Move move);
    void handleCastle(Move move);
    void handleSinglePieceMove(Move move);
    void handlePawnMove(Move move, ColoredPiece& pieceToMove);
    void handleNormalKingMove();
    void updateRookCastlingRights(BoardPosition rookPosition, Side rookSide);

    const std::vector<PiecePosition>& getPieces() const { return pieces_; }

    Side getSideToMove() const { return sideToMove_; }

    bool canCastleKingSide(Side side) const {
        return mayCastleKingSide_[(std::size_t)side];
    }

    bool canCastleQueenSide(Side side) const {
        return mayCastleQueenSide_[(std::size_t)side];
    }

    BoardPosition getEnPassantTarget() const { return enPassantTarget_; }

    std::uint16_t getPlySinceCaptureOrPawn() const {
        return plySinceCaptureOrPawn_;
    }

   private:
    std::set<BoardPosition> generateEnemyControlledSquares(
            const std::map<BoardPosition, ColoredPiece>& positionToPiece) const;
    bool isInCheck(
            const std::set<BoardPosition>& enemeyControlledSquares) const;

    GameState() = default;

    Side sideToMove_ = Side::None;

    BoardPosition enPassantTarget_ = BoardPosition::Invalid;

    std::uint16_t plySinceCaptureOrPawn_ = 0;

    std::array<bool, kNumSides> mayCastleKingSide_ = {false, false};
    std::array<bool, kNumSides> mayCastleQueenSide_ = {false, false};

    std::vector<PiecePosition> pieces_ = {};
};

Move moveFromAlgebraic(std::string_view algebraic,
                       const GameState& gameState);  // TODO

std::string algebraicFromMove(Move move, const GameState& gameState);  // TODO
