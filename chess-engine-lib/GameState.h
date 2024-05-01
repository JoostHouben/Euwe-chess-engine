#pragma once

#include <array>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

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

enum class Piece : std::uint8_t { None, Pawn, Knight, Bishop, Rook, Queen, King };

enum class ColoredPiece : std::uint8_t { None };

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
    static_assert(((std::is_same_v<FlagTs, MoveFlags> || std::is_same_v<FlagTs, Piece>)&&...));
    return static_cast<MoveFlags>((static_cast<int>(flags) | ...));
}

struct Move {
    BoardPosition from;
    BoardPosition to;
    MoveFlags flags = MoveFlags::None;
};

enum class BitBoard : std::uint64_t { Empty };

constexpr bool isSet(BitBoard bitboard, BoardPosition position) {
    return (std::uint64_t)bitboard & (1ULL << (int)position);
}

constexpr void set(BitBoard& bitboard, BoardPosition position) {
    bitboard = (BitBoard)((std::uint64_t)bitboard | 1ULL << (int)position);
}

constexpr void clear(BitBoard& bitboard, BoardPosition position) {
    bitboard = (BitBoard)((std::uint64_t)bitboard & ~(1ULL << (int)position));
}

template <typename... BitBoardTs>
constexpr BitBoard any(BitBoardTs... bitboards) {
    static_assert((std::is_same_v<BitBoardTs, BitBoard> && ...));
    return (BitBoard)((std::uint64_t)bitboards | ...);
}

template <typename... BitBoardTs>
constexpr BitBoard intersection(BitBoardTs... bitboards) {
    static_assert((std::is_same_v<BitBoardTs, BitBoard> && ...));
    return (BitBoard)((std::uint64_t)bitboards & ...);
}

struct PieceOccupationBitBoards {
    BitBoard ownPiece = BitBoard::Empty;
    BitBoard enemyPiece = BitBoard::Empty;
};

inline std::string getStartingPositionFen() {
    return "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
}

class GameState {
   public:
    enum class CastlingRights : uint8_t {
        None = 0,
        KingSide = 1 << 0,
        QueenSide = 1 << 1,
        WhiteKingSide = 1 << 0,
        WhiteQueenSide = 1 << 1,
        BlackKingSide = 1 << 2,
        BlackQueenSide = 1 << 3,
    };

    struct PieceInfo {
        ColoredPiece coloredPiece = ColoredPiece::None;
        BoardPosition position = BoardPosition::Invalid;
        BitBoard controlledSquares = BitBoard::Empty;
    };

    struct UnmakeMoveInfo {
        BoardPosition enPassantTarget = BoardPosition::Invalid;
        CastlingRights castlingRights = CastlingRights::None;
        std::uint8_t plySinceCaptureOrPawn = 0;
        PieceInfo capturedPiece = {};
    };

    static GameState fromFen(const std::string& fenString);
    static GameState startingPosition();

    std::string toFen(int moveCounter) const;
    std::string toVisualString() const;

    bool isInCheck() const;

    std::vector<Move> generateMoves();

    UnmakeMoveInfo makeMove(const Move& move);
    UnmakeMoveInfo makeNullMove();
    void unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo);
    void unmakeNullMove(const UnmakeMoveInfo& unmakeMoveInfo);

    const std::vector<PieceInfo>& getPieces() const { return pieces_; }

    Side getSideToMove() const { return sideToMove_; }

    bool canCastleKingSide(Side side) const { return canCastle(side, CastlingRights::KingSide); }

    bool canCastleQueenSide(Side side) const { return canCastle(side, CastlingRights::QueenSide); }

    bool canCastle(Side side, CastlingRights castlingSide) const {
        const int bit = (int)castlingSide << ((int)side * 2);
        return (int)castlingRights_ & bit;
    }

    BoardPosition getEnPassantTarget() const { return enPassantTarget_; }

    std::uint16_t getPlySinceCaptureOrPawn() const { return plySinceCaptureOrPawn_; }

   private:
    void recalculateControlledSquaresForAffectedSquares(
            const std::array<BoardPosition, 4>& affectedSquares, int numAffectedSquares);
    void recalculateControlledSquares(PieceInfo& pieceInfo) const;
    BitBoard generateEnemyControlledSquares() const;
    bool isInCheck(BitBoard enemyControlledSquares) const;

    void setCanCastleKingSide(Side side, bool canCastle);
    void setCanCastleQueenSide(Side side, bool canCastle);
    void setCanCastle(Side side, CastlingRights castlingSide, bool canCastle);

    void makeCastleMove(const Move& move, bool reverse = false);
    PieceInfo makeSinglePieceMove(const Move& move);
    void handlePawnMove(const Move& move, ColoredPiece& pieceToMove);
    void handleNormalKingMove();
    void updateRookCastlingRights(BoardPosition rookPosition, Side rookSide);

    void unmakeSinglePieceMove(const Move& move);

    GameState() = default;

    Side sideToMove_ = Side::None;

    BoardPosition enPassantTarget_ = BoardPosition::Invalid;

    CastlingRights castlingRights_ = CastlingRights::None;

    std::uint8_t plySinceCaptureOrPawn_ = 0;

    std::vector<PieceInfo> pieces_ = {};

    PieceOccupationBitBoards occupation_ = {};
};

Move moveFromAlgebraic(std::string_view algebraic,
                       const GameState& gameState);  // TODO

std::string algebraicFromMove(Move move, const GameState& gameState);  // TODO
