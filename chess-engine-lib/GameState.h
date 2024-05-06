#include "ClangDiagnosticIgnore.h"

#pragma once

#include "MyAssert.h"
#include "StackOfVectors.h"

#include <array>
#include <string>
#include <string_view>
#include <utility>

#include <cstdint>

enum class Side : std::uint8_t { White, Black, None };
inline constexpr int kNumSides = 2;

inline constexpr int kRanks   = 8;
inline constexpr int kFiles   = 8;
inline constexpr int kSquares = kRanks * kFiles;

inline constexpr int kNumPawns         = 8;
inline constexpr int kNumNonPawns      = 8;
inline constexpr int kNumPiecesPerSide = kNumPawns + kNumNonPawns;
inline constexpr int kNumTotalPieces   = kNumPiecesPerSide * kNumSides;

[[nodiscard]] constexpr Side nextSide(Side side) {
    switch (side) {
        case Side::White:
            return Side::Black;
        case Side::Black:
            return Side::White;
        case Side::None:
            return Side::None;
    }
    UNREACHABLE;
}

enum class Piece : std::uint8_t { None, Pawn, Knight, Bishop, Rook, Queen, King };

[[nodiscard]] constexpr std::string pieceToString(Piece piece) {
    switch (piece) {
        case Piece::None:
            return "0";
        case Piece::Pawn:
            return "p";
        case Piece::Knight:
            return "N";
        case Piece::Bishop:
            return "B";
        case Piece::Rook:
            return "R";
        case Piece::Queen:
            return "Q";
        case Piece::King:
            return "K";
    }
    UNREACHABLE;
}

[[nodiscard]] constexpr bool isSlidingPiece(Piece piece) {
    return piece == Piece::Bishop || piece == Piece::Rook || piece == Piece::Queen;
}

[[nodiscard]] constexpr bool isPinningPiece(Piece piece) {
    return piece == Piece::Bishop || piece == Piece::Rook || piece == Piece::Queen;
}

enum class ColoredPiece : std::uint8_t { None };

[[nodiscard]] constexpr ColoredPiece getColoredPiece(Piece piece, Side side) {
    return static_cast<ColoredPiece>(((std::uint8_t)side << 3) | (std::uint8_t)piece);
}

[[nodiscard]] constexpr Piece getPiece(ColoredPiece coloredPiece) {
    constexpr std::uint8_t kPieceMask = 7;
    return static_cast<Piece>((std::uint8_t)coloredPiece & kPieceMask);
}

[[nodiscard]] constexpr Side getSide(ColoredPiece coloredPiece) {
    return static_cast<Side>((std::uint8_t)coloredPiece >> 3);
}

enum class BoardPosition : std::uint8_t { Invalid = 1 << 6 };

[[nodiscard]] constexpr BoardPosition positionFromFileRank(int file, int rank) {
    return static_cast<BoardPosition>(rank * 8 + file);
}

[[nodiscard]] constexpr int fileFromPosition(BoardPosition position) {
    return (int)position % 8;
}

[[nodiscard]] constexpr int rankFromPosition(BoardPosition position) {
    return (int)position / 8;
}

[[nodiscard]] constexpr std::pair<int, int> fileRankFromPosition(BoardPosition position) {
    return {fileFromPosition(position), rankFromPosition(position)};
}

[[nodiscard]] constexpr BoardPosition positionFromAlgebraic(std::string_view algebraic) {
    const int file = algebraic[0] - 'a';
    const int rank = algebraic[1] - '1';
    return positionFromFileRank(file, rank);
}

[[nodiscard]] constexpr std::string algebraicFromPosition(BoardPosition position) {
    const auto [file, rank] = fileRankFromPosition(position);
    return {(char)('a' + file), (char)('1' + rank)};
}

enum class MoveFlags : std::uint8_t {
    None = 0,
    // Lowest 3 bits: Piece if promoting
    IsCapture   = 1 << 3,
    IsEnPassant = 1 << 4,
    IsCastle    = 1 << 5,
};

[[nodiscard]] constexpr Piece getPromotionPiece(MoveFlags flags) {
    return static_cast<Piece>((int)flags & 7);
}

[[nodiscard]] constexpr bool isPromotion(MoveFlags flags) {
    return getPromotionPiece(flags) != Piece::None;
}

[[nodiscard]] constexpr bool isCapture(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsCapture;
}

[[nodiscard]] constexpr bool isEnPassant(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsEnPassant;
}

[[nodiscard]] constexpr bool isCastle(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsCastle;
}

template <typename... FlagTs>
[[nodiscard]] constexpr MoveFlags getFlags(FlagTs... flags) {
    static_assert(((std::is_same_v<FlagTs, MoveFlags> || std::is_same_v<FlagTs, Piece>)&&...));
    return static_cast<MoveFlags>((static_cast<int>(flags) | ...));
}

enum class PieceIndex : int {
    Invalid = -1,

    WhitePieces = 0,
    WhiteKing   = WhitePieces,
    WhiteNonKing,

    BlackPieces = 16,
    BlackKing   = BlackPieces,
    BlackNonKing,

    WhitePawn = 32,
    BlackPawn
};

[[nodiscard]] constexpr PieceIndex getKingIndex(Side side) {
    return side == Side::Black ? PieceIndex::BlackKing : PieceIndex::WhiteKing;
}

struct Move {
    PieceIndex pieceToMove = PieceIndex::Invalid;
    BoardPosition from     = BoardPosition::Invalid;
    BoardPosition to       = BoardPosition::Invalid;
    MoveFlags flags        = MoveFlags::None;

    bool operator==(const Move& other) const = default;
};

enum class BitBoard : std::uint64_t {
    Empty = 0,
    Full  = ~0ULL,
};

[[nodiscard]] std::string bitBoardToVisualString(BitBoard bitboard);

[[nodiscard]] constexpr bool isSet(BitBoard bitboard, BoardPosition position) {
    return (std::uint64_t)bitboard & (1ULL << (int)position);
}

constexpr void set(BitBoard& bitboard, BoardPosition position) {
    bitboard = (BitBoard)((std::uint64_t)bitboard | 1ULL << (int)position);
}

constexpr void clear(BitBoard& bitboard, BoardPosition position) {
    bitboard = (BitBoard)((std::uint64_t)bitboard & ~(1ULL << (int)position));
}

template <typename... BitBoardTs>
[[nodiscard]] constexpr BitBoard any(BitBoardTs... bitboards) {
    static_assert((std::is_same_v<BitBoardTs, BitBoard> && ...));
    return (BitBoard)((std::uint64_t)bitboards | ...);
}

template <typename... BitBoardTs>
[[nodiscard]] constexpr BitBoard intersection(BitBoardTs... bitboards) {
    static_assert((std::is_same_v<BitBoardTs, BitBoard> && ...));
    return (BitBoard)((std::uint64_t)bitboards & ...);
}

[[nodiscard]] constexpr BitBoard subtract(BitBoard lhs, BitBoard rhs) {
    return (BitBoard)((std::uint64_t)lhs & ~(std::uint64_t)rhs);
}

struct PieceOccupancyBitBoards {
    BitBoard ownPiece   = BitBoard::Empty;
    BitBoard enemyPiece = BitBoard::Empty;
};

[[nodiscard]] inline std::string getStartingPositionFen() {
    return "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
}

class GameState {
  public:
    enum class CastlingRights : uint8_t {
        None           = 0,
        KingSide       = 1 << 0,
        QueenSide      = 1 << 1,
        WhiteKingSide  = 1 << 0,
        WhiteQueenSide = 1 << 1,
        BlackKingSide  = 1 << 2,
        BlackQueenSide = 1 << 3,
    };

    struct PieceInfo {
        ColoredPiece coloredPiece = ColoredPiece::None;
        bool captured             = true;
        BoardPosition position    = BoardPosition::Invalid;
    };

    struct UnmakeMoveInfo {
        BoardPosition enPassantTarget      = BoardPosition::Invalid;
        CastlingRights castlingRights      = CastlingRights::None;
        std::uint8_t plySinceCaptureOrPawn = 0;
        PieceIndex capturedPieceIndex      = PieceIndex::Invalid;
    };

    [[nodiscard]] static GameState fromFen(const std::string& fenString);
    [[nodiscard]] static GameState startingPosition();

    [[nodiscard]] std::string toFen(int moveCounter) const;
    [[nodiscard]] std::string toVisualString() const;

    [[nodiscard]] bool isInCheck() const;

    [[nodiscard]] StackVector<Move> generateMoves(StackOfVectors<Move>& stack) const;

    UnmakeMoveInfo makeMove(const Move& move);
    UnmakeMoveInfo makeNullMove();
    void unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo);
    void unmakeNullMove(const UnmakeMoveInfo& unmakeMoveInfo);

    [[nodiscard]] const PieceInfo& getPieceInfo(PieceIndex pieceIndex) const {
        return pieces_[(int)pieceIndex];
    }
    [[nodiscard]] const std::array<PieceInfo, kNumTotalPieces>& getPieces() const {
        return pieces_;
    }
    [[nodiscard]] BitBoard getPawnBitBoard(Side side) const { return pawnBitBoards_[(int)side]; }

    [[nodiscard]] Side getSideToMove() const { return sideToMove_; }

    [[nodiscard]] bool canCastleKingSide(Side side) const {
        return canCastle(side, CastlingRights::KingSide);
    }

    [[nodiscard]] bool canCastleQueenSide(Side side) const {
        return canCastle(side, CastlingRights::QueenSide);
    }

    [[nodiscard]] bool canCastle(Side side, CastlingRights castlingSide) const {
        const int bit = (int)castlingSide << ((int)side * 2);
        return (int)castlingRights_ & bit;
    }

    [[nodiscard]] BoardPosition getEnPassantTarget() const { return enPassantTarget_; }

    [[nodiscard]] std::uint16_t getPlySinceCaptureOrPawn() const { return plySinceCaptureOrPawn_; }

  private:
    [[nodiscard]] PieceInfo& getPieceInfo(PieceIndex pieceIndex) {
        return pieces_[(int)pieceIndex];
    }

    [[nodiscard]] StackVector<Move> generateMovesInCheck(
            StackOfVectors<Move>& stack, BitBoard enemyControlledSquares) const;

    [[nodiscard]] std::array<BitBoard, kNumPiecesPerSide - 1>
    calculatePiecePinOrKingAttackBitBoards(Side kingSide) const;
    [[nodiscard]] BitBoard calculatePinOrKingAttackBitBoard(
            const std::array<BitBoard, kNumPiecesPerSide - 1>& piecePinOrKingAttackBitBoards) const;

    [[nodiscard]] bool enPassantWillPutUsInCheck() const;

    [[nodiscard]] BitBoard getEnemyControlledSquares() const;
    bool isInCheck(BitBoard enemyControlledSquares) const;

    void setCanCastleKingSide(Side side, bool canCastle);
    void setCanCastleQueenSide(Side side, bool canCastle);
    void setCanCastle(Side side, CastlingRights castlingSide, bool canCastle);

    void makeCastleMove(const Move& move, bool reverse = false);
    [[nodiscard]] PieceIndex makeSinglePieceMove(const Move& move);
    void handlePawnMove(const Move& move);
    void handleNormalKingMove();
    void updateRookCastlingRights(BoardPosition rookPosition, Side rookSide);

    void unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo);

    GameState() = default;

    Side sideToMove_ = Side::None;

    BoardPosition enPassantTarget_ = BoardPosition::Invalid;

    CastlingRights castlingRights_ = CastlingRights::None;

    std::uint8_t plySinceCaptureOrPawn_ = 0;

    std::array<std::uint8_t, kNumSides> numNonPawns_ = {};
    // TODO: store all pieces using bitboards?
    std::array<PieceInfo, kNumTotalPieces> pieces_ = {};

    std::array<BitBoard, kNumSides> pawnBitBoards_ = {};

    PieceOccupancyBitBoards occupancy_ = {};
};

Move moveFromAlgebraic(std::string_view algebraic,
                       const GameState& gameState);  // TODO

std::string algebraicFromMove(Move move, const GameState& gameState);  // TODO

// Similar to algebraic notation, except:
//  - Uses origin square instead of piece type.
//  - Uses 'y' for en passant captures.
//  - No marker for check or checkmate.
// Examples: a1a2, a7xa8=Q, b4ya3
[[nodiscard]] constexpr std::string moveToStringSimple(const Move& move) {
    if (isCastle(move.flags)) {
        const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
        const bool isQueenSide              = kingToFile == 2;  // c
        return isQueenSide ? "O-O-O" : "O-O";
    }

    const std::string captureString = isEnPassant(move.flags) ? "y"
                                      : isCapture(move.flags) ? "x"
                                                              : "";
    const Piece promotionPiece      = getPromotionPiece(move.flags);
    const std::string promotionString =
            promotionPiece == Piece::None ? "" : "=" + pieceToString(promotionPiece);
    return algebraicFromPosition(move.from) + captureString + algebraicFromPosition(move.to) +
           promotionString;
}
