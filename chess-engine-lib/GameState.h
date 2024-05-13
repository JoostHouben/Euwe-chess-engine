#include "ClangDiagnosticIgnore.h"

#pragma once

#include "MyAssert.h"
#include "StackOfVectors.h"

#include <array>
#include <bit>
#include <string>
#include <string_view>
#include <utility>

#include <cstdint>

enum class Side : std::uint8_t { White, Black };
constexpr int kNumSides = 2;

constexpr int kRanks   = 8;
constexpr int kFiles   = 8;
constexpr int kSquares = kRanks * kFiles;

constexpr int kNumPawns         = 8;
constexpr int kNumNonPawns      = 8;
constexpr int kNumPiecesPerSide = kNumPawns + kNumNonPawns;
constexpr int kNumTotalPieces   = kNumPiecesPerSide * kNumSides;

constexpr int kNumPieceTypes = 6;

[[nodiscard]] constexpr Side nextSide(Side side) {
    switch (side) {
        case Side::White:
            return Side::Black;
        case Side::Black:
            return Side::White;
    }
    UNREACHABLE;
}

enum class Piece : std::uint8_t { Pawn, Knight, Bishop, Rook, Queen, King, Invalid = 7 };

[[nodiscard]] constexpr std::string pieceToString(Piece piece) {
    switch (piece) {
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
        case Piece::Invalid:
            return "!";
    }
    UNREACHABLE;
}

[[nodiscard]] constexpr bool isSlidingPiece(Piece piece) {
    return piece == Piece::Bishop || piece == Piece::Rook || piece == Piece::Queen;
}

[[nodiscard]] constexpr bool isPinningPiece(Piece piece) {
    return piece == Piece::Bishop || piece == Piece::Rook || piece == Piece::Queen;
}

enum class ColoredPiece : std::uint8_t {
    WhitePawn   = (std::uint8_t)Piece::Pawn,
    WhiteKnight = (std::uint8_t)Piece::Knight,
    WhiteBishop = (std::uint8_t)Piece::Bishop,
    WhiteRook   = (std::uint8_t)Piece::Rook,
    WhiteQueen  = (std::uint8_t)Piece::Queen,
    WhiteKing   = (std::uint8_t)Piece::King,

    BlackPawn   = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Pawn,
    BlackKnight = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Knight,
    BlackBishop = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Bishop,
    BlackRook   = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Rook,
    BlackQueen  = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::Queen,
    BlackKing   = ((std::uint8_t)Side::Black << 3) + (std::uint8_t)Piece::King,

    Invalid = (std::uint8_t)Piece::Invalid,
};

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
    return getPromotionPiece(flags) != Piece::Pawn;
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

struct Move {
    Piece pieceToMove  = Piece::Invalid;
    BoardPosition from = BoardPosition::Invalid;
    BoardPosition to   = BoardPosition::Invalid;
    MoveFlags flags    = MoveFlags::None;

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

[[nodiscard]] constexpr BoardPosition getFirstSetPosition(BitBoard bitBoard) {
    return (BoardPosition)std::countr_zero((std::uint64_t)bitBoard);
}

[[nodiscard]] constexpr BoardPosition popFirstSetPosition(BitBoard& bitBoard) {
    const BoardPosition position = getFirstSetPosition(bitBoard);
    bitBoard = (BitBoard)((std::uint64_t)bitBoard & ((std::uint64_t)bitBoard - 1ull));
    return position;
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
        WhiteKingSide  = KingSide,
        WhiteQueenSide = QueenSide,
        BlackKingSide  = KingSide << 2,
        BlackQueenSide = QueenSide << 2,
    };

    struct UnmakeMoveInfo {
        BoardPosition enPassantTarget      = BoardPosition::Invalid;
        CastlingRights castlingRights      = CastlingRights::None;
        std::uint8_t plySinceCaptureOrPawn = 0;
        Piece capturedPiece                = Piece::Invalid;
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

    [[nodiscard]] BitBoard getPieceBitBoard(Side side, Piece piece) const {
        return pieceBitBoards_[(int)side][(int)piece];
    }
    [[nodiscard]] BitBoard getPieceBitBoard(ColoredPiece coloredPiece) const {
        return getPieceBitBoard(getSide(coloredPiece), getPiece(coloredPiece));
    }

    [[nodiscard]] ColoredPiece getPieceOnSquare(BoardPosition position) const {
        return pieceOnSquare_[(int)position];
    }

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
    struct PieceIdentifier {
        Piece piece;
        BoardPosition position;
    };

    struct CheckInformation {
        BitBoard checkingPieceControl         = BitBoard::Empty;
        PieceIdentifier checkingPieceId       = {Piece::Invalid, BoardPosition::Invalid};
        PieceIdentifier secondCheckingPieceId = {Piece::Invalid, BoardPosition::Invalid};
    };

    [[nodiscard]] BitBoard& getPieceBitBoard(Side side, Piece piece) {
        return pieceBitBoards_[(int)side][(int)piece];
    }
    [[nodiscard]] BitBoard& getPieceBitBoard(ColoredPiece coloredPiece) {
        return getPieceBitBoard(getSide(coloredPiece), getPiece(coloredPiece));
    }

    [[nodiscard]] ColoredPiece& getPieceOnSquare(BoardPosition position) {
        return pieceOnSquare_[(int)position];
    }

    [[nodiscard]] StackVector<Move> generateMovesInCheck(
            StackOfVectors<Move>& stack, BitBoard enemyControl) const;

    // TODO: rename this something like xray bitboards?
    // The last entry in the array stores the union of the other entries
    [[nodiscard]] std::array<BitBoard, kNumPiecesPerSide> calculatePiecePinOrKingAttackBitBoards(
            Side kingSide) const;

    [[nodiscard]] bool enPassantWillPutUsInCheck() const;

    [[nodiscard]] BitBoard getEnemyControl() const;
    bool isInCheck(BitBoard enemyControl) const;

    [[nodiscard]] CheckInformation getCheckInformation() const;

    void setCanCastleKingSide(Side side, bool canCastle);
    void setCanCastleQueenSide(Side side, bool canCastle);
    void setCanCastle(Side side, CastlingRights castlingSide, bool canCastle);

    void makeCastleMove(const Move& move, bool reverse = false);
    [[nodiscard]] Piece makeSinglePieceMove(const Move& move);
    void handlePawnMove(const Move& move);
    void handleNormalKingMove();
    void updateRookCastlingRights(BoardPosition rookPosition, Side rookSide);

    void unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo);

    GameState() = default;

    Side sideToMove_ = Side::White;

    BoardPosition enPassantTarget_ = BoardPosition::Invalid;

    CastlingRights castlingRights_ = CastlingRights::None;

    std::uint8_t plySinceCaptureOrPawn_ = 0;

    std::array<ColoredPiece, kSquares> pieceOnSquare_ = {};

    // TODO: should we store the king as a position instead of a bitboard?
    std::array<std::array<BitBoard, kNumPieceTypes>, kNumSides> pieceBitBoards_ = {};

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

    // TODO: add piece

    const std::string captureString = isEnPassant(move.flags) ? "y"
                                      : isCapture(move.flags) ? "x"
                                                              : "";
    const Piece promotionPiece      = getPromotionPiece(move.flags);
    const std::string promotionString =
            promotionPiece == Piece::Pawn ? "" : "=" + pieceToString(promotionPiece);
    return algebraicFromPosition(move.from) + captureString + algebraicFromPosition(move.to) +
           promotionString;
}
