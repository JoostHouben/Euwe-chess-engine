#pragma once

#include "BitBoard.h"
#include "BoardConstants.h"
#include "BoardHash.h"
#include "BoardPosition.h"
#include "Move.h"
#include "MyAssert.h"
#include "Piece.h"
#include "Side.h"
#include "StackOfVectors.h"

#include <array>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <cstdint>

[[nodiscard]] inline std::string getStartingPositionFen() {
    return "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
}

using PieceBitBoards = std::array<std::array<BitBoard, kNumPieceTypes>, kNumSides>;

struct BoardControl {
    std::array<BitBoard, kNumSides> sideControl{};
    PieceBitBoards pieceTypeControl{};

    // See: https://chess.stackexchange.com/questions/5343/maximum-number-of-non-pawn-pieces-on-the-board
    // 26 non-pawns, non-kings, so 28 total including kings.
    static constexpr int kMaxNumNonPawns = 28;
    std::array<BitBoard, kMaxNumNonPawns> pieceControl{};

    int blackControlStartIdx = 0;

    const BitBoard& getEnemyControl(const Side sideToMove) const {
        return sideControl[(int)nextSide(sideToMove)];
    }

    int getPieceControlStartIdx(const Side side) const {
        return side == Side::White ? 0 : blackControlStartIdx;
    }
};

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
        int lastReversiblePositionHashIdx  = 0;
    };

    using DirectCheckBitBoards = std::array<BitBoard, kNumPieceTypes - 1>;

    [[nodiscard]] static GameState fromFen(std::string_view fenString);
    [[nodiscard]] static GameState startingPosition();

    [[nodiscard]] std::string toFen() const;
    [[nodiscard]] std::string toFenNoMoveCounters() const;
    [[nodiscard]] std::string toVisualString() const;

    [[nodiscard]] BoardControl getBoardControl() const;

    [[nodiscard]] bool isInCheck() const;
    [[nodiscard]] bool isInCheck(const BoardControl& boardControl) const;
    [[nodiscard]] bool isRepetition(int repetitionThreshold = 3) const;
    [[nodiscard]] bool isFiftyMoves() const;

    [[nodiscard]] bool givesCheck(
            const Move& move,
            const std::array<BitBoard, kNumPieceTypes - 1>& directCheckBitBoards,
            const std::optional<BitBoard>& enemyPinBitBoard) const;

    [[nodiscard]] StackVector<Move> generateMoves(
            StackOfVectors<Move>& stack, bool capturesOnly = false) const;
    [[nodiscard]] StackVector<Move> generateMoves(
            StackOfVectors<Move>& stack,
            const BoardControl& boardControl,
            bool capturesOnly = false) const;
    [[nodiscard]] StackVector<Move> generateMovesInCheck(
            StackOfVectors<Move>& stack,
            const BoardControl& boardControl,
            bool capturesOnly = false) const;

    UnmakeMoveInfo makeMove(const Move& move);
    UnmakeMoveInfo makeNullMove();
    void unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo);
    void unmakeNullMove(const UnmakeMoveInfo& unmakeMoveInfo);

    // Removes a piece from the board.
    // NOTE: this function should only be used for heuristic purposes; after calling this function,
    // this object should no longer be used in the normal course of play. This function removes some
    // internal history (like previous hashes or en passant target).
    void removePiece(BoardPosition position);

    [[nodiscard]] BitBoard getPieceBitBoard(Side side, Piece piece) const {
        return pieceBitBoards_[(int)side][(int)piece];
    }
    [[nodiscard]] BitBoard getPieceBitBoard(ColoredPiece coloredPiece) const {
        return getPieceBitBoard(getSide(coloredPiece), getPiece(coloredPiece));
    }

    [[nodiscard]] const PieceBitBoards& getPieceBitBoards() const { return pieceBitBoards_; }

    [[nodiscard]] ColoredPiece getPieceOnSquare(BoardPosition position) const {
        return pieceOnSquare_[(int)position];
    }

    [[nodiscard]] ColoredPiece getPieceOnSquareConst(BoardPosition position) const {
        return pieceOnSquare_[(int)position];
    }

    [[nodiscard]] Side getSideToMove() const { return sideToMove_; }

    [[nodiscard]] CastlingRights getCastlingRights() const { return castlingRights_; }

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

    [[nodiscard]] std::uint8_t getPlySinceCaptureOrPawn() const { return plySinceCaptureOrPawn_; }

    [[nodiscard]] std::uint16_t getHalfMoveClock() const { return halfMoveClock_; }

    [[nodiscard]] HashT getBoardHash() const { return boardHash_; }
    [[nodiscard]] HashT getPawnKingHash() const { return pawnKingHash_; }

    [[nodiscard]] BitBoard getAnyOccupancy() const { return occupancy_[0] | occupancy_[1]; }

    [[nodiscard]] int getNumPieces() const { return popCount(getAnyOccupancy()); }

    [[nodiscard]] const BitBoard& getSideOccupancy(const Side side) const {
        return occupancy_[(int)side];
    }

    [[nodiscard]] const BitBoard& getOwnOccupancy() const { return getSideOccupancy(sideToMove_); }

    [[nodiscard]] const BitBoard& getEnemyOccupancy() const {
        return getSideOccupancy(nextSide(sideToMove_));
    }

    [[nodiscard]] const BitBoard& getPinBitBoard(Side kingSide) const;

    [[nodiscard]] const BitBoard& getPinBitBoard(Side kingSide, BoardPosition kingPosition) const;

    [[nodiscard]] const std::optional<BitBoard>& getCalculatedPinBitBoard(Side kingSide) const {
        return pinBitBoards_[(int)kingSide];
    }

    [[nodiscard]] DirectCheckBitBoards getDirectCheckBitBoards() const;

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

    [[nodiscard]] BitBoard& getPieceBitBoardMut(Side side, Piece piece) {
        return pieceBitBoards_[(int)side][(int)piece];
    }
    [[nodiscard]] BitBoard& getPieceBitBoardMut(ColoredPiece coloredPiece) {
        return getPieceBitBoardMut(getSide(coloredPiece), getPiece(coloredPiece));
    }

    [[nodiscard]] ColoredPiece& getPieceOnSquareMut(BoardPosition position) {
        return pieceOnSquare_[(int)position];
    }

    [[nodiscard]] BitBoard& getSideOccupancyMut(const Side side) { return occupancy_[(int)side]; }

    [[nodiscard]] BitBoard& getOwnOccupancyMut() { return getSideOccupancyMut(sideToMove_); }

    [[nodiscard]] BitBoard& getEnemyOccupancyMut() {
        return getSideOccupancyMut(nextSide(sideToMove_));
    }

    [[nodiscard]] bool enPassantWillPutUsInCheck() const;

    [[nodiscard]] CheckInformation getCheckInformation() const;

    void setCanCastleKingSide(Side side, bool canCastle);
    void setCanCastleQueenSide(Side side, bool canCastle);
    void setCanCastle(Side side, CastlingRights castlingSide, bool canCastle);

    void makeCastleMove(const Move& move, bool reverse = false);
    [[nodiscard]] Piece makeSinglePieceMove(const Move& move);
    void handlePawnMove(const Move& move);
    void handleNormalKingMove(const Move& move);
    void updateRookCastlingRights(BoardPosition rookPosition, Side rookSide);

    void unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo);

    GameState() = default;

    Side sideToMove_ = Side::White;

    BoardPosition enPassantTarget_ = BoardPosition::Invalid;

    CastlingRights castlingRights_ = CastlingRights::None;

    std::uint8_t plySinceCaptureOrPawn_ = 0;

    std::uint16_t halfMoveClock_ = 0;

    std::array<ColoredPiece, kSquares> pieceOnSquare_ = {};

    PieceBitBoards pieceBitBoards_ = {};

    std::array<BitBoard, kNumSides> occupancy_ = {};

    HashT boardHash_    = 0;
    HashT pawnKingHash_ = 0;

    std::vector<HashT> previousHashes_ = {};

    // Index of the hash of the first position after the last irreversible move (in
    // previousHashes_).
    int lastReversiblePositionHashIdx_ = 0;

    mutable std::array<std::optional<BitBoard>, kNumSides> pinBitBoards_{};
};
