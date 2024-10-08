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
#include <string>
#include <string_view>
#include <vector>

#include <cstdint>

[[nodiscard]] inline std::string getStartingPositionFen() {
    return "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
}

using PieceBitBoards = std::array<std::array<BitBoard, kNumPieceTypes>, kNumSides>;

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

    struct PieceOccupancyBitBoards {
        BitBoard ownPiece   = BitBoard::Empty;
        BitBoard enemyPiece = BitBoard::Empty;
    };

    [[nodiscard]] static GameState fromFen(std::string_view fenString);
    [[nodiscard]] static GameState startingPosition();

    [[nodiscard]] std::string toFen() const;
    [[nodiscard]] std::string toFenNoMoveCounters() const;
    [[nodiscard]] std::string toVisualString() const;

    [[nodiscard]] BitBoard getEnemyControl() const;

    [[nodiscard]] bool isInCheck() const;
    [[nodiscard]] bool isInCheck(BitBoard enemyControl) const;
    [[nodiscard]] bool isRepetition(int repetitionThreshold = 3) const;
    [[nodiscard]] bool isFiftyMoves() const;

    [[nodiscard]] bool givesCheck(const Move& move) const;

    [[nodiscard]] StackVector<Move> generateMoves(
            StackOfVectors<Move>& stack, bool capturesOnly = false) const;
    [[nodiscard]] StackVector<Move> generateMoves(
            StackOfVectors<Move>& stack, BitBoard enemyControl, bool capturesOnly = false) const;
    [[nodiscard]] StackVector<Move> generateMovesInCheck(
            StackOfVectors<Move>& stack, BitBoard enemyControl, bool capturesOnly = false) const;

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

    [[nodiscard]] const PieceBitBoards& getPieceBitBoards() const { return pieceBitBoards_; }

    [[nodiscard]] ColoredPiece getPieceOnSquare(BoardPosition position) const {
        return pieceOnSquare_[(int)position];
    }

    [[nodiscard]] ColoredPiece getPieceOnSquareConst(BoardPosition position) const {
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

    [[nodiscard]] std::uint8_t getPlySinceCaptureOrPawn() const { return plySinceCaptureOrPawn_; }

    [[nodiscard]] std::uint16_t getHalfMoveClock() const { return halfMoveClock_; }

    [[nodiscard]] HashT getBoardHash() const { return boardHash_; }

    [[nodiscard]] const PieceOccupancyBitBoards& getOccupancy() const { return occupancy_; }

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

    // TODO: rename this something like xray bitboards?
    // The last entry in the array stores the union of the other entries
    [[nodiscard]] std::array<BitBoard, kNumPiecesPerSide> calculatePiecePinOrKingAttackBitBoards(
            Side kingSide) const;

    [[nodiscard]] bool enPassantWillPutUsInCheck() const;

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

    std::uint16_t halfMoveClock_ = 0;

    std::array<ColoredPiece, kSquares> pieceOnSquare_ = {};

    PieceBitBoards pieceBitBoards_ = {};

    PieceOccupancyBitBoards occupancy_ = {};

    HashT boardHash_ = 0;

    std::vector<HashT> previousHashes_ = {};

    // Index of the hash of the first position after the last irreversible move (in
    // previousHashes_).
    int lastReversiblePositionHashIdx_ = 0;
};
