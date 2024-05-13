#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BitBoard.h"
#include "BoardConstants.h"
#include "BoardPosition.h"
#include "Move.h"
#include "MyAssert.h"
#include "Piece.h"
#include "Side.h"
#include "StackOfVectors.h"

#include <array>
#include <string>

#include <cstdint>

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

    struct PieceOccupancyBitBoards {
        BitBoard ownPiece   = BitBoard::Empty;
        BitBoard enemyPiece = BitBoard::Empty;
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
