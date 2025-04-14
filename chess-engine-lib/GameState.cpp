#include "GameState.h"

#include "Macros.h"
#include "Math.h"
#include "MyAssert.h"
#include "PieceControl.h"

namespace {

[[nodiscard]] FORCE_INLINE BitBoard
getKingRayBitBoard(const BoardPosition piecePosition, const BoardPosition kingPosition) {
    const auto [fileIncrement, rankIncrement] = getFileRankIncrement(piecePosition, kingPosition);
    const BitBoard kingRayBitBoard =
            (BitBoard)getFullRay(piecePosition, fileIncrement, rankIncrement)
            | (BitBoard)getFullRay(piecePosition, -fileIncrement, -rankIncrement);

    return kingRayBitBoard;
}

void generatePawnMoves(
        const BitBoard pawnBitBoard,
        const Side side,
        const std::array<BitBoard, kNumSides>& occupancy,
        const BoardPosition enPassantTarget,
        const BitBoard pinBitBoard,
        const BoardPosition kingPosition,
        StackVector<Move>& moves,
        const bool capturesOnly,
        const BitBoard checkResolutionBitBoard = BitBoard::Full) {
    const std::uint64_t startingRankMask =
            side == Side::White ? (0xffULL << (1 * 8)) : (0xffULL << (6 * 8));
    auto forwardShift = [=](const BitBoard bitBoard) FORCE_INLINE {
        return side == Side::White ? (BitBoard)((std::uint64_t)bitBoard << 8)
                                   : (BitBoard)((std::uint64_t)bitBoard >> 8);
    };
    auto leftForwardShift = [=](const BitBoard bitBoard) FORCE_INLINE {
        return side == Side::White ? (BitBoard)(((std::uint64_t)bitBoard & kNotWestFileMask) << 7)
                                   : (BitBoard)(((std::uint64_t)bitBoard & kNotWestFileMask) >> 9);
    };
    auto rightForwardShift = [=](const BitBoard bitBoard) FORCE_INLINE {
        return side == Side::White ? (BitBoard)(((std::uint64_t)bitBoard & kNotEastFileMask) << 9)
                                   : (BitBoard)(((std::uint64_t)bitBoard & kNotEastFileMask) >> 7);
    };

    const BitBoard anyPiece = occupancy[0] | occupancy[1];
    BitBoard captureTargets = occupancy[(int)nextSide(side)];
    if (enPassantTarget != BoardPosition::Invalid) {
        captureTargets |= enPassantTarget;
    }

    BitBoard leftCaptures  = leftForwardShift(pawnBitBoard) & captureTargets;
    BitBoard rightCaptures = rightForwardShift(pawnBitBoard) & captureTargets;

    leftCaptures  = leftCaptures & checkResolutionBitBoard;
    rightCaptures = rightCaptures & checkResolutionBitBoard;

    const int forwardBits       = side == Side::White ? 8 : -8;
    const int moveRankIncrement = side == Side::White ? 1 : -1;
    constexpr int leftBits      = -1;
    constexpr int rightBits     = 1;

    const int promotionRank = side == Side::White ? 7 : 0;

    auto generateMoves = [&](BitBoard targetBitBoard,
                             const int originOffset,
                             const int moveFileIncrement,
                             MoveFlags baseFlags) FORCE_INLINE {
        while (targetBitBoard != BitBoard::Empty) {
            const BoardPosition targetPosition = popFirstSetPosition(targetBitBoard);

            const int originIdx = (int)targetPosition - originOffset;
            MY_ASSERT(originIdx >= 0 && originIdx < kSquares);

            const BoardPosition originPosition = (BoardPosition)originIdx;

            if (pinBitBoard & originPosition) [[unlikely]] {
                const auto [kingFileIncrement, kingRankIncrement] =
                        getFileRankIncrement(originPosition, kingPosition);

                const bool moveIsAlongPin = (kingFileIncrement == moveFileIncrement
                                             && kingRankIncrement == moveRankIncrement)
                                         || (kingFileIncrement == -moveFileIncrement
                                             && kingRankIncrement == -moveRankIncrement);

                if (!moveIsAlongPin) {
                    // Pin is not along the move direction, so move is impossible
                    continue;
                }
            }

            MoveFlags flags = baseFlags;
            if (targetPosition == enPassantTarget) {
                MY_ASSERT(isCapture(flags));
                flags = flags | MoveFlags::IsEnPassant;
            }

            int toRank = rankFromPosition(targetPosition);
            if (toRank == promotionRank) {
                for (const auto promotionPiece : kPromotionPieces) {
                    moves.emplace_back(
                            Piece::Pawn, originPosition, targetPosition, flags | promotionPiece);
                }
            } else {
                moves.emplace_back(Piece::Pawn, originPosition, targetPosition, flags);
            }
        }
    };

    if (!capturesOnly) {
        BitBoard singlePushes = forwardShift(pawnBitBoard) & ~anyPiece;

        const BitBoard startingPawns           = pawnBitBoard & (BitBoard)startingRankMask;
        const BitBoard startingPawnsSinglePush = forwardShift(startingPawns) & ~anyPiece;
        BitBoard doublePushes                  = forwardShift(startingPawnsSinglePush) & ~anyPiece;

        singlePushes = singlePushes & checkResolutionBitBoard;
        doublePushes = doublePushes & checkResolutionBitBoard;

        generateMoves(singlePushes, forwardBits, 0, MoveFlags::None);
        generateMoves(doublePushes, 2 * forwardBits, 0, MoveFlags::None);
    }

    generateMoves(leftCaptures, forwardBits + leftBits, -1, MoveFlags::IsCapture);
    generateMoves(rightCaptures, forwardBits + rightBits, 1, MoveFlags::IsCapture);
}

FORCE_INLINE void generateCastlingMoves(
        const Side sideToMove,
        const bool canCastleKingSide,
        const bool canCastleQueenSide,
        const BitBoard anyPiece,
        const BitBoard enemyControlledSquares,
        StackVector<Move>& moves) {
    MY_ASSERT(sideToMove == Side::White || sideToMove == Side::Black);

    const BoardPosition kingPosition =
            sideToMove == Side::White ? BoardPosition::E1 : BoardPosition::E8;

    const auto [kingFile, kingRank] = fileRankFromPosition(kingPosition);

    if (canCastleKingSide) {
        const BitBoard emptySquaresMask = (BitBoard)(0x60ULL << (kingRank * 8));

        bool castleIsValid = true;
        if ((emptySquaresMask & anyPiece) != BitBoard::Empty) {
            castleIsValid = false;
        }
        if ((emptySquaresMask & enemyControlledSquares) != BitBoard::Empty) {
            castleIsValid = false;
        }

        if (castleIsValid) {
            const BoardPosition targetPosition = positionFromFileRank(kingFile + 2, kingRank);
            moves.emplace_back(Piece::King, kingPosition, targetPosition, MoveFlags::IsCastle);
        }
    }
    if (canCastleQueenSide) {
        const BitBoard emptySquaresMask = (BitBoard)(0xeULL << (kingRank * 8));
        const BitBoard controlMask      = (BitBoard)(0x1cULL << (kingRank * 8));

        bool castleIsValid = true;
        if ((emptySquaresMask & anyPiece) != BitBoard::Empty) {
            castleIsValid = false;
        }
        if ((controlMask & enemyControlledSquares) != BitBoard::Empty) {
            castleIsValid = false;
        }

        if (castleIsValid) {
            const BoardPosition targetPosition = positionFromFileRank(kingFile - 2, kingRank);
            moves.emplace_back(Piece::King, kingPosition, targetPosition, MoveFlags::IsCastle);
        }
    }
}

// Can not be used for generating pawn non-captures
FORCE_INLINE void generateSinglePieceMovesFromControl(
        const Piece piece,
        const BoardPosition piecePosition,
        BitBoard controlledSquares,
        const BitBoard ownPiece,
        const BitBoard enemyPiece,
        StackVector<Move>& moves,
        bool capturesOnly) {
    // Can't move to our own pieces
    controlledSquares = controlledSquares & ~ownPiece;

    BitBoard captures = controlledSquares & enemyPiece;
    while (captures != BitBoard::Empty) {
        const BoardPosition capturePosition = popFirstSetPosition(captures);
        moves.emplace_back(piece, piecePosition, capturePosition, MoveFlags::IsCapture);
    }

    if (!capturesOnly) {
        BitBoard nonCaptures = controlledSquares & ~enemyPiece;
        while (nonCaptures != BitBoard::Empty) {
            const BoardPosition movePosition = popFirstSetPosition(nonCaptures);
            moves.emplace_back(piece, piecePosition, movePosition);
        }
    }
}

}  // namespace

GameState GameState::startingPosition() {
    static const GameState startingPosition = fromFen(getStartingPositionFen());
    return startingPosition;
}

bool GameState::isInCheck() const {
    return isInCheck(getBoardControl());
}

StackVector<Move> GameState::generateMoves(StackOfVectors<Move>& stack, bool capturesOnly) const {
    const BoardControl boardControl = getBoardControl();
    return generateMoves(stack, boardControl, capturesOnly);
}

StackVector<Move> GameState::generateMoves(
        StackOfVectors<Move>& stack, const BoardControl& boardControl, bool capturesOnly) const {

    if (isInCheck(boardControl)) {
        return generateMovesInCheck(stack, boardControl, capturesOnly);
    }

    StackVector<Move> moves = stack.makeStackVector();

    const BoardPosition ownKingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));

    const BitBoard pinBitBoard = getPinBitBoard(sideToMove_, ownKingPosition);

    const auto getPiecePinBitBoard = [&](BoardPosition position) {
        if (!(pinBitBoard & position)) {
            return BitBoard::Full;
        }

        return getKingRayBitBoard(position, ownKingPosition);
    };

    const bool enPassantCheck =
            enPassantTarget_ != BoardPosition::Invalid && enPassantWillPutUsInCheck();

    const BoardPosition enPassantTarget =
            enPassantCheck ? BoardPosition::Invalid : enPassantTarget_;

    // Generate moves for pawns
    generatePawnMoves(
            getPieceBitBoard(sideToMove_, Piece::Pawn),
            sideToMove_,
            occupancy_,
            enPassantTarget,
            pinBitBoard,
            ownKingPosition,
            moves,
            capturesOnly);

    const BitBoard anyOccupancy = getAnyOccupancy();

    int pieceControlIdx = boardControl.getPieceControlStartIdx(sideToMove_);

    // Generate moves for normal pieces (non-pawns excl. king)
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(sideToMove_, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);
            const BitBoard piecePinBitBoard   = getPiecePinBitBoard(piecePosition);
            const BitBoard& controlledSquares = boardControl.pieceControl[pieceControlIdx++];

            generateSinglePieceMovesFromControl(
                    piece,
                    piecePosition,
                    controlledSquares & piecePinBitBoard,
                    getOwnOccupancy(),
                    getEnemyOccupancy(),
                    moves,
                    capturesOnly);
        }
    }

    // Generate king moves
    const BitBoard& enemyControl = boardControl.getEnemyControl(sideToMove_);

    // Normal king moves
    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));
    // King can't walk into check
    const BitBoard kingControlledSquares =
            boardControl.pieceControl[pieceControlIdx++] & ~enemyControl;
    generateSinglePieceMovesFromControl(
            Piece::King,
            kingPosition,
            kingControlledSquares,
            getOwnOccupancy(),
            getEnemyOccupancy(),
            moves,
            capturesOnly);

    if (!capturesOnly) {
        // Castling moves
        generateCastlingMoves(
                sideToMove_,
                canCastleKingSide(sideToMove_),
                canCastleQueenSide(sideToMove_),
                anyOccupancy,
                enemyControl,
                moves);
    }

    moves.lock();
    return moves;
}

StackVector<Move> GameState::generateMovesInCheck(
        StackOfVectors<Move>& stack, const BoardControl& boardControl, bool capturesOnly) const {
    StackVector<Move> moves = stack.makeStackVector();

    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));

    const BitBoard anyPieceNoKing = getAnyOccupancy() & ~kingPosition;

    const CheckInformation checkInformation = getCheckInformation();

    PieceIdentifier checkingPieceId             = checkInformation.checkingPieceId;
    const PieceIdentifier secondCheckingPieceId = checkInformation.secondCheckingPieceId;

    const bool doubleCheck = secondCheckingPieceId.piece != Piece::Invalid;

    // Controlled squares of checking pieces if the king weren't there
    BitBoard kingAttackBitBoard = BitBoard::Empty;
    if (isSlidingPiece(checkingPieceId.piece)) {
        kingAttackBitBoard = getPieceControlledSquares(
                checkingPieceId.piece, checkingPieceId.position, anyPieceNoKing);
    }
    if (isSlidingPiece(secondCheckingPieceId.piece)) {
        const BitBoard secondCheckingPieceControlledSquares = getPieceControlledSquares(
                secondCheckingPieceId.piece, secondCheckingPieceId.position, anyPieceNoKing);
        kingAttackBitBoard = kingAttackBitBoard | secondCheckingPieceControlledSquares;
    }
    // Controlled squares of the first checking piece (only used when not in double check)
    const BitBoard checkingPieceControlledSquares = checkInformation.checkingPieceControl;

    BitBoard kingControlledSquares = getKingControlledSquares(kingPosition);
    // King can't walk into check
    kingControlledSquares = kingControlledSquares & ~boardControl.getEnemyControl(sideToMove_);
    kingControlledSquares = kingControlledSquares & ~kingAttackBitBoard;
    generateSinglePieceMovesFromControl(
            Piece::King,
            kingPosition,
            kingControlledSquares,
            getOwnOccupancy(),
            getEnemyOccupancy(),
            moves,
            capturesOnly);

    if (doubleCheck) {
        // Double check: only the king can move
        moves.lock();
        return moves;
    }

    BitBoard blockOrCaptureBitBoard = BitBoard::Empty;

    if (isPinningPiece(checkingPieceId.piece)) {
        const auto [kingFile, kingRank]         = fileRankFromPosition(kingPosition);
        const auto [checkingFile, checkingRank] = fileRankFromPosition(checkingPieceId.position);

        const int fileIncrement = signum(kingFile - checkingFile);
        const int rankIncrement = signum(kingRank - checkingRank);

        const BitBoard checkingRay =
                (BitBoard)getFullRay(checkingPieceId.position, fileIncrement, rankIncrement);

        blockOrCaptureBitBoard = checkingRay & checkingPieceControlledSquares;
    } else if (checkingPieceId.piece == Piece::Pawn) {
        // Pawn control was calculated 'in bulk', so we don't have the checking pawn's position.
        // We need to calculate it now.
        // We find the checking pawn by considering a pawn at the king's position and seeing which
        // enemy pawns it attacks.

        const BitBoard kingPawnBitBoard = BitBoard::Empty | kingPosition;

        const BitBoard kingPawnAttacks = getPawnControlledSquares(kingPawnBitBoard, sideToMove_);
        const BitBoard checkingPawnBitBoard =
                kingPawnAttacks & getPieceBitBoard(nextSide(sideToMove_), Piece::Pawn);

        checkingPieceId.position = getFirstSetPosition(checkingPawnBitBoard);
    }
    blockOrCaptureBitBoard |= checkingPieceId.position;

    const BitBoard pinBitBoard = getPinBitBoard(sideToMove_, kingPosition);

    bool canTakeCheckingPieceEnPassant = false;
    if (enPassantTarget_ != BoardPosition::Invalid) {
        const BoardPosition enPassantPiecePosition =
                getEnPassantPiecePosition(enPassantTarget_, sideToMove_);

        canTakeCheckingPieceEnPassant = enPassantPiecePosition == checkingPieceId.position;
        MY_ASSERT(IMPLIES(canTakeCheckingPieceEnPassant, checkingPieceId.piece == Piece::Pawn));
    }
    const BoardPosition enPassantTarget =
            canTakeCheckingPieceEnPassant ? enPassantTarget_ : BoardPosition::Invalid;
    BitBoard pawnBlockOrCaptureBitBoard = blockOrCaptureBitBoard;
    if (canTakeCheckingPieceEnPassant) {
        pawnBlockOrCaptureBitBoard |= enPassantTarget;
    }

    // Generate pawn moves that either capture the checking piece or block
    const BitBoard nonPinnedPawns = getPieceBitBoard(sideToMove_, Piece::Pawn) & ~pinBitBoard;
    generatePawnMoves(
            nonPinnedPawns,
            sideToMove_,
            occupancy_,
            enPassantTarget,
            /*pinBitBoard*/ BitBoard::Empty,
            kingPosition,
            moves,
            capturesOnly,
            pawnBlockOrCaptureBitBoard);

    int pieceControlIdx = boardControl.getPieceControlStartIdx(sideToMove_);

    // Generate moves for normal pieces (non-pawns excl. king)
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(sideToMove_, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);

            const int thisPieceControlIdx = pieceControlIdx++;

            if (pinBitBoard & piecePosition) {
                // Piece is pinned; can't capture pinning piece or remain in pin because that wouldn't
                // resolve the check, so no moves.
                continue;
            }

            const BitBoard& controlledSquares = boardControl.pieceControl[thisPieceControlIdx];

            // Treat blockOrCapture as a pin. This will cause only moves that block or capture to be generated.
            generateSinglePieceMovesFromControl(
                    piece,
                    piecePosition,
                    controlledSquares & blockOrCaptureBitBoard,
                    getOwnOccupancy(),
                    getEnemyOccupancy(),
                    moves,
                    capturesOnly);
        }
    }

    moves.lock();
    return moves;
}

GameState::UnmakeMoveInfo GameState::makeMove(const Move& move) {
    UnmakeMoveInfo unmakeInfo = {
            .enPassantTarget               = enPassantTarget_,
            .castlingRights                = castlingRights_,
            .plySinceCaptureOrPawn         = plySinceCaptureOrPawn_,
            .lastReversiblePositionHashIdx = lastReversiblePositionHashIdx_};

    if (isCastle(move)) {
        makeCastleMove(move);
    } else {
        unmakeInfo.capturedPiece = makeSinglePieceMove(move);
    }

    ++halfMoveClock_;

    const bool isIrreversible = isCapture(move) || move.pieceToMove == Piece::Pawn
                             || unmakeInfo.castlingRights != castlingRights_;

    if (isIrreversible) {
        lastReversiblePositionHashIdx_ = (int)previousHashes_.size();
    }

    previousHashes_.push_back(boardHash_);

    pinBitBoards_[0].reset();
    pinBitBoards_[1].reset();

    return unmakeInfo;
}

GameState::UnmakeMoveInfo GameState::makeNullMove() {
    const UnmakeMoveInfo unmakeInfo = {
            .enPassantTarget               = enPassantTarget_,
            .castlingRights                = castlingRights_,
            .plySinceCaptureOrPawn         = plySinceCaptureOrPawn_,
            .lastReversiblePositionHashIdx = lastReversiblePositionHashIdx_};

    if (enPassantTarget_ != BoardPosition::Invalid) {
        updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
    }

    sideToMove_      = nextSide(sideToMove_);
    enPassantTarget_ = BoardPosition::Invalid;
    ++halfMoveClock_;

    updateHashForSideToMove(boardHash_);
    updateHashForSideToMove(pawnKingHash_);

    // We consider a null move to be irreversible for tie checking purposes.
    // For discussion see: https://www.talkchess.com/forum/viewtopic.php?t=35052
    lastReversiblePositionHashIdx_ = (int)previousHashes_.size();
    plySinceCaptureOrPawn_         = 0;

    previousHashes_.push_back(boardHash_);

    pinBitBoards_[0].reset();
    pinBitBoards_[1].reset();

    return unmakeInfo;
}

void GameState::unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    --halfMoveClock_;

    castlingRights_  = unmakeMoveInfo.castlingRights;
    enPassantTarget_ = unmakeMoveInfo.enPassantTarget;

    lastReversiblePositionHashIdx_ = unmakeMoveInfo.lastReversiblePositionHashIdx;
    previousHashes_.pop_back();

    if (isCastle(move)) {
        makeCastleMove(move, /*reverse*/ true);
    } else {
        unmakeSinglePieceMove(move, unmakeMoveInfo);
    }

    boardHash_ = previousHashes_.back();

    pinBitBoards_[0].reset();
    pinBitBoards_[1].reset();
}

void GameState::unmakeNullMove(const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    enPassantTarget_       = unmakeMoveInfo.enPassantTarget;
    castlingRights_        = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    --halfMoveClock_;

    lastReversiblePositionHashIdx_ = unmakeMoveInfo.lastReversiblePositionHashIdx;
    previousHashes_.pop_back();

    boardHash_ = previousHashes_.back();
    updateHashForSideToMove(pawnKingHash_);

    pinBitBoards_[0].reset();
    pinBitBoards_[1].reset();
}

void GameState::removePiece(const BoardPosition position) {
    previousHashes_.clear();
    lastReversiblePositionHashIdx_ = 0;
    pinBitBoards_[0].reset();
    pinBitBoards_[1].reset();

    const ColoredPiece coloredPiece = getPieceOnSquare(position);
    const Piece piece               = getPiece(coloredPiece);
    const Side pieceSide            = getSide(coloredPiece);

    MY_ASSERT(piece != Piece::King);

    if (piece == Piece::Pawn) {
        // Removing a pawn may invalidate en passant. For simplicity, we always invalidate it.
        if (enPassantTarget_ != BoardPosition::Invalid) {
            updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
            enPassantTarget_ = BoardPosition::Invalid;
        }
    } else if (piece == Piece::Rook) {
        updateRookCastlingRights(position, pieceSide);
    }

    getPieceOnSquareMut(position) = ColoredPiece::Invalid;
    getPieceBitBoardMut(coloredPiece) &= ~position;
    getSideOccupancyMut(pieceSide) &= ~position;
    updateHashForPiecePosition(coloredPiece, position, boardHash_);

    if (piece == Piece::Pawn || piece == Piece::King) {
        updateHashForPiecePosition(coloredPiece, position, pawnKingHash_);
    }
}

void GameState::makeCastleMove(const Move& move, const bool reverse) {
    const auto [kingFromFile, kingFromRank] = fileRankFromPosition(move.from);

    const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
    const bool isQueenSide              = kingToFile == 2;  // c

    MY_ASSERT(IMPLIES(isQueenSide, canCastleQueenSide(sideToMove_)));
    MY_ASSERT(IMPLIES(!isQueenSide, canCastleKingSide(sideToMove_)));

    const int rookFromFile         = isQueenSide ? /*a*/ 0 : /*h*/ 7;
    BoardPosition rookFromPosition = positionFromFileRank(rookFromFile, kingFromRank);
    BoardPosition rookToPosition =
            positionFromFileRank((kingFromFile + kingToFile) / 2, kingFromRank);

    BoardPosition kingFromPosition = move.from;
    BoardPosition kingToPosition   = move.to;

    if (reverse) {
        std::swap(rookFromPosition, rookToPosition);
        std::swap(kingFromPosition, kingToPosition);
    }

    BitBoard& ownOccupancy = getOwnOccupancyMut();

    ownOccupancy &= ~kingFromPosition;
    ownOccupancy |= kingToPosition;

    ownOccupancy &= ~rookFromPosition;
    ownOccupancy |= rookToPosition;

    // Update king
    getPieceBitBoardMut(sideToMove_, Piece::King) = (BitBoard)(1ULL << (int)kingToPosition);

    getPieceOnSquareMut(kingToPosition)   = getPieceOnSquare(kingFromPosition);
    getPieceOnSquareMut(kingFromPosition) = ColoredPiece::Invalid;

    // Update rook
    BitBoard& rookBitBoard = getPieceBitBoardMut(sideToMove_, Piece::Rook);
    rookBitBoard &= ~rookFromPosition;
    rookBitBoard |= rookToPosition;

    getPieceOnSquareMut(rookToPosition)   = getPieceOnSquare(rookFromPosition);
    getPieceOnSquareMut(rookFromPosition) = ColoredPiece::Invalid;

    updateHashForPiecePosition(sideToMove_, Piece::King, kingFromPosition, pawnKingHash_);
    updateHashForPiecePosition(sideToMove_, Piece::King, kingToPosition, pawnKingHash_);
    updateHashForSideToMove(pawnKingHash_);

    if (!reverse) {
        updateHashForPiecePosition(sideToMove_, Piece::King, kingFromPosition, boardHash_);
        updateHashForPiecePosition(sideToMove_, Piece::King, kingToPosition, boardHash_);

        updateHashForPiecePosition(sideToMove_, Piece::Rook, rookFromPosition, boardHash_);
        updateHashForPiecePosition(sideToMove_, Piece::Rook, rookToPosition, boardHash_);

        if (canCastleKingSide(sideToMove_)) {
            setCanCastleKingSide(sideToMove_, false);
            updateHashForKingSideCastlingRights(sideToMove_, boardHash_);
        }
        if (canCastleQueenSide(sideToMove_)) {
            setCanCastleQueenSide(sideToMove_, false);
            updateHashForQueenSideCastlingRights(sideToMove_, boardHash_);
        }

        if (enPassantTarget_ != BoardPosition::Invalid) {
            updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
            enPassantTarget_ = BoardPosition::Invalid;
        }

        sideToMove_ = nextSide(sideToMove_);
        ++plySinceCaptureOrPawn_;

        updateHashForSideToMove(boardHash_);
    }
}

Piece GameState::makeSinglePieceMove(const Move& move) {
    Piece capturedPiece               = Piece::Invalid;
    BoardPosition captureTargetSquare = move.to;

    if (isEnPassant(move)) {
        MY_ASSERT(isCapture(move));
        MY_ASSERT(move.to == enPassantTarget_);

        const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
        const auto [toFile, toRank]     = fileRankFromPosition(move.to);
        captureTargetSquare             = positionFromFileRank(toFile, fromRank);
    }

    if (enPassantTarget_ != BoardPosition::Invalid) {
        updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
        enPassantTarget_ = BoardPosition::Invalid;
    }

    MY_ASSERT(move.from != BoardPosition::Invalid && move.to != BoardPosition::Invalid);

    BitBoard& ownOccupancy = getOwnOccupancyMut();
    ownOccupancy &= ~move.from;
    ownOccupancy |= move.to;

    if (isCapture(move)) {
        getEnemyOccupancyMut() &= ~captureTargetSquare;

        capturedPiece = getPiece(getPieceOnSquare(captureTargetSquare));
        MY_ASSERT(capturedPiece != Piece::Invalid);

        getPieceBitBoardMut(nextSide(sideToMove_), capturedPiece) &= ~captureTargetSquare;

        if (capturedPiece == Piece::Rook) {
            updateRookCastlingRights(captureTargetSquare, nextSide(sideToMove_));
        }

        updateHashForPiecePosition(
                nextSide(sideToMove_), capturedPiece, captureTargetSquare, boardHash_);

        if (capturedPiece == Piece::Pawn) {
            updateHashForPiecePosition(
                    nextSide(sideToMove_), capturedPiece, captureTargetSquare, pawnKingHash_);
        }
    }

    BitBoard& pieceBitBoard = getPieceBitBoardMut(sideToMove_, move.pieceToMove);
    pieceBitBoard &= ~move.from;
    pieceBitBoard |= move.to;

    MY_ASSERT(getPiece(getPieceOnSquare(move.from)) == move.pieceToMove);
    MY_ASSERT(getSide(getPieceOnSquare(move.from)) == sideToMove_);

    MY_ASSERT(IMPLIES(
            isCapture(move), getPieceOnSquare(captureTargetSquare) != ColoredPiece::Invalid));
    MY_ASSERT(
            IMPLIES(isCapture(move),
                    getSide(getPieceOnSquare(captureTargetSquare)) == nextSide(sideToMove_)));

    getPieceOnSquareMut(move.to)   = getPieceOnSquare(move.from);
    getPieceOnSquareMut(move.from) = ColoredPiece::Invalid;

    updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.from, boardHash_);
    updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.to, boardHash_);

    // pawnKingHash_ updated in handlePawnMove or handleNormalKingMove

    if (isEnPassant(move)) {
        getPieceOnSquareMut(captureTargetSquare) = ColoredPiece::Invalid;
    }

    if (move.pieceToMove == Piece::Pawn) {
        handlePawnMove(move);
    } else if (move.pieceToMove == Piece::King) {
        handleNormalKingMove(move);
    } else if (move.pieceToMove == Piece::Rook) {
        updateRookCastlingRights(move.from, sideToMove_);
    }

    if (isCapture(move) || move.pieceToMove == Piece::Pawn) {
        plySinceCaptureOrPawn_ = 0;
    } else {
        ++plySinceCaptureOrPawn_;
    }

    sideToMove_ = nextSide(sideToMove_);

    updateHashForSideToMove(boardHash_);
    updateHashForSideToMove(pawnKingHash_);

    return capturedPiece;
}

void GameState::unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    BitBoard& ownOccupancy = getOwnOccupancyMut();
    ownOccupancy |= move.from;
    ownOccupancy &= ~move.to;

    BitBoard& pieceBitBoard = getPieceBitBoardMut(sideToMove_, move.pieceToMove);
    pieceBitBoard &= ~move.to;
    pieceBitBoard |= move.from;

    // Can't use getPieceOnSquare(move.to) here because that fails when undoing a promotion.
    getPieceOnSquareMut(move.from) = getColoredPiece(move.pieceToMove, sideToMove_);

    const Piece promotionPiece = getPromotionPiece(move);
    if (promotionPiece != Piece::Pawn) {
        BitBoard& promotionBitBoard = getPieceBitBoardMut(sideToMove_, promotionPiece);
        promotionBitBoard &= ~move.to;

        updateHashForPiecePosition(sideToMove_, Piece::Pawn, move.from, pawnKingHash_);
    } else if (move.pieceToMove == Piece::Pawn || move.pieceToMove == Piece::King) {
        updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.to, pawnKingHash_);
        updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.from, pawnKingHash_);
    }

    if (isCapture(move)) {
        MY_ASSERT(unmakeMoveInfo.capturedPiece != Piece::Invalid);

        BoardPosition captureTarget = move.to;

        if (isEnPassant(move)) {
            const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
            const auto [toFile, toRank]     = fileRankFromPosition(move.to);
            captureTarget                   = positionFromFileRank(toFile, fromRank);
        }

        BitBoard& capturedPieceBitBoard =
                getPieceBitBoardMut(nextSide(sideToMove_), unmakeMoveInfo.capturedPiece);
        capturedPieceBitBoard |= captureTarget;
        getEnemyOccupancyMut() |= captureTarget;

        getPieceOnSquareMut(captureTarget) =
                getColoredPiece(unmakeMoveInfo.capturedPiece, nextSide(sideToMove_));
        if (isEnPassant(move)) {
            getPieceOnSquareMut(move.to) = ColoredPiece::Invalid;
        }

        if (unmakeMoveInfo.capturedPiece == Piece::Pawn) {
            updateHashForPiecePosition(
                    nextSide(sideToMove_), Piece::Pawn, captureTarget, pawnKingHash_);
        }
    } else {
        getPieceOnSquareMut(move.to) = ColoredPiece::Invalid;
    }

    updateHashForSideToMove(pawnKingHash_);
}

void GameState::handlePawnMove(const Move& move) {
    const Piece promotionPiece = getPromotionPiece(move);
    if (promotionPiece != Piece::Pawn) {
        BitBoard& pawnBitBoard           = getPieceBitBoardMut(sideToMove_, Piece::Pawn);
        BitBoard& promotionPieceBitBoard = getPieceBitBoardMut(sideToMove_, promotionPiece);

        pawnBitBoard &= ~move.to;
        promotionPieceBitBoard |= move.to;

        getPieceOnSquareMut(move.to) = getColoredPiece(promotionPiece, sideToMove_);

        updateHashForPiecePosition(sideToMove_, Piece::Pawn, move.to, boardHash_);
        updateHashForPiecePosition(sideToMove_, promotionPiece, move.to, boardHash_);

        updateHashForPiecePosition(sideToMove_, Piece::Pawn, move.from, pawnKingHash_);
    } else {
        updateHashForPiecePosition(sideToMove_, Piece::Pawn, move.from, pawnKingHash_);
        updateHashForPiecePosition(sideToMove_, Piece::Pawn, move.to, pawnKingHash_);
    }

    const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
    const auto [_, toRank]          = fileRankFromPosition(move.to);

    // Double pawn push
    if (std::abs(fromRank - toRank) == 2) {
        const std::uint64_t toMask = (std::uint64_t)1 << (int)move.to;
        const std::uint64_t neighborMask =
                (toMask & kNotWestFileMask) >> 1 | (toMask & kNotEastFileMask) << 1;

        const BitBoard& opponentPawns = getPieceBitBoard(nextSide(sideToMove_), Piece::Pawn);

        const bool pawnCanCaptureEnPassant =
                (opponentPawns & (BitBoard)neighborMask) != BitBoard::Empty;

        if (pawnCanCaptureEnPassant) {
            enPassantTarget_ = positionFromFileRank(fromFile, (fromRank + toRank) / 2);

            updateHashForEnPassantFile(fromFile, boardHash_);
        }
    }
}

void GameState::handleNormalKingMove(const Move& move) {
    if (canCastleKingSide(sideToMove_)) {
        setCanCastleKingSide(sideToMove_, false);
        updateHashForKingSideCastlingRights(sideToMove_, boardHash_);
    }

    if (canCastleQueenSide(sideToMove_)) {
        setCanCastleQueenSide(sideToMove_, false);
        updateHashForQueenSideCastlingRights(sideToMove_, boardHash_);
    }

    updateHashForPiecePosition(sideToMove_, Piece::King, move.from, pawnKingHash_);
    updateHashForPiecePosition(sideToMove_, Piece::King, move.to, pawnKingHash_);
}

void GameState::updateRookCastlingRights(BoardPosition rookPosition, Side rookSide) {
    if (rookSide == Side::White && rookPosition == BoardPosition::A1
        && canCastleQueenSide(rookSide)) {
        setCanCastleQueenSide(rookSide, false);
        updateHashForQueenSideCastlingRights(rookSide, boardHash_);
    } else if (
            rookSide == Side::White && rookPosition == BoardPosition::H1
            && canCastleKingSide(rookSide)) {
        setCanCastleKingSide(rookSide, false);
        updateHashForKingSideCastlingRights(rookSide, boardHash_);
    } else if (
            rookSide == Side::Black && rookPosition == BoardPosition::A8
            && canCastleQueenSide(rookSide)) {
        setCanCastleQueenSide(rookSide, false);
        updateHashForQueenSideCastlingRights(rookSide, boardHash_);
    } else if (
            rookSide == Side::Black && rookPosition == BoardPosition::H8
            && canCastleKingSide(rookSide)) {
        setCanCastleKingSide(rookSide, false);
        updateHashForKingSideCastlingRights(rookSide, boardHash_);
    }
}

FORCE_INLINE const BitBoard& GameState::getPinBitBoard(const Side kingSide) const {
    const BoardPosition kingPosition = getFirstSetPosition(getPieceBitBoard(kingSide, Piece::King));
    return getPinBitBoard(kingSide, kingPosition);
}

FORCE_INLINE const BitBoard& GameState::getPinBitBoard(
        const Side kingSide, const BoardPosition kingPosition) const {
    if (pinBitBoards_[(int)kingSide]) {
        return *pinBitBoards_[(int)kingSide];
    }

    const BitBoard anyPiece = getAnyOccupancy();

    BitBoard allPins = BitBoard::Empty;

    const BitBoard enemyRooksOrQueens = getPieceBitBoard(nextSide(kingSide), Piece::Rook)
                                      | getPieceBitBoard(nextSide(kingSide), Piece::Queen);

    if (enemyRooksOrQueens != BitBoard::Empty) {
        const BitBoard rookXRayFromKing = getRookXRay(kingPosition, anyPiece);
        BitBoard xRayingRooks           = rookXRayFromKing & enemyRooksOrQueens;

        if (xRayingRooks != BitBoard::Empty) {
            const BitBoard rookAttackFromKing = getRookAttack(kingPosition, anyPiece);
            xRayingRooks &= ~rookAttackFromKing;

            while (xRayingRooks != BitBoard::Empty) {
                const BoardPosition pinningPiecePosition = popFirstSetPosition(xRayingRooks);

                const BitBoard pinningBitBoard =
                        rookAttackFromKing & getRookAttack(pinningPiecePosition, anyPiece);

                allPins |= pinningBitBoard;
            }
        }
    }

    const BitBoard enemyBishopsOrQueens = getPieceBitBoard(nextSide(kingSide), Piece::Bishop)
                                        | getPieceBitBoard(nextSide(kingSide), Piece::Queen);

    if (enemyBishopsOrQueens != BitBoard::Empty) {
        const BitBoard bishopXRayFromKing = getBishopXRay(kingPosition, anyPiece);
        BitBoard xRayingBishops           = bishopXRayFromKing & enemyBishopsOrQueens;

        if (xRayingBishops != BitBoard::Empty) {
            const BitBoard bishopAttackFromKing = getBishopAttack(kingPosition, anyPiece);
            xRayingBishops &= ~bishopAttackFromKing;

            while (xRayingBishops != BitBoard::Empty) {
                const BoardPosition pinningPiecePosition = popFirstSetPosition(xRayingBishops);

                const BitBoard pinningBitBoard =
                        bishopAttackFromKing & getBishopAttack(pinningPiecePosition, anyPiece);

                allPins |= pinningBitBoard;
            }
        }
    }

    pinBitBoards_[(int)kingSide] = allPins;

    return *pinBitBoards_[(int)kingSide];
}

FORCE_INLINE GameState::DirectCheckBitBoards GameState::getDirectCheckBitBoards() const {
    const BitBoard anyPiece               = getAnyOccupancy();
    const Side enemySide                  = nextSide(sideToMove_);
    const BitBoard enemyKingBitBoard      = getPieceBitBoard(enemySide, Piece::King);
    const BoardPosition enemyKingPosition = getFirstSetPosition(enemyKingBitBoard);

    const BitBoard directPawnChecks = getPawnControlledSquares(enemyKingBitBoard, enemySide);
    const BitBoard directKnightChecks =
            getPieceControlledSquares(Piece::Knight, enemyKingPosition, anyPiece);
    const BitBoard directBishopChecks =
            getPieceControlledSquares(Piece::Bishop, enemyKingPosition, anyPiece);
    const BitBoard directRookChecks =
            getPieceControlledSquares(Piece::Rook, enemyKingPosition, anyPiece);
    const BitBoard directQueenChecks = directBishopChecks | directRookChecks;

    return std::array{
            directPawnChecks,
            directKnightChecks,
            directBishopChecks,
            directRookChecks,
            directQueenChecks};
}

bool GameState::enPassantWillPutUsInCheck() const {
    MY_ASSERT(enPassantTarget_ != BoardPosition::Invalid);

    const BoardPosition enPassantPiecePosition =
            getEnPassantPiecePosition(enPassantTarget_, sideToMove_);
    const auto [enPassantTargetFile, enPassantOriginRank] =
            fileRankFromPosition(enPassantPiecePosition);

    BitBoard nextToEnPassantOriginMask = BitBoard::Empty;
    if (enPassantTargetFile > 0) {
        nextToEnPassantOriginMask |=
                positionFromFileRank(enPassantTargetFile - 1, enPassantOriginRank);
    }
    if (enPassantTargetFile < kFiles - 1) {
        nextToEnPassantOriginMask |=
                positionFromFileRank(enPassantTargetFile + 1, enPassantOriginRank);
    }
    const BitBoard& ownPawnBitBoard = getPieceBitBoard(sideToMove_, Piece::Pawn);
    BitBoard neighboringPawns       = ownPawnBitBoard & nextToEnPassantOriginMask;
    const int numOwnPawns           = std::popcount((std::uint64_t)neighboringPawns);

    if (numOwnPawns == 2) {
        // If there's two neighboring own pawns, the en passant capture can't put us in check because
        // of a discovered attack by a rook along the rank.
        // However, we could still be put in check because of a discovered attack by a queen or
        // bishop along the diagonal. But for those the vacating of the own pawns doesn't matter.
        // So we can zero out the neighboringPawns.
        neighboringPawns = BitBoard::Empty;
    }

    // Check whether a rook, bishop, or queen can attack the king after vacating the en passant
    // target and own neighboring pawns.
    // This assumes we're not currently in check by a rook/bishop/queen.

    BitBoard occupancyAfterEnPassant = getAnyOccupancy();
    occupancyAfterEnPassant &= ~neighboringPawns;
    occupancyAfterEnPassant &= ~enPassantPiecePosition;
    occupancyAfterEnPassant |= enPassantTarget_;

    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));
    const BitBoard enemyQueens = getPieceBitBoard(nextSide(sideToMove_), Piece::Queen);

    const BitBoard bishopAttackFromKing = getBishopAttack(kingPosition, occupancyAfterEnPassant);
    const BitBoard enemyBishops         = getPieceBitBoard(nextSide(sideToMove_), Piece::Bishop);
    const BitBoard enemyDiagonalMovers  = enemyBishops | enemyQueens;
    if ((bishopAttackFromKing & enemyDiagonalMovers) != BitBoard::Empty) {
        return true;
    }

    const BitBoard rookAttackFromKing  = getRookAttack(kingPosition, occupancyAfterEnPassant);
    const BitBoard enemyRooks          = getPieceBitBoard(nextSide(sideToMove_), Piece::Rook);
    const BitBoard enemyStraightMovers = enemyRooks | enemyQueens;
    if ((rookAttackFromKing & enemyStraightMovers) != BitBoard::Empty) {
        return true;
    }

    return false;
}

FORCE_INLINE BoardControl GameState::getBoardControl() const {
    BoardControl boardControl{};
    int pieceControlIdx = 0;

    const BitBoard anyPiece = getAnyOccupancy();

    for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
        const Side side = (Side)sideIdx;

        if (side == Side::Black) {
            boardControl.blackControlStartIdx = pieceControlIdx;
        }

        BitBoard& sideControl = boardControl.sideControl[sideIdx];

        BitBoard& pawnControl       = boardControl.pieceTypeControl[sideIdx][(int)Piece::Pawn];
        const BitBoard pawnBitBoard = getPieceBitBoard(side, Piece::Pawn);
        pawnControl                 = getPawnControlledSquares(pawnBitBoard, side);
        sideControl |= pawnControl;

        for (int pieceTypeIdx = (int)Piece::Knight; pieceTypeIdx < kNumPieceTypes; ++pieceTypeIdx) {
            const Piece piece          = (Piece)pieceTypeIdx;
            BitBoard& pieceTypeControl = boardControl.pieceTypeControl[sideIdx][pieceTypeIdx];

            BitBoard pieceBitBoard = getPieceBitBoard(side, piece);
            while (pieceBitBoard != BitBoard::Empty) {
                const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);
                BitBoard& pieceControl            = boardControl.pieceControl[pieceControlIdx++];

                pieceControl = getPieceControlledSquares(piece, piecePosition, anyPiece);
                pieceTypeControl |= pieceControl;
            }
            sideControl |= pieceTypeControl;
        }
    }

    return boardControl;
}

bool GameState::isInCheck(const BoardControl& boardControl) const {
    const BitBoard& enemyControl = boardControl.getEnemyControl(sideToMove_);
    return enemyControl & getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));
}

bool GameState::isRepetition(const int repetitionThreshold) const {
    int repetitions = 0;
    for (int hashIdx = (int)previousHashes_.size() - 3; hashIdx >= lastReversiblePositionHashIdx_;
         hashIdx -= 2) {
        if (previousHashes_[hashIdx] == boardHash_) {
            ++repetitions;
            if (repetitions == repetitionThreshold - 1) {
                return true;
            }
        }
    }

    return false;
}

bool GameState::isFiftyMoves() const {
    // 50 move rule
    if (plySinceCaptureOrPawn_ >= 100) {
        return true;
    }

    return false;
}

bool GameState::givesCheck(
        const Move& move,
        const std::array<BitBoard, kNumPieceTypes - 1>& directCheckBitBoards,
        const std::optional<BitBoard>& enemyPinBitBoard) const {
    if (move.pieceToMove != Piece::King && !isPromotion(move)) {
        const auto& pieceDirectCheckBitBoard = directCheckBitBoards[(int)move.pieceToMove];
        if (pieceDirectCheckBitBoard & move.to) {
            return true;
        }
    }

    bool needToCheckDiscoveredChecks = true;
    if (enemyPinBitBoard) {
        // If we have the pin bit board calculated, use it to check if this piece was 'pinned'
        // (in this case, that means shielding a discovered attack).
        // If not, moving the piece can't reveal a discovered attack, so we don't need to check for
        // that.
        // An en passant capture can remove two pieces from a rank in one move, so the logic doesn't
        // work there and we always need to check those.
        needToCheckDiscoveredChecks = (*enemyPinBitBoard & move.from) || isEnPassant(move);
    }

    const bool isSpecialMove = isCastle(move) || isPromotion(move);

    if (!needToCheckDiscoveredChecks && !isSpecialMove) {
        return false;
    }

    const BitBoard enemyKingBitBoard = getPieceBitBoard(nextSide(sideToMove_), Piece::King);
    const BitBoard occupied          = getAnyOccupancy();

    const Piece movedPiece = isPromotion(move) ? getPromotionPiece(move) : move.pieceToMove;

    BitBoard occupancyAfterMove = (occupied & ~move.from) | move.to;

    // Check for direct attacks from special moves
    if (isCastle(move)) {
        const auto [kingFromFile, kingFromRank] = fileRankFromPosition(move.from);

        const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);

        const BoardPosition rookToPosition =
                positionFromFileRank((kingFromFile + kingToFile) / 2, kingFromRank);

        const BitBoard rookAttack = getRookAttack(rookToPosition, occupancyAfterMove);
        if ((rookAttack & enemyKingBitBoard) != BitBoard::Empty) {
            return true;
        }

        // Castling can't give a discovered check
        return false;
    } else if (isPromotion(move)) {
        // For promotions we can't use the direct check bit boards, because the pawn's old
        // position might block the promoted piece's attack to the enemy king.
        const BitBoard newControl =
                getPieceControlledSquares(movedPiece, move.to, occupancyAfterMove);
        if ((newControl & enemyKingBitBoard) != BitBoard::Empty) {
            return true;
        }
    }

    // Check for discovered checks
    if (needToCheckDiscoveredChecks) {
        if (isEnPassant(move)) {
            const BoardPosition enPassantPiecePosition =
                    getEnPassantPiecePosition(move.to, sideToMove_);
            occupancyAfterMove &= ~enPassantPiecePosition;
        }

        const BoardPosition kingPosition = getFirstSetPosition(enemyKingBitBoard);
        const BitBoard ownQueens         = getPieceBitBoard(sideToMove_, Piece::Queen);

        const BitBoard bishopAttackFromKing = getBishopAttack(kingPosition, occupancyAfterMove);
        const BitBoard ownBishops           = getPieceBitBoard(sideToMove_, Piece::Bishop);
        const BitBoard ownDiagonalMovers    = ownBishops | ownQueens;
        if ((bishopAttackFromKing & ownDiagonalMovers) != BitBoard::Empty) {
            return true;
        }

        const BitBoard rookAttackFromKing = getRookAttack(kingPosition, occupancyAfterMove);
        const BitBoard ownRooks           = getPieceBitBoard(sideToMove_, Piece::Rook);
        const BitBoard ownStraightMovers  = ownRooks | ownQueens;
        if ((rookAttackFromKing & ownStraightMovers) != BitBoard::Empty) {
            return true;
        }
    }

    return false;
}

GameState::CheckInformation GameState::getCheckInformation() const {
    const Side enemySide    = nextSide(sideToMove_);
    const BitBoard anyPiece = getAnyOccupancy();
    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));

    CheckInformation checkInformation{};

    const BitBoard pawnControl =
            getPawnControlledSquares(getPieceBitBoard(enemySide, Piece::Pawn), enemySide);
    if (pawnControl & kingPosition) {
        checkInformation.checkingPieceId = {Piece::Pawn, BoardPosition::Invalid};
    }

    // Skip king
    for (int pieceIdx = (int)Piece::Pawn + 1; pieceIdx < (int)Piece::King; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(enemySide, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);

            const BitBoard pieceControl = getPieceControlledSquares(piece, piecePosition, anyPiece);

            if (pieceControl & kingPosition) {
                if (checkInformation.checkingPieceId.piece == Piece::Invalid) {
                    checkInformation.checkingPieceId      = {piece, piecePosition};
                    checkInformation.checkingPieceControl = pieceControl;
                } else {
                    checkInformation.secondCheckingPieceId = {piece, piecePosition};

                    // Can't have more than two checking pieces.
                    return checkInformation;
                }
            }
        }
    }

    return checkInformation;
}

void GameState::setCanCastleKingSide(const Side side, const bool canCastle) {
    setCanCastle(side, CastlingRights::KingSide, canCastle);
}

void GameState::setCanCastleQueenSide(const Side side, const bool canCastle) {
    setCanCastle(side, CastlingRights::QueenSide, canCastle);
}

void GameState::setCanCastle(
        const Side side, const CastlingRights castlingSide, const bool canCastle) {
    const int bit = (int)castlingSide << ((int)side * 2);
    if (canCastle) {
        castlingRights_ = (CastlingRights)((int)castlingRights_ | bit);
    } else {
        castlingRights_ = (CastlingRights)((int)castlingRights_ & ~bit);
    }
}
