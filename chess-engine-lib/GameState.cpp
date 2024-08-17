#include "GameState.h"

#include "Macros.h"
#include "Math.h"
#include "MyAssert.h"
#include "PieceControl.h"

#define IMPLIES(a, b) (!(a) || (b))

namespace {

void generatePawnMoves(
        const BitBoard pawnBitBoard,
        const Side side,
        const GameState::PieceOccupancyBitBoards& occupancy,
        const BoardPosition enPassantTarget,
        const std::array<BitBoard, kNumPiecesPerSide>& piecePinBitBoards,
        const BitBoard pinBitBoard,
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
        return side == Side::White ? (BitBoard)(((std::uint64_t)bitBoard & notWestFileMask) << 7)
                                   : (BitBoard)(((std::uint64_t)bitBoard & notWestFileMask) >> 9);
    };
    auto rightForwardShift = [=](const BitBoard bitBoard) FORCE_INLINE {
        return side == Side::White ? (BitBoard)(((std::uint64_t)bitBoard & notEastFileMask) << 9)
                                   : (BitBoard)(((std::uint64_t)bitBoard & notEastFileMask) >> 7);
    };

    const BitBoard anyPiece = occupancy.ownPiece | occupancy.enemyPiece;
    BitBoard captureTargets = occupancy.enemyPiece;
    if (enPassantTarget != BoardPosition::Invalid) {
        captureTargets |= enPassantTarget;
    }

    BitBoard leftCaptures  = leftForwardShift(pawnBitBoard) & captureTargets;
    BitBoard rightCaptures = rightForwardShift(pawnBitBoard) & captureTargets;

    leftCaptures  = leftCaptures & checkResolutionBitBoard;
    rightCaptures = rightCaptures & checkResolutionBitBoard;

    const int forwardBits   = side == Side::White ? 8 : -8;
    constexpr int leftBits  = -1;
    constexpr int rightBits = 1;

    const int promotionRank = side == Side::White ? 7 : 0;

    auto generateMoves = [&](BitBoard targetBitBoard,
                             const int originOffset,
                             MoveFlags baseFlags) FORCE_INLINE {
        while (targetBitBoard != BitBoard::Empty) {
            const BoardPosition targetPosition = popFirstSetPosition(targetBitBoard);

            const int originIdx                = (int)targetPosition - originOffset;
            const BoardPosition originPosition = (BoardPosition)originIdx;

            if (pinBitBoard & originPosition) [[unlikely]] {
                // Find pin bit board
                BitBoard pawnPiecePinBitBoard = BitBoard::Empty;
                for (const auto piecePinBitBoard : piecePinBitBoards) {
                    if (piecePinBitBoard & originPosition) {
                        pawnPiecePinBitBoard = piecePinBitBoard;
                        break;
                    }
                }
                MY_ASSERT(pawnPiecePinBitBoard != BitBoard::Empty);

                if (!(pawnPiecePinBitBoard & targetPosition)) {
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

        generateMoves(singlePushes, forwardBits, MoveFlags::None);
        generateMoves(doublePushes, 2 * forwardBits, MoveFlags::None);
    }

    generateMoves(leftCaptures, forwardBits + leftBits, MoveFlags::IsCapture);
    generateMoves(rightCaptures, forwardBits + rightBits, MoveFlags::IsCapture);
}

FORCE_INLINE void generateCastlingMoves(
        const Side sideToMove,
        const bool canCastleKingSide,
        const bool canCastleQueenSide,
        const GameState::PieceOccupancyBitBoards& occupancy,
        const BitBoard enemyControlledSquares,
        StackVector<Move>& moves) {
    MY_ASSERT(sideToMove == Side::White || sideToMove == Side::Black);

    const BoardPosition kingPosition =
            sideToMove == Side::White ? BoardPosition::E1 : BoardPosition::E8;

    const BitBoard anyPiece = occupancy.ownPiece | occupancy.enemyPiece;

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
        const GameState::PieceOccupancyBitBoards& occupancy,
        const BitBoard piecePinBitBoard,
        StackVector<Move>& moves,
        bool capturesOnly) {
    // Can't move to our own pieces
    controlledSquares = controlledSquares & ~occupancy.ownPiece;

    // Pinned pieces can only move along the pin direction
    controlledSquares = controlledSquares & piecePinBitBoard;

    BitBoard captures = controlledSquares & occupancy.enemyPiece;
    while (captures != BitBoard::Empty) {
        const BoardPosition capturePosition = popFirstSetPosition(captures);
        moves.emplace_back(piece, piecePosition, capturePosition, MoveFlags::IsCapture);
    }

    if (!capturesOnly) {
        BitBoard nonCaptures = controlledSquares & ~occupancy.enemyPiece;
        while (nonCaptures != BitBoard::Empty) {
            const BoardPosition movePosition = popFirstSetPosition(nonCaptures);
            moves.emplace_back(piece, piecePosition, movePosition);
        }
    }
}

FORCE_INLINE bool getFileRankIncrement(
        const Piece piece,
        const BoardPosition from,
        const BoardPosition to,
        int& fileIncrement,
        int& rankIncrement) {
    const auto [fromFile, fromRank] = fileRankFromPosition(from);
    const auto [toFile, toRank]     = fileRankFromPosition(to);
    const int deltaFile             = toFile - fromFile;
    const int deltaRank             = toRank - fromRank;

    const bool isRookMove   = deltaFile == 0 || deltaRank == 0;
    const bool isBishopMove = std::abs(deltaFile) == std::abs(deltaRank);

    if (!isRookMove && !isBishopMove) {
        return false;
    }
    if (isRookMove && piece != Piece::Rook && piece != Piece::Queen) {
        return false;
    }
    if (isBishopMove && piece != Piece::Bishop && piece != Piece::Queen) {
        return false;
    }

    fileIncrement = signum(deltaFile);
    rankIncrement = signum(deltaRank);

    return true;
}

}  // namespace

GameState GameState::startingPosition() {
    static const GameState startingPosition = fromFen(getStartingPositionFen());
    return startingPosition;
}

bool GameState::isInCheck() const {
    return isInCheck(getEnemyControl());
}

StackVector<Move> GameState::generateMoves(StackOfVectors<Move>& stack, bool capturesOnly) const {
    const BitBoard enemyControl = getEnemyControl();
    return generateMoves(stack, enemyControl, capturesOnly);
}

StackVector<Move> GameState::generateMoves(
        StackOfVectors<Move>& stack, BitBoard enemyControl, bool capturesOnly) const {

    if (isInCheck(enemyControl)) {
        return generateMovesInCheck(stack, enemyControl, capturesOnly);
    }

    StackVector<Move> moves = stack.makeStackVector();

    const std::array<BitBoard, kNumPiecesPerSide> pinBitBoards =
            calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard& pinBitBoard = pinBitBoards.back();

    auto getPiecePinBitBoard = [&](BoardPosition position) {
        if (!(pinBitBoard & position)) {
            return BitBoard::Full;
        }
        // Piece is pinned: can only move along the pin direction
        // Find the pinning piece
        for (const auto& pinBitBoard : pinBitBoards) {
            if (pinBitBoard & position) {
                return pinBitBoard;
            }
        }
        UNREACHABLE;
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
            pinBitBoards,
            pinBitBoard,
            moves,
            capturesOnly);

    const BitBoard anyPiece = occupancy_.ownPiece | occupancy_.enemyPiece;

    // Generate moves for normal pieces (non-pawns excl. king)
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(sideToMove_, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);
            const BitBoard piecePinBitBoard   = getPiecePinBitBoard(piecePosition);
            const BitBoard controlledSquares =
                    getPieceControlledSquares(piece, piecePosition, anyPiece);

            generateSinglePieceMovesFromControl(
                    piece,
                    piecePosition,
                    controlledSquares,
                    occupancy_,
                    piecePinBitBoard,
                    moves,
                    capturesOnly);
        }
    }

    // Generate king moves

    // Normal king moves
    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));
    // King can't walk into check
    const BitBoard kingControlledSquares = getKingControlledSquares(kingPosition) & ~enemyControl;
    generateSinglePieceMovesFromControl(
            Piece::King,
            kingPosition,
            kingControlledSquares,
            occupancy_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
            moves,
            capturesOnly);

    if (!capturesOnly) {
        // Castling moves
        generateCastlingMoves(
                sideToMove_,
                canCastleKingSide(sideToMove_),
                canCastleQueenSide(sideToMove_),
                occupancy_,
                enemyControl,
                moves);
    }

    moves.lock();
    return moves;
}

StackVector<Move> GameState::generateMovesInCheck(
        StackOfVectors<Move>& stack, const BitBoard enemyControl, bool capturesOnly) const {
    StackVector<Move> moves = stack.makeStackVector();

    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));

    const BitBoard anyPiece = occupancy_.ownPiece | occupancy_.enemyPiece;

    const BitBoard anyPieceNoKing = anyPiece & ~kingPosition;

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
    kingControlledSquares = kingControlledSquares & ~enemyControl;
    kingControlledSquares = kingControlledSquares & ~kingAttackBitBoard;
    generateSinglePieceMovesFromControl(
            Piece::King,
            kingPosition,
            kingControlledSquares,
            occupancy_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
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

    // Treat the pin or king attack bitboard as the pin bitboard.
    // This is fine: any piece directly on the other side of the king from the checking piece (if
    // the checking piece is a sliding piece) will be incorrectly marked as pinned, but those
    // pieces wouldn't be able to block or capture the checking piece anyway. (If it's a sliding
    // piece it can't move through the king; if it's a knight its move doesn't line up with the attacker.)
    const auto pinOrKingAttackBitBoards = calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard& pinBitBoard         = pinOrKingAttackBitBoards.back();

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
            /*unused*/ pinOrKingAttackBitBoards,
            /*pinBitBoard*/ BitBoard::Empty,
            moves,
            capturesOnly,
            pawnBlockOrCaptureBitBoard);

    // Generate moves for normal pieces (non-pawns excl. king)
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(sideToMove_, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);

            if (pinBitBoard & piecePosition) {
                // Piece is pinned; can't capture pinning piece or remain in pin because that wouldn't
                // resolve the check, so no moves.
                continue;
            }

            const BitBoard controlledSquares =
                    getPieceControlledSquares(piece, piecePosition, anyPiece);

            // Treat blockOrCapture as a pin. This will cause only moves that block or capture to be generated.
            generateSinglePieceMovesFromControl(
                    piece,
                    piecePosition,
                    controlledSquares,
                    occupancy_,
                    /*piecePinBitBoard =*/blockOrCaptureBitBoard,
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

    if (isCastle(move.flags)) {
        makeCastleMove(move);
    } else {
        unmakeInfo.capturedPiece = makeSinglePieceMove(move);
    }

    ++halfMoveClock_;

    const bool isIrreversible = isCapture(move.flags) || move.pieceToMove == Piece::Pawn
                             || unmakeInfo.castlingRights != castlingRights_;

    if (isIrreversible) {
        lastReversiblePositionHashIdx_ = (int)previousHashes_.size();
    }

    previousHashes_.push_back(boardHash_);

    return unmakeInfo;
}

GameState::UnmakeMoveInfo GameState::makeNullMove() {
    UnmakeMoveInfo unmakeInfo = {
            .enPassantTarget               = enPassantTarget_,
            .castlingRights                = castlingRights_,
            .plySinceCaptureOrPawn         = plySinceCaptureOrPawn_,
            .lastReversiblePositionHashIdx = lastReversiblePositionHashIdx_};

    if (enPassantTarget_ != BoardPosition::Invalid) {
        updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
    }

    sideToMove_      = nextSide(sideToMove_);
    enPassantTarget_ = BoardPosition::Invalid;
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
    ++halfMoveClock_;

    updateHashForSideToMove(boardHash_);

    // We consider a null move to be irreversible for tie checking purposes.
    // For discussion see: https://www.talkchess.com/forum/viewtopic.php?t=35052
    lastReversiblePositionHashIdx_ = (int)previousHashes_.size();
    plySinceCaptureOrPawn_         = 0;

    previousHashes_.push_back(boardHash_);

    return unmakeInfo;
}

void GameState::unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
    --halfMoveClock_;

    updateHashForSideToMove(boardHash_);

    if (castlingRights_ != unmakeMoveInfo.castlingRights) {
        for (int sideIdx = 0; sideIdx < kNumSides; ++sideIdx) {
            const std::uint8_t kingSideFlag  = (uint8_t)CastlingRights::KingSide << (sideIdx * 2);
            const std::uint8_t queenSideFlag = (uint8_t)CastlingRights::QueenSide << (sideIdx * 2);

            if (((std::uint8_t)castlingRights_ & kingSideFlag)
                != ((std::uint8_t)unmakeMoveInfo.castlingRights & kingSideFlag)) {
                updateHashForKingSideCastlingRights((Side)sideIdx, boardHash_);
            }

            if (((std::uint8_t)castlingRights_ & queenSideFlag)
                != ((std::uint8_t)unmakeMoveInfo.castlingRights & queenSideFlag)) {
                updateHashForQueenSideCastlingRights((Side)sideIdx, boardHash_);
            }
        }

        castlingRights_ = unmakeMoveInfo.castlingRights;
    }

    if (enPassantTarget_ != unmakeMoveInfo.enPassantTarget) {
        if (enPassantTarget_ != BoardPosition::Invalid) {
            updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
        }

        enPassantTarget_ = unmakeMoveInfo.enPassantTarget;

        if (enPassantTarget_ != BoardPosition::Invalid) {
            updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
        }
    }

    lastReversiblePositionHashIdx_ = unmakeMoveInfo.lastReversiblePositionHashIdx;
    previousHashes_.pop_back();

#ifndef NDEBUG
    const HashT expectedHash = previousHashes_.back();
#endif

    if (isCastle(move.flags)) {
        makeCastleMove(move, /*reverse*/ true);
    } else {
        unmakeSinglePieceMove(move, unmakeMoveInfo);
    }

    MY_ASSERT_DEBUG(boardHash_ == expectedHash);
}

void GameState::unmakeNullMove(const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    enPassantTarget_       = unmakeMoveInfo.enPassantTarget;
    castlingRights_        = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
    --halfMoveClock_;

    lastReversiblePositionHashIdx_ = unmakeMoveInfo.lastReversiblePositionHashIdx;
    previousHashes_.pop_back();

    updateHashForSideToMove(boardHash_);

    if (enPassantTarget_ != BoardPosition::Invalid) {
        updateHashForEnPassantFile(fileFromPosition(enPassantTarget_), boardHash_);
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

    occupancy_.ownPiece &= ~kingFromPosition;
    occupancy_.ownPiece |= kingToPosition;

    occupancy_.ownPiece &= ~rookFromPosition;
    occupancy_.ownPiece |= rookToPosition;

    // Update king
    getPieceBitBoard(sideToMove_, Piece::King) = (BitBoard)(1ULL << (int)kingToPosition);

    getPieceOnSquare(kingToPosition)   = getPieceOnSquare(kingFromPosition);
    getPieceOnSquare(kingFromPosition) = ColoredPiece::Invalid;

    updateHashForPiecePosition(sideToMove_, Piece::King, kingFromPosition, boardHash_);
    updateHashForPiecePosition(sideToMove_, Piece::King, kingToPosition, boardHash_);

    // Update rook
    BitBoard& rookBitBoard = getPieceBitBoard(sideToMove_, Piece::Rook);
    rookBitBoard &= ~rookFromPosition;
    rookBitBoard |= rookToPosition;

    getPieceOnSquare(rookToPosition)   = getPieceOnSquare(rookFromPosition);
    getPieceOnSquare(rookFromPosition) = ColoredPiece::Invalid;

    updateHashForPiecePosition(sideToMove_, Piece::Rook, rookFromPosition, boardHash_);
    updateHashForPiecePosition(sideToMove_, Piece::Rook, rookToPosition, boardHash_);

    if (!reverse) {
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
        std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);

        updateHashForSideToMove(boardHash_);
    }
}

Piece GameState::makeSinglePieceMove(const Move& move) {
    Piece capturedPiece               = Piece::Invalid;
    BoardPosition captureTargetSquare = move.to;

    if (isEnPassant(move.flags)) {
        MY_ASSERT(isCapture(move.flags));
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

    occupancy_.ownPiece &= ~move.from;
    occupancy_.ownPiece |= move.to;

    if (isCapture(move.flags)) {
        occupancy_.enemyPiece &= ~captureTargetSquare;

        capturedPiece = getPiece(getPieceOnSquare(captureTargetSquare));
        MY_ASSERT(capturedPiece != Piece::Invalid);

        getPieceBitBoard(nextSide(sideToMove_), capturedPiece) &= ~captureTargetSquare;

        if (capturedPiece == Piece::Rook) {
            updateRookCastlingRights(captureTargetSquare, nextSide(sideToMove_));
        }

        updateHashForPiecePosition(
                nextSide(sideToMove_), capturedPiece, captureTargetSquare, boardHash_);
    }

    BitBoard& pieceBitBoard = getPieceBitBoard(sideToMove_, move.pieceToMove);
    pieceBitBoard &= ~move.from;
    pieceBitBoard |= move.to;

    MY_ASSERT(getPiece(getPieceOnSquare(move.from)) == move.pieceToMove);
    MY_ASSERT(getSide(getPieceOnSquare(move.from)) == sideToMove_);

    MY_ASSERT(IMPLIES(
            isCapture(move.flags), getPieceOnSquare(captureTargetSquare) != ColoredPiece::Invalid));
    MY_ASSERT(
            IMPLIES(isCapture(move.flags),
                    getSide(getPieceOnSquare(captureTargetSquare)) == nextSide(sideToMove_)));

    getPieceOnSquare(move.to)   = getPieceOnSquare(move.from);
    getPieceOnSquare(move.from) = ColoredPiece::Invalid;

    updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.from, boardHash_);
    updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.to, boardHash_);

    if (isEnPassant(move.flags)) {
        getPieceOnSquare(captureTargetSquare) = ColoredPiece::Invalid;
    }

    if (move.pieceToMove == Piece::Pawn) {
        handlePawnMove(move);
    } else if (move.pieceToMove == Piece::King) {
        handleNormalKingMove();
    } else if (move.pieceToMove == Piece::Rook) {
        updateRookCastlingRights(move.from, sideToMove_);
    }

    if (isCapture(move.flags) || move.pieceToMove == Piece::Pawn) {
        plySinceCaptureOrPawn_ = 0;
    } else {
        ++plySinceCaptureOrPawn_;
    }

    sideToMove_ = nextSide(sideToMove_);
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);

    updateHashForSideToMove(boardHash_);

    return capturedPiece;
}

void GameState::unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    occupancy_.ownPiece |= move.from;
    occupancy_.ownPiece &= ~move.to;

    BitBoard& pieceBitBoard = getPieceBitBoard(sideToMove_, move.pieceToMove);
    pieceBitBoard &= ~move.to;
    pieceBitBoard |= move.from;

    // Can't use getPieceOnSquare(move.to) here because that fails when undoing a promotion.
    getPieceOnSquare(move.from) = getColoredPiece(move.pieceToMove, sideToMove_);

    updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.from, boardHash_);

    const Piece promotionPiece = getPromotionPiece(move.flags);
    if (promotionPiece != Piece::Pawn) {
        BitBoard& promotionBitBoard = getPieceBitBoard(sideToMove_, promotionPiece);
        promotionBitBoard &= ~move.to;

        updateHashForPiecePosition(sideToMove_, promotionPiece, move.to, boardHash_);
    } else {
        updateHashForPiecePosition(sideToMove_, move.pieceToMove, move.to, boardHash_);
    }

    if (isCapture(move.flags)) {
        MY_ASSERT(unmakeMoveInfo.capturedPiece != Piece::Invalid);

        BoardPosition captureTarget = move.to;

        if (isEnPassant(move.flags)) {
            const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
            const auto [toFile, toRank]     = fileRankFromPosition(move.to);
            captureTarget                   = positionFromFileRank(toFile, fromRank);
        }

        BitBoard& capturedPieceBitBoard =
                getPieceBitBoard(nextSide(sideToMove_), unmakeMoveInfo.capturedPiece);
        capturedPieceBitBoard |= captureTarget;
        occupancy_.enemyPiece |= captureTarget;

        getPieceOnSquare(captureTarget) =
                getColoredPiece(unmakeMoveInfo.capturedPiece, nextSide(sideToMove_));
        if (isEnPassant(move.flags)) {
            getPieceOnSquare(move.to) = ColoredPiece::Invalid;
        }

        updateHashForPiecePosition(
                nextSide(sideToMove_), unmakeMoveInfo.capturedPiece, captureTarget, boardHash_);
    } else {
        getPieceOnSquare(move.to) = ColoredPiece::Invalid;
    }
}

void GameState::handlePawnMove(const Move& move) {
    const Piece promotionPiece = getPromotionPiece(move.flags);
    if (promotionPiece != Piece::Pawn) {
        BitBoard& pawnBitBoard           = getPieceBitBoard(sideToMove_, Piece::Pawn);
        BitBoard& promotionPieceBitBoard = getPieceBitBoard(sideToMove_, promotionPiece);

        pawnBitBoard &= ~move.to;
        promotionPieceBitBoard |= move.to;

        getPieceOnSquare(move.to) = getColoredPiece(promotionPiece, sideToMove_);

        updateHashForPiecePosition(sideToMove_, Piece::Pawn, move.to, boardHash_);
        updateHashForPiecePosition(sideToMove_, promotionPiece, move.to, boardHash_);
    }

    const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
    const auto [_, toRank]          = fileRankFromPosition(move.to);

    if (std::abs(fromRank - toRank) == 2) {
        // Double pawn push
        enPassantTarget_ = positionFromFileRank(fromFile, (fromRank + toRank) / 2);

        updateHashForEnPassantFile(fromFile, boardHash_);
    }
}

void GameState::handleNormalKingMove() {
    if (canCastleKingSide(sideToMove_)) {
        setCanCastleKingSide(sideToMove_, false);
        updateHashForKingSideCastlingRights(sideToMove_, boardHash_);
    }

    if (canCastleQueenSide(sideToMove_)) {
        setCanCastleQueenSide(sideToMove_, false);
        updateHashForQueenSideCastlingRights(sideToMove_, boardHash_);
    }
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

FORCE_INLINE std::array<BitBoard, kNumPiecesPerSide>
GameState::calculatePiecePinOrKingAttackBitBoards(const Side kingSide) const {
    std::array<BitBoard, kNumPiecesPerSide> piecePinOrKingAttackBitBoards{};
    const BoardPosition kingPosition = getFirstSetPosition(getPieceBitBoard(kingSide, Piece::King));
    const BitBoard anyPiece          = occupancy_.ownPiece | occupancy_.enemyPiece;

    const BitBoard rookXRayFromKing   = getRookXRay(kingPosition, anyPiece);
    const BitBoard bishopXRayFromKing = getBishopXRay(kingPosition, anyPiece);

    const BitBoard enemyRooksOrQueens = getPieceBitBoard(nextSide(kingSide), Piece::Rook)
                                      | getPieceBitBoard(nextSide(kingSide), Piece::Queen);

    const BitBoard enemyBishopsOrQueens = getPieceBitBoard(nextSide(kingSide), Piece::Bishop)
                                        | getPieceBitBoard(nextSide(kingSide), Piece::Queen);

    BitBoard xRayingRooks   = rookXRayFromKing & enemyRooksOrQueens;
    BitBoard xRayingBishops = bishopXRayFromKing & enemyBishopsOrQueens;

    int pinIdx        = 0;
    BitBoard& allPins = piecePinOrKingAttackBitBoards[kNumPiecesPerSide - 1ULL];

    while (xRayingRooks != BitBoard::Empty) {
        const BoardPosition pinningPiecePosition = popFirstSetPosition(xRayingRooks);

        int fileIncrement;
        int rankIncrement;
        const bool incrementOk = getFileRankIncrement(
                Piece::Rook, pinningPiecePosition, kingPosition, fileIncrement, rankIncrement);
        MY_ASSERT(incrementOk);

        BitBoard pinningBitBoard = getRookXRay(pinningPiecePosition, anyPiece);
        const BitBoard fullRay =
                (BitBoard)getFullRay(pinningPiecePosition, fileIncrement, rankIncrement);
        pinningBitBoard &= fullRay;

        MY_ASSERT(pinningBitBoard & kingPosition);

        MY_ASSERT(pinIdx < piecePinOrKingAttackBitBoards.size());
        piecePinOrKingAttackBitBoards[pinIdx++] = pinningBitBoard;
        allPins |= pinningBitBoard;
    }

    while (xRayingBishops != BitBoard::Empty) {
        const BoardPosition pinningPiecePosition = popFirstSetPosition(xRayingBishops);

        int fileIncrement;
        int rankIncrement;
        const bool incrementOk = getFileRankIncrement(
                Piece::Bishop, pinningPiecePosition, kingPosition, fileIncrement, rankIncrement);
        MY_ASSERT(incrementOk);

        BitBoard pinningBitBoard = getBishopXRay(pinningPiecePosition, anyPiece);
        const BitBoard fullRay =
                (BitBoard)getFullRay(pinningPiecePosition, fileIncrement, rankIncrement);
        pinningBitBoard &= fullRay;

        MY_ASSERT(pinningBitBoard & kingPosition);

        MY_ASSERT(pinIdx < piecePinOrKingAttackBitBoards.size());
        piecePinOrKingAttackBitBoards[pinIdx++] = pinningBitBoard;
        allPins |= pinningBitBoard;
    }

    return piecePinOrKingAttackBitBoards;
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

    BitBoard occupancyAfterEnPassant = (occupancy_.ownPiece | occupancy_.enemyPiece);
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

BitBoard GameState::getEnemyControl() const {
    const Side enemySide    = nextSide(sideToMove_);
    const BitBoard anyPiece = occupancy_.ownPiece | occupancy_.enemyPiece;

    BitBoard enemyControl = BitBoard::Empty;

    enemyControl = getPawnControlledSquares(getPieceBitBoard(enemySide, Piece::Pawn), enemySide);

    for (int pieceIdx = (int)Piece::Pawn + 1; pieceIdx <= (int)Piece::King; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(enemySide, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);

            const BitBoard pieceControl = getPieceControlledSquares(piece, piecePosition, anyPiece);
            enemyControl |= pieceControl;
        }
    }

    return enemyControl;
}

bool GameState::isInCheck(const BitBoard enemyControl) const {
    return enemyControl & getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));
}

bool GameState::isRepetition(int repetitionsForDraw) const {
    // Three-fold repetition
    int repetitions = 0;
    for (int hashIdx = (int)previousHashes_.size() - 3; hashIdx >= lastReversiblePositionHashIdx_;
         hashIdx -= 2) {
        if (previousHashes_[hashIdx] == boardHash_) {
            ++repetitions;
            if (repetitions == repetitionsForDraw) {
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

bool GameState::givesCheck(const Move& move) const {
    const BitBoard enemyKingBitBoard = getPieceBitBoard(nextSide(sideToMove_), Piece::King);
    const BitBoard occupied          = occupancy_.ownPiece | occupancy_.enemyPiece;

    const Piece movedPiece =
            isPromotion(move.flags) ? getPromotionPiece(move.flags) : move.pieceToMove;

    BitBoard occupancyAfterMove = occupied & ~move.from | move.to;
    if (isEnPassant(move.flags)) {
        const BoardPosition enPassantPiecePosition =
                getEnPassantPiecePosition(move.to, sideToMove_);
        occupancyAfterMove &= ~enPassantPiecePosition;
    }

    // Check for direct attacks
    if (isCastle(move.flags)) {
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
    } else if (movedPiece == Piece::Pawn) {
        const BitBoard newPositionBB    = BitBoard::Empty | move.to;
        const BitBoard movedPawnControl = getPawnControlledSquares(newPositionBB, sideToMove_);
        if ((movedPawnControl & enemyKingBitBoard) != BitBoard::Empty) {
            return true;
        }
    } else {
        const BitBoard newControl =
                getPieceControlledSquares(movedPiece, move.to, occupancyAfterMove);
        if ((newControl & enemyKingBitBoard) != BitBoard::Empty) {
            return true;
        }
    }

    // Check for discovered checks
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

    return false;
}

GameState::CheckInformation GameState::getCheckInformation() const {
    const Side enemySide    = nextSide(sideToMove_);
    const BitBoard anyPiece = occupancy_.ownPiece | occupancy_.enemyPiece;
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
