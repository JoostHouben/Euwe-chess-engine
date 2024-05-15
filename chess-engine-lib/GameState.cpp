#include "GameState.h"

#include "Macros.h"
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

    const BitBoard anyPiece = any(occupancy.ownPiece, occupancy.enemyPiece);
    BitBoard captureTargets = occupancy.enemyPiece;
    if (enPassantTarget != BoardPosition::Invalid) {
        set(captureTargets, enPassantTarget);
    }

    BitBoard singlePushes = subtract(forwardShift(pawnBitBoard), anyPiece);

    const BitBoard startingPawns           = intersection(pawnBitBoard, (BitBoard)startingRankMask);
    const BitBoard startingPawnsSinglePush = subtract(forwardShift(startingPawns), anyPiece);
    BitBoard doublePushes = subtract(forwardShift(startingPawnsSinglePush), anyPiece);

    BitBoard leftCaptures  = intersection(leftForwardShift(pawnBitBoard), captureTargets);
    BitBoard rightCaptures = intersection(rightForwardShift(pawnBitBoard), captureTargets);

    singlePushes  = intersection(singlePushes, checkResolutionBitBoard);
    doublePushes  = intersection(doublePushes, checkResolutionBitBoard);
    leftCaptures  = intersection(leftCaptures, checkResolutionBitBoard);
    rightCaptures = intersection(rightCaptures, checkResolutionBitBoard);

    const int forwardBits   = side == Side::White ? 8 : -8;
    constexpr int leftBits  = -1;
    constexpr int rightBits = 1;

    const int promotionRank = side == Side::White ? 7 : 0;

    auto generateMoves =
            [&](BitBoard targetBitBoard, const int originOffset, MoveFlags baseFlags) FORCE_INLINE {
                while (targetBitBoard != BitBoard::Empty) {
                    const BoardPosition targetPosition = popFirstSetPosition(targetBitBoard);

                    const int originIdx                = (int)targetPosition - originOffset;
                    const BoardPosition originPosition = (BoardPosition)originIdx;

                    if (isSet(pinBitBoard, originPosition)) [[unlikely]] {
                        // Find pin bit board
                        BitBoard pawnPiecePinBitBoard = BitBoard::Empty;
                        for (const auto piecePinBitBoard : piecePinBitBoards) {
                            if (isSet(piecePinBitBoard, originPosition)) {
                                pawnPiecePinBitBoard = piecePinBitBoard;
                                break;
                            }
                        }
                        MY_ASSERT(pawnPiecePinBitBoard != BitBoard::Empty);

                        if (!isSet(pawnPiecePinBitBoard, targetPosition)) {
                            // Pin is not along the move direction, so move is impossible
                            continue;
                        }
                    }

                    MoveFlags flags = baseFlags;
                    if (targetPosition == enPassantTarget) {
                        MY_ASSERT(flags == MoveFlags::IsCapture);
                        flags = getFlags(flags, MoveFlags::IsEnPassant);
                    }

                    int toRank = rankFromPosition(targetPosition);
                    if (toRank == promotionRank) {
                        for (const auto promotionPiece : kPromotionPieces) {
                            moves.emplace_back(
                                    Piece::Pawn,
                                    originPosition,
                                    targetPosition,
                                    getFlags(flags, promotionPiece));
                        }
                    } else {
                        moves.emplace_back(Piece::Pawn, originPosition, targetPosition, flags);
                    }
                }
            };

    generateMoves(singlePushes, forwardBits, MoveFlags::None);
    generateMoves(doublePushes, 2 * forwardBits, MoveFlags::None);

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

    const BitBoard anyPiece = any(occupancy.ownPiece, occupancy.enemyPiece);

    const auto [kingFile, kingRank] = fileRankFromPosition(kingPosition);

    if (canCastleKingSide) {
        const BitBoard emptySquaresMask = (BitBoard)(0x60ULL << (kingRank * 8));

        bool castleIsValid = true;
        if (intersection(emptySquaresMask, anyPiece) != BitBoard::Empty) {
            castleIsValid = false;
        }
        if (intersection(emptySquaresMask, enemyControlledSquares) != BitBoard::Empty) {
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
        if (intersection(emptySquaresMask, anyPiece) != BitBoard::Empty) {
            castleIsValid = false;
        }
        if (intersection(controlMask, enemyControlledSquares) != BitBoard::Empty) {
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
        StackVector<Move>& moves) {
    // Can't move to our own pieces
    controlledSquares = subtract(controlledSquares, occupancy.ownPiece);

    // Pinned pieces can only move along the pin direction
    controlledSquares = intersection(controlledSquares, piecePinBitBoard);

    BitBoard captures    = intersection(controlledSquares, occupancy.enemyPiece);
    BitBoard nonCaptures = subtract(controlledSquares, occupancy.enemyPiece);

    while (captures != BitBoard::Empty) {
        const BoardPosition capturePosition = popFirstSetPosition(captures);
        moves.emplace_back(piece, piecePosition, capturePosition, MoveFlags::IsCapture);
    }
    while (nonCaptures != BitBoard::Empty) {
        const BoardPosition movePosition = popFirstSetPosition(nonCaptures);
        moves.emplace_back(piece, piecePosition, movePosition);
    }
}

FORCE_INLINE constexpr int signum(const int x) {
    return (x > 0) - (x < 0);
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
    return fromFen(getStartingPositionFen());
}

bool GameState::isInCheck() const {
    return isInCheck(getEnemyControl());
}

StackVector<Move> GameState::generateMoves(StackOfVectors<Move>& stack) const {
    const BitBoard enemyControl = getEnemyControl();

    if (isInCheck(enemyControl)) {
        return generateMovesInCheck(stack, enemyControl);
    }

    StackVector<Move> moves = stack.makeStackVector();

    const std::array<BitBoard, kNumPiecesPerSide> pinBitBoards =
            calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard& pinBitBoard = pinBitBoards.back();

    auto getPiecePinBitBoard = [&](BoardPosition position) {
        if (!isSet(pinBitBoard, position)) {
            return BitBoard::Full;
        }
        // Piece is pinned: can only move along the pin direction
        // Find the pinning piece
        for (const auto& pinBitBoard : pinBitBoards) {
            if (isSet(pinBitBoard, position)) {
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
            moves);

    const BitBoard anyPiece = any(occupancy_.ownPiece, occupancy_.enemyPiece);

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
                    piece, piecePosition, controlledSquares, occupancy_, piecePinBitBoard, moves);
        }
    }

    // Generate king moves

    // Normal king moves
    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));
    BitBoard kingControlledSquares = getKingControlledSquares(kingPosition);
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControl);
    generateSinglePieceMovesFromControl(
            Piece::King,
            kingPosition,
            kingControlledSquares,
            occupancy_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
            moves);

    // Castling moves
    generateCastlingMoves(
            sideToMove_,
            canCastleKingSide(sideToMove_),
            canCastleQueenSide(sideToMove_),
            occupancy_,
            enemyControl,
            moves);

    moves.lock();
    return moves;
}

StackVector<Move> GameState::generateMovesInCheck(
        StackOfVectors<Move>& stack, const BitBoard enemyControl) const {
    StackVector<Move> moves = stack.makeStackVector();

    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));

    const BitBoard anyPiece = any(occupancy_.ownPiece, occupancy_.enemyPiece);

    BitBoard anyPieceNoKing = anyPiece;
    clear(anyPieceNoKing, kingPosition);

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
        kingAttackBitBoard = any(kingAttackBitBoard, secondCheckingPieceControlledSquares);
    }
    // Controlled squares of the first checking piece (only used when not in double check)
    const BitBoard checkingPieceControlledSquares = checkInformation.checkingPieceControl;

    BitBoard kingControlledSquares = getKingControlledSquares(kingPosition);
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControl);
    kingControlledSquares = subtract(kingControlledSquares, kingAttackBitBoard);
    generateSinglePieceMovesFromControl(
            Piece::King,
            kingPosition,
            kingControlledSquares,
            occupancy_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
            moves);

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

        blockOrCaptureBitBoard = intersection(checkingRay, checkingPieceControlledSquares);
    } else if (checkingPieceId.piece == Piece::Pawn) {
        // Pawn control was calculated 'in bulk', so we don't have the checking pawn's position.
        // We need to calculate it now.
        // We find the checking pawn by considering a pawn at the king's position and seeing which
        // enemy pawns it attacks.

        BitBoard kingPawnBitBoard = BitBoard::Empty;
        set(kingPawnBitBoard, kingPosition);

        const BitBoard kingPawnAttacks = getPawnControlledSquares(kingPawnBitBoard, sideToMove_);
        const BitBoard checkingPawnBitBoard =
                intersection(kingPawnAttacks, getPieceBitBoard(nextSide(sideToMove_), Piece::Pawn));

        checkingPieceId.position = getFirstSetPosition(checkingPawnBitBoard);
    }
    set(blockOrCaptureBitBoard, checkingPieceId.position);

    // Treat the pin or king attack bitboard as the pin bitboard.
    // This is fine: any piece directly on the other side of the king from the checking piece (if
    // the checking piece is a sliding piece) will be incorrectly marked as pinned, but those
    // pieces wouldn't be able to block or capture the checking piece anyway. (If it's a sliding
    // piece it can't move through the king; if it's a knight its move doesn't line up with the attacker.)
    const auto pinOrKingAttackBitBoards = calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard& pinBitBoard         = pinOrKingAttackBitBoards.back();

    bool canTakeCheckingPieceEnPassant = false;
    if (enPassantTarget_ != BoardPosition::Invalid) {
        // TODO extract some helper function(s) for en passant position calculations
        const auto [enPassantFile, enPassantRank] = fileRankFromPosition(enPassantTarget_);
        const int enPassantPieceRank =
                sideToMove_ == Side::White ? enPassantRank - 1 : enPassantRank + 1;
        const BoardPosition enPassantPiecePosition =
                positionFromFileRank(enPassantFile, enPassantPieceRank);

        canTakeCheckingPieceEnPassant = enPassantPiecePosition == checkingPieceId.position;
        MY_ASSERT(IMPLIES(canTakeCheckingPieceEnPassant, checkingPieceId.piece == Piece::Pawn));
    }
    const BoardPosition enPassantTarget =
            canTakeCheckingPieceEnPassant ? enPassantTarget_ : BoardPosition::Invalid;
    BitBoard pawnBlockOrCaptureBitBoard = blockOrCaptureBitBoard;
    if (canTakeCheckingPieceEnPassant) {
        set(pawnBlockOrCaptureBitBoard, enPassantTarget);
    }

    // Generate pawn moves that either capture the checking piece or block
#pragma warning(suppress : 4269)
    const std::array<BitBoard, kNumPiecesPerSide> unusedPiecePinBitBoards;  // NOLINT
    const BitBoard nonPinnedPawns =
            subtract(getPieceBitBoard(sideToMove_, Piece::Pawn), pinBitBoard);
    generatePawnMoves(
            nonPinnedPawns,
            sideToMove_,
            occupancy_,
            enPassantTarget,
            unusedPiecePinBitBoards,
            BitBoard::Empty,
            moves,
            pawnBlockOrCaptureBitBoard);

    // Generate moves for normal pieces (non-pawns excl. king)
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(sideToMove_, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);

            if (isSet(pinBitBoard, piecePosition)) {
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
                    moves);
        }
    }

    moves.lock();
    return moves;
}

GameState::UnmakeMoveInfo GameState::makeMove(const Move& move) {
    UnmakeMoveInfo unmakeInfo = {
            .enPassantTarget       = enPassantTarget_,
            .castlingRights        = castlingRights_,
            .plySinceCaptureOrPawn = plySinceCaptureOrPawn_};

    if (isCastle(move.flags)) {
        makeCastleMove(move);
    } else {
        unmakeInfo.capturedPiece = makeSinglePieceMove(move);
    }

    ++halfMoveClock_;

    return unmakeInfo;
}

GameState::UnmakeMoveInfo GameState::makeNullMove() {
    UnmakeMoveInfo unmakeInfo = {
            .enPassantTarget       = enPassantTarget_,
            .castlingRights        = castlingRights_,
            .plySinceCaptureOrPawn = plySinceCaptureOrPawn_};

    sideToMove_      = nextSide(sideToMove_);
    enPassantTarget_ = BoardPosition::Invalid;
    // Do not increment plySinceCaptureOrPawn_
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
    ++halfMoveClock_;

    return unmakeInfo;
}

void GameState::unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    enPassantTarget_       = unmakeMoveInfo.enPassantTarget;
    castlingRights_        = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
    --halfMoveClock_;

    if (isCastle(move.flags)) {
        makeCastleMove(move, /*reverse*/ true);
    } else {
        unmakeSinglePieceMove(move, unmakeMoveInfo);
    }
}

void GameState::unmakeNullMove(const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    enPassantTarget_       = unmakeMoveInfo.enPassantTarget;
    castlingRights_        = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
    --halfMoveClock_;
}

void GameState::makeCastleMove(const Move& move, const bool reverse) {
    const int kingFromFile = 4;  // e
    const int kingFromRank = sideToMove_ == Side::White ? 0 : 7;

    const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
    const bool isQueenSide              = kingToFile == 2;  // c

    MY_ASSERT(IMPLIES(isQueenSide, canCastleQueenSide(sideToMove_)));
    MY_ASSERT(IMPLIES(!isQueenSide, canCastleKingSide(sideToMove_)));

    const int rookFromFile         = isQueenSide ? /*a*/ 0 : /*h*/ 7;
    BoardPosition rookFromPosition = positionFromFileRank(rookFromFile, kingFromRank);
    BoardPosition rookToPosition =
            positionFromFileRank((kingFromFile + kingToFile) / 2, kingFromRank);

    BoardPosition kingFromPosition = positionFromFileRank(kingFromFile, kingFromRank);
    BoardPosition kingToPosition   = move.to;

    if (reverse) {
        std::swap(rookFromPosition, rookToPosition);
        std::swap(kingFromPosition, kingToPosition);
    }

    clear(occupancy_.ownPiece, kingFromPosition);
    set(occupancy_.ownPiece, kingToPosition);

    clear(occupancy_.ownPiece, rookFromPosition);
    set(occupancy_.ownPiece, rookToPosition);

    // Update king
    getPieceBitBoard(sideToMove_, Piece::King) = (BitBoard)(1ULL << (int)kingToPosition);

    getPieceOnSquare(kingToPosition)   = getPieceOnSquare(kingFromPosition);
    getPieceOnSquare(kingFromPosition) = ColoredPiece::Invalid;

    // Update rook
    BitBoard& rookBitBoard = getPieceBitBoard(sideToMove_, Piece::Rook);
    clear(rookBitBoard, rookFromPosition);
    set(rookBitBoard, rookToPosition);

    getPieceOnSquare(rookToPosition)   = getPieceOnSquare(rookFromPosition);
    getPieceOnSquare(rookFromPosition) = ColoredPiece::Invalid;

    if (!reverse) {
        setCanCastleKingSide(sideToMove_, false);
        setCanCastleQueenSide(sideToMove_, false);

        sideToMove_      = nextSide(sideToMove_);
        enPassantTarget_ = BoardPosition::Invalid;
        ++plySinceCaptureOrPawn_;
        std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
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

    enPassantTarget_ = BoardPosition::Invalid;

    clear(occupancy_.ownPiece, move.from);
    set(occupancy_.ownPiece, move.to);

    if (isCapture(move.flags)) {
        clear(occupancy_.enemyPiece, captureTargetSquare);

        capturedPiece = getPiece(getPieceOnSquare(captureTargetSquare));
        MY_ASSERT(capturedPiece != Piece::Invalid);

        clear(getPieceBitBoard(nextSide(sideToMove_), capturedPiece), captureTargetSquare);

        if (capturedPiece == Piece::Rook) {
            updateRookCastlingRights(captureTargetSquare, nextSide(sideToMove_));
        }
    }

    BitBoard& pieceBitBoard = getPieceBitBoard(sideToMove_, move.pieceToMove);
    clear(pieceBitBoard, move.from);
    set(pieceBitBoard, move.to);

    MY_ASSERT(getPiece(getPieceOnSquare(move.from)) == move.pieceToMove);
    MY_ASSERT(getSide(getPieceOnSquare(move.from)) == sideToMove_);

    MY_ASSERT(IMPLIES(
            isCapture(move.flags), getPieceOnSquare(captureTargetSquare) != ColoredPiece::Invalid));
    MY_ASSERT(
            IMPLIES(isCapture(move.flags),
                    getSide(getPieceOnSquare(captureTargetSquare)) == nextSide(sideToMove_)));

    getPieceOnSquare(move.to)   = getPieceOnSquare(move.from);
    getPieceOnSquare(move.from) = ColoredPiece::Invalid;

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

    return capturedPiece;
}

void GameState::unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    set(occupancy_.ownPiece, move.from);
    clear(occupancy_.ownPiece, move.to);

    BitBoard& pieceBitBoard = getPieceBitBoard(sideToMove_, move.pieceToMove);
    clear(pieceBitBoard, move.to);
    set(pieceBitBoard, move.from);

    // Can't use getPieceOnSquare(move.to) here because that fails when undoing a promotion.
    getPieceOnSquare(move.from) = getColoredPiece(move.pieceToMove, sideToMove_);

    const Piece promotionPiece = getPromotionPiece(move.flags);
    if (promotionPiece != Piece::Pawn) {
        BitBoard& promotionBitBoard = getPieceBitBoard(sideToMove_, promotionPiece);
        clear(promotionBitBoard, move.to);
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
        set(capturedPieceBitBoard, captureTarget);
        set(occupancy_.enemyPiece, captureTarget);

        getPieceOnSquare(captureTarget) =
                getColoredPiece(unmakeMoveInfo.capturedPiece, nextSide(sideToMove_));
        if (isEnPassant(move.flags)) {
            getPieceOnSquare(move.to) = ColoredPiece::Invalid;
        }
    } else {
        getPieceOnSquare(move.to) = ColoredPiece::Invalid;
    }
}

void GameState::handlePawnMove(const Move& move) {
    const Piece promotionPiece = getPromotionPiece(move.flags);
    if (promotionPiece != Piece::Pawn) {
        BitBoard& pawnBitBoard           = getPieceBitBoard(sideToMove_, Piece::Pawn);
        BitBoard& promotionPieceBitBoard = getPieceBitBoard(sideToMove_, promotionPiece);

        clear(pawnBitBoard, move.to);
        set(promotionPieceBitBoard, move.to);

        getPieceOnSquare(move.to) = getColoredPiece(promotionPiece, sideToMove_);
    }

    const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
    const auto [_, toRank]          = fileRankFromPosition(move.to);

    if (std::abs(fromRank - toRank) == 2) {
        // Double pawn push
        enPassantTarget_ = positionFromFileRank(fromFile, (fromRank + toRank) / 2);
    }
}

void GameState::handleNormalKingMove() {
    setCanCastleKingSide(sideToMove_, false);
    setCanCastleQueenSide(sideToMove_, false);
}

void GameState::updateRookCastlingRights(BoardPosition rookPosition, Side rookSide) {
    if (rookSide == Side::White && rookPosition == BoardPosition::A1) {
        setCanCastleQueenSide(rookSide, false);
    } else if (rookSide == Side::White && rookPosition == BoardPosition::H1) {
        setCanCastleKingSide(rookSide, false);
    } else if (rookSide == Side::Black && rookPosition == BoardPosition::A8) {
        setCanCastleQueenSide(rookSide, false);
    } else if (rookSide == Side::Black && rookPosition == BoardPosition::H8) {
        setCanCastleKingSide(rookSide, false);
    }
}

FORCE_INLINE std::array<BitBoard, kNumPiecesPerSide>
GameState::calculatePiecePinOrKingAttackBitBoards(const Side kingSide) const {
    std::array<BitBoard, kNumPiecesPerSide> piecePinOrKingAttackBitBoards{};
    const BoardPosition kingPosition = getFirstSetPosition(getPieceBitBoard(kingSide, Piece::King));
    const BitBoard anyPiece          = any(occupancy_.ownPiece, occupancy_.enemyPiece);

    const BitBoard rookXRayFromKing   = getRookXRay(kingPosition, anyPiece);
    const BitBoard bishopXRayFromKing = getBishopXRay(kingPosition, anyPiece);

    const BitBoard enemyRooksOrQueens =
            any(getPieceBitBoard(nextSide(kingSide), Piece::Rook),
                getPieceBitBoard(nextSide(kingSide), Piece::Queen));

    const BitBoard enemyBishopsOrQueens =
            any(getPieceBitBoard(nextSide(kingSide), Piece::Bishop),
                getPieceBitBoard(nextSide(kingSide), Piece::Queen));

    BitBoard xRayingRooks   = intersection(rookXRayFromKing, enemyRooksOrQueens);
    BitBoard xRayingBishops = intersection(bishopXRayFromKing, enemyBishopsOrQueens);

    int pinIdx        = 0;
    BitBoard& allPins = piecePinOrKingAttackBitBoards[kNumPiecesPerSide - 1];

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
        pinningBitBoard = intersection(fullRay, pinningBitBoard);

        MY_ASSERT(isSet(pinningBitBoard, kingPosition));

        piecePinOrKingAttackBitBoards[pinIdx++] = pinningBitBoard;
        allPins                                 = any(allPins, pinningBitBoard);
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
        pinningBitBoard = intersection(fullRay, pinningBitBoard);

        MY_ASSERT(isSet(pinningBitBoard, kingPosition));

        piecePinOrKingAttackBitBoards[pinIdx++] = pinningBitBoard;
        allPins                                 = any(allPins, pinningBitBoard);
    }

    return piecePinOrKingAttackBitBoards;
}

bool GameState::enPassantWillPutUsInCheck() const {
    MY_ASSERT(enPassantTarget_ != BoardPosition::Invalid);

    const auto [enPassantTargetFile, enPassantTargetRank] = fileRankFromPosition(enPassantTarget_);
    const int enPassantOriginRank =
            sideToMove_ == Side::White ? enPassantTargetRank - 1 : enPassantTargetRank + 1;

    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));
    const auto [kingFile, kingRank] = fileRankFromPosition(kingPosition);
    if (kingRank != enPassantOriginRank) {
        return false;
    }

    const bool kingIsLeft = kingFile < enPassantTargetFile;

    BitBoard nextToEnPassantOriginMask = BitBoard::Empty;
    if (enPassantTargetFile > 0) {
        set(nextToEnPassantOriginMask,
            positionFromFileRank(enPassantTargetFile - 1, enPassantOriginRank));
    }
    if (enPassantTargetFile < kFiles - 1) {
        set(nextToEnPassantOriginMask,
            positionFromFileRank(enPassantTargetFile + 1, enPassantOriginRank));
    }
    const BitBoard& ownPawnBitBoard = getPieceBitBoard(sideToMove_, Piece::Pawn);
    const BitBoard neighboringPawns = intersection(ownPawnBitBoard, nextToEnPassantOriginMask);
    const int numOwnPawns           = std::popcount((std::uint64_t)neighboringPawns);

    if (numOwnPawns != 1) {
        // If zero: no en passant capture is possible.
        // If greater than 1: rank would still be blocked after en passant capture
        return false;
    }

    // Mask for squares on the other side of the king from the en passant target
    std::uint64_t otherSideRankMask;
    // Mask for squares between the king and the en passant target
    std::uint64_t kingSideBlockerMask;
    if (kingIsLeft) {
        // Set bits for file > enPassantTargetFile on rank 0
        otherSideRankMask = (0xffULL << (enPassantTargetFile + 1)) & 0xffUL;

        // Set bits for file < enPassantTargetFile on rank 0
        kingSideBlockerMask = 0xffULL >> (kFiles - enPassantTargetFile);
        // Clear bits for file <= kingFile on rank 0
        kingSideBlockerMask &= ~(0xffULL >> (kFiles - kingFile - 1));
    } else {
        // Set bits for file < enPassantTargetFile on rank 0
        otherSideRankMask = 0xffULL >> (kFiles - enPassantTargetFile);

        // Set bits for file > enPassantTargetFile on rank 0
        kingSideBlockerMask = (0xffULL << (enPassantTargetFile + 1)) & 0xffUL;
        // Clear bits for file >= kingFile on rank 0
        kingSideBlockerMask &= ~((0xffULL << kingFile) & 0xffUL);
    }
    otherSideRankMask <<= enPassantOriginRank * kFiles;
    kingSideBlockerMask <<= enPassantOriginRank * kFiles;

    MY_ASSERT(!isSet((BitBoard)otherSideRankMask, enPassantTarget_));
    MY_ASSERT(!isSet((BitBoard)otherSideRankMask, kingPosition));
    MY_ASSERT(!isSet((BitBoard)kingSideBlockerMask, enPassantTarget_));
    MY_ASSERT(!isSet((BitBoard)kingSideBlockerMask, kingPosition));

    // Enemy rooks or queens on the other side of the king
    BitBoard enemyRookOrQueenBitBoard =
            any(getPieceBitBoard(nextSide(sideToMove_), Piece::Rook),
                getPieceBitBoard(nextSide(sideToMove_), Piece::Queen));
    enemyRookOrQueenBitBoard = intersection(enemyRookOrQueenBitBoard, (BitBoard)otherSideRankMask);

    if (enemyRookOrQueenBitBoard == BitBoard::Empty) {
        // No enemy rook or queen on the other side of the king
        return false;
    }

    // Any piece on the en passant rank that isn't an enemy rook or queen on the other side of the
    // king, or an immediately neighboring own pawn
    BitBoard potentialBlockersBitBoard = any(occupancy_.ownPiece, occupancy_.enemyPiece);
    potentialBlockersBitBoard = subtract(potentialBlockersBitBoard, enemyRookOrQueenBitBoard);
    potentialBlockersBitBoard = subtract(potentialBlockersBitBoard, neighboringPawns);

    // Potential blockers on the other side of the king
    const BitBoard otherSideBitBoard =
            intersection(potentialBlockersBitBoard, (BitBoard)otherSideRankMask);

    // Potential blockers between the king and the double-moved pawn
    const BitBoard kingSideBlockers =
            intersection(potentialBlockersBitBoard, (BitBoard)kingSideBlockerMask);

    if (kingSideBlockers != BitBoard::Empty) {
        // Found a piece between the king and the double-moved pawn (that isn't an immediately neighboring own pawn)
        // This piece blocks any would-be discovered check
        return false;
    }

    int closestEnemyRookOrQueenDistance = kFiles;
    int closestOtherDistance            = kFiles;

    if (kingIsLeft) {
        // King is on the LSB side: find lowest set bit
        const int closestPosition       = std::countr_zero((std::uint64_t)enemyRookOrQueenBitBoard);
        const int closestFile           = fileFromPosition((BoardPosition)closestPosition);
        closestEnemyRookOrQueenDistance = closestFile - enPassantTargetFile;
    } else {
        // King is on the MSB side: find highest set bit
        const int closestPosition =
                kSquares - 1 - std::countl_zero((std::uint64_t)enemyRookOrQueenBitBoard);
        const int closestFile           = fileFromPosition((BoardPosition)closestPosition);
        closestEnemyRookOrQueenDistance = enPassantTargetFile - closestFile;
    }
    MY_ASSERT(closestEnemyRookOrQueenDistance < kFiles);

    if (otherSideBitBoard != BitBoard::Empty) {
        if (kingIsLeft) {
            // King is on the LSB side: find lowest set bit
            const int closestPosition = std::countr_zero((std::uint64_t)otherSideBitBoard);
            const int closestFile     = fileFromPosition((BoardPosition)closestPosition);
            closestOtherDistance      = closestFile - enPassantTargetFile;
        } else {
            // King is on the MSB side: find highest set bit
            const int closestPosition =
                    kSquares - 1 - std::countl_zero((std::uint64_t)otherSideBitBoard);
            const int closestFile = fileFromPosition((BoardPosition)closestPosition);
            closestOtherDistance  = enPassantTargetFile - closestFile;
        }
        MY_ASSERT(closestOtherDistance > 0);
    }

    if (closestEnemyRookOrQueenDistance < closestOtherDistance) {
        // Capturing en passant would put the king in check
        return true;
    }

    return false;
}

BitBoard GameState::getEnemyControl() const {
    const Side enemySide    = nextSide(sideToMove_);
    const BitBoard anyPiece = any(occupancy_.ownPiece, occupancy_.enemyPiece);

    BitBoard enemyControl = BitBoard::Empty;

    enemyControl = getPawnControlledSquares(getPieceBitBoard(enemySide, Piece::Pawn), enemySide);

    for (int pieceIdx = (int)Piece::Pawn + 1; pieceIdx <= (int)Piece::King; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(enemySide, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);

            const BitBoard pieceControl = getPieceControlledSquares(piece, piecePosition, anyPiece);
            enemyControl                = any(enemyControl, pieceControl);
        }
    }

    return enemyControl;
}

bool GameState::isInCheck(const BitBoard enemyControl) const {
    return isSet(enemyControl, getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King)));
}

bool GameState::isForcedDraw() const {
    // TODO: three-fold repetition.

    // 50 move rule
    return plySinceCaptureOrPawn_ >= 100;
}

GameState::CheckInformation GameState::getCheckInformation() const {
    const Side enemySide    = nextSide(sideToMove_);
    const BitBoard anyPiece = any(occupancy_.ownPiece, occupancy_.enemyPiece);
    const BoardPosition kingPosition =
            getFirstSetPosition(getPieceBitBoard(sideToMove_, Piece::King));

    CheckInformation checkInformation{};

    const BitBoard pawnControl =
            getPawnControlledSquares(getPieceBitBoard(enemySide, Piece::Pawn), enemySide);
    if (isSet(pawnControl, kingPosition)) {
        checkInformation.checkingPieceId = {Piece::Pawn, BoardPosition::Invalid};
    }

    // Skip king
    for (int pieceIdx = (int)Piece::Pawn + 1; pieceIdx < (int)Piece::King; ++pieceIdx) {
        const Piece piece = (Piece)pieceIdx;

        BitBoard pieceBitBoard = getPieceBitBoard(enemySide, piece);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition piecePosition = popFirstSetPosition(pieceBitBoard);

            const BitBoard pieceControl = getPieceControlledSquares(piece, piecePosition, anyPiece);

            if (isSet(pieceControl, kingPosition)) {
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
