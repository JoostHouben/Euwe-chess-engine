#include "GameState.h"

#include "MyAssert.h"

#include <array>
#include <bit>

#define IMPLIES(a, b) (!(a) || (b))

namespace {

constexpr std::array kSigns = {+1, -1};

static constexpr std::array kPromotionPieces = {
        Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen};

constexpr std::uint64_t notLeftFileMask   = ~0x0101010101010101ULL;
constexpr std::uint64_t notRightFileMask  = ~0x8080808080808080ULL;
constexpr std::uint64_t notBottomRankMask = ~0xffULL;
constexpr std::uint64_t notTopRankMask    = ~0xff00000000000000ULL;

BitBoard computePawnControlledSquares(const BitBoard pawnBitBoard, const Side side) {
    auto forwardShift = [=](const BitBoard bitBoard) {
        return side == Side::White ? (BitBoard)((std::uint64_t)bitBoard << 8)
                                   : (BitBoard)((std::uint64_t)bitBoard >> 8);
    };
    auto leftShift = [=](const BitBoard bitBoard) {
        return (BitBoard)(((std::uint64_t)bitBoard & notLeftFileMask) >> 1);
    };
    auto rightShift = [=](const BitBoard bitBoard) {
        return (BitBoard)(((std::uint64_t)bitBoard & notRightFileMask) << 1);
    };

    const BitBoard leftCaptures  = leftShift(forwardShift(pawnBitBoard));
    const BitBoard rightCaptures = rightShift(forwardShift(pawnBitBoard));

    return any(leftCaptures, rightCaptures);
}

constexpr int kMaxPawnTargetSquares = 4;

struct PawnTargetSquaresPerOrigin {
    std::array<std::array<BoardPosition, kMaxPawnTargetSquares>, kSquares> targetSquaresPerOrigin;
    std::array<std::uint8_t, kSquares> numSquaresPerOrigin = {};

    void push_back(BoardPosition origin, BoardPosition target) {
        const int originIdx      = (int)origin;
        std::uint8_t& numSquares = numSquaresPerOrigin[originIdx];

        targetSquaresPerOrigin[originIdx][numSquares++] = target;
    }
};

PawnTargetSquaresPerOrigin generatePawnTargetSquares(
        const BitBoard pawnBitBoard,
        const Side side,
        const PieceOccupationBitBoards& occupation,
        const BoardPosition enPassantTarget,
        const std::array<BitBoard, kNumPiecesPerSide - 1>& piecePinBitBoards,
        const BitBoard pinBitBoard,
        const BitBoard checkResolutionBitBoard = BitBoard::Full) {
    PawnTargetSquaresPerOrigin pawnTargetSquaresPerOrigin{};

    const std::uint64_t startingRankMask =
            side == Side::White ? (0xffULL << (1 * 8)) : (0xffULL << (6 * 8));
    auto forwardShift = [=](const BitBoard bitBoard) {
        return side == Side::White ? (BitBoard)((std::uint64_t)bitBoard << 8)
                                   : (BitBoard)((std::uint64_t)bitBoard >> 8);
    };
    auto leftShift = [=](const BitBoard bitBoard) {
        return (BitBoard)(((std::uint64_t)bitBoard & notLeftFileMask) >> 1);
    };
    auto rightShift = [=](const BitBoard bitBoard) {
        return (BitBoard)(((std::uint64_t)bitBoard & notRightFileMask) << 1);
    };

    const BitBoard anyPiece = any(occupation.ownPiece, occupation.enemyPiece);
    BitBoard captureTargets = occupation.enemyPiece;
    if (enPassantTarget != BoardPosition::Invalid) {
        set(captureTargets, enPassantTarget);
    }

    BitBoard singlePushes = subtract(forwardShift(pawnBitBoard), anyPiece);

    const BitBoard startingPawns           = intersection(pawnBitBoard, (BitBoard)startingRankMask);
    const BitBoard startingPawnsSinglePush = subtract(forwardShift(startingPawns), anyPiece);
    BitBoard doublePushes = subtract(forwardShift(startingPawnsSinglePush), anyPiece);

    BitBoard leftCaptures  = intersection(leftShift(forwardShift(pawnBitBoard)), captureTargets);
    BitBoard rightCaptures = intersection(rightShift(forwardShift(pawnBitBoard)), captureTargets);

    singlePushes  = intersection(singlePushes, checkResolutionBitBoard);
    doublePushes  = intersection(doublePushes, checkResolutionBitBoard);
    leftCaptures  = intersection(leftCaptures, checkResolutionBitBoard);
    rightCaptures = intersection(rightCaptures, checkResolutionBitBoard);

    const int forwardBits   = side == Side::White ? 8 : -8;
    constexpr int leftBits  = -1;
    constexpr int rightBits = 1;

    auto addTargetSquares = [&](BitBoard targetBitBoard, const int originOffset) {
        while (targetBitBoard != BitBoard::Empty) {
            const int targetBit = std::countr_zero((std::uint64_t)targetBitBoard);

            const BoardPosition targetPosition = (BoardPosition)targetBit;
            clear(targetBitBoard, targetPosition);

            const int originIdx                = targetBit - originOffset;
            const BoardPosition originPosition = (BoardPosition)originIdx;

            if (isSet(pinBitBoard, originPosition)) {
                // Find pin bit board
                BitBoard pinBitBoard = BitBoard::Empty;
                for (const auto piecePinBitBoard : piecePinBitBoards) {
                    if (isSet(piecePinBitBoard, originPosition)) {
                        pinBitBoard = piecePinBitBoard;
                        break;
                    }
                }
                MY_ASSERT(pinBitBoard != BitBoard::Empty);

                if (!isSet(pinBitBoard, targetPosition)) {
                    // Piece is pinned: only moves along the pinning direction are allowed
                    continue;
                }
            }

            pawnTargetSquaresPerOrigin.push_back(originPosition, targetPosition);
        }
    };

    addTargetSquares(singlePushes, forwardBits);
    addTargetSquares(doublePushes, 2 * forwardBits);

    addTargetSquares(leftCaptures, forwardBits + leftBits);
    addTargetSquares(rightCaptures, forwardBits + rightBits);

    return pawnTargetSquaresPerOrigin;
}

void generateMovesFromPawnTargetSquares(
        const PawnTargetSquaresPerOrigin& pawnTargetSquaresPerOrigin,
        const std::array<GameState::PieceInfo, kNumTotalPieces>& pieces,
        const Side side,
        const BoardPosition enPassantTarget,
        StackVector<Move>& moves) {
    const int promotionRank = side == Side::White ? 7 : 0;

    const int pawnStartIdx =
            side == Side::White ? (int)PieceIndex::WhitePawn0 : (int)PieceIndex::BlackPawn0;
    const int pawnEndIdx = pawnStartIdx + kNumPawns;
    for (int pawnIdx = pawnStartIdx; pawnIdx < pawnEndIdx; ++pawnIdx) {
        const GameState::PieceInfo& pawnInfo = pieces[pawnIdx];
        if (pawnInfo.captured || getPiece(pawnInfo.coloredPiece) != Piece::Pawn) {
            continue;
        }

        const int originIdx        = (int)pawnInfo.position;
        const int numTargetSquares = pawnTargetSquaresPerOrigin.numSquaresPerOrigin[originIdx];
        for (int targetIdx = 0; targetIdx < numTargetSquares; ++targetIdx) {
            const BoardPosition targetPosition =
                    pawnTargetSquaresPerOrigin.targetSquaresPerOrigin[originIdx][targetIdx];
            MoveFlags flags                     = MoveFlags::None;
            const auto [targetFile, targetRank] = fileRankFromPosition(targetPosition);
            if (fileFromPosition(pawnInfo.position) != targetFile) {
                flags = MoveFlags::IsCapture;
            }
            if (targetPosition == enPassantTarget) {
                flags = getFlags(flags, MoveFlags::IsEnPassant);
            }
            if (targetRank == promotionRank) {
                for (const auto promotionPiece : kPromotionPieces) {
                    moves.emplace_back(
                            (PieceIndex)pawnIdx, targetPosition, getFlags(flags, promotionPiece));
                }
            } else {
                moves.emplace_back((PieceIndex)pawnIdx, targetPosition, flags);
            }
        }
    }
}

// TODO: this can be pre-calculated easily
BitBoard computeKnightControlledSquares(const BoardPosition origin) {
    static constexpr std::array kFileRankDsts = {std::pair{1, 2}, std::pair{2, 1}};

    const auto [file, rank]    = fileRankFromPosition(origin);
    BitBoard controlledSquares = BitBoard::Empty;
    for (const auto [fileDst, rankDst] : kFileRankDsts) {
        for (const auto fileSign : kSigns) {
            for (const auto rankSign : kSigns) {
                const int newFile = file + fileSign * fileDst;
                const int newRank = rank + rankSign * rankDst;

                if (newFile < 0 || newFile > 7 || newRank < 0 || newRank > 7) {
                    continue;
                }
                const BoardPosition newPosition = positionFromFileRank(newFile, newRank);
                set(controlledSquares, newPosition);
            }
        }
    }
    return controlledSquares;
}

BitBoard computeBishopControlledSquares(const BoardPosition origin, const BitBoard anyPiece) {
    const std::uint64_t originBitBoard    = 1ULL << (int)origin;
    const std::uint64_t notOtherPieceMask = ~((std::uint64_t)anyPiece & ~originBitBoard);

    // North-East
    constexpr std::uint64_t notNorthEastMask = notTopRankMask & notRightFileMask;
    std::uint64_t northEastControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        northEastControlledSquares |=
                ((northEastControlledSquares & notOtherPieceMask & notNorthEastMask) << 9);
    }

    // South-East
    constexpr std::uint64_t notSouthEastMask = notBottomRankMask & notRightFileMask;
    std::uint64_t southEastControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        southEastControlledSquares |=
                ((southEastControlledSquares & notOtherPieceMask & notSouthEastMask) >> 7);
    }

    // South-West
    constexpr std::uint64_t notSouthWestMask = notBottomRankMask & notLeftFileMask;
    std::uint64_t southWestControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        southWestControlledSquares |=
                ((southWestControlledSquares & notOtherPieceMask & notSouthWestMask) >> 9);
    }

    // North-West
    constexpr std::uint64_t notNorthWestMask = notTopRankMask & notLeftFileMask;
    std::uint64_t northWestControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        northWestControlledSquares |=
                ((northWestControlledSquares & notOtherPieceMask & notNorthWestMask) << 7);
    }

    return (BitBoard)((northEastControlledSquares | southEastControlledSquares |
                       southWestControlledSquares | northWestControlledSquares) &
                      ~originBitBoard);
}

BitBoard computeRookControlledSquares(const BoardPosition origin, const BitBoard anyPiece) {
    const std::uint64_t originBitBoard    = 1ULL << (int)origin;
    const std::uint64_t notOtherPieceMask = ~((std::uint64_t)anyPiece & ~originBitBoard);

    // North
    constexpr std::uint64_t notNorthMask = notTopRankMask;
    std::uint64_t northControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        northControlledSquares |=
                ((northControlledSquares & notOtherPieceMask & notNorthMask) << 8);
    }

    // East
    constexpr std::uint64_t notEastMask = notRightFileMask;
    std::uint64_t eastControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        eastControlledSquares |= ((eastControlledSquares & notOtherPieceMask & notEastMask) << 1);
    }

    // South
    constexpr std::uint64_t notSouthMask = notBottomRankMask;
    std::uint64_t southControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        southControlledSquares |=
                ((southControlledSquares & notOtherPieceMask & notSouthMask) >> 8);
    }

    // West
    constexpr std::uint64_t notWestMask = notLeftFileMask;
    std::uint64_t westControlledSquares = originBitBoard;
    for (int i = 0; i < 7; ++i) {
        westControlledSquares |= ((westControlledSquares & notOtherPieceMask & notWestMask) >> 1);
    }

    return (BitBoard)((northControlledSquares | eastControlledSquares | southControlledSquares |
                       westControlledSquares) &
                      ~originBitBoard);
}

BitBoard computeQueenControlledSquares(const BoardPosition origin, const BitBoard anyPiece) {
    const BitBoard bishopControlledSquares = computeBishopControlledSquares(origin, anyPiece);
    const BitBoard rookControlledSquares   = computeRookControlledSquares(origin, anyPiece);
    return any(bishopControlledSquares, rookControlledSquares);
}

// TODO: this can be pre-calculated easily
BitBoard computeKingControlledSquares(const BoardPosition origin) {
    const auto [file, rank] = fileRankFromPosition(origin);

    BitBoard controlledSquares = BitBoard::Empty;
    for (int deltaFile = -1; deltaFile <= 1; ++deltaFile) {
        for (int deltaRank = -1; deltaRank <= 1; ++deltaRank) {
            const int newFile = file + deltaFile;
            const int newRank = rank + deltaRank;

            if (newFile < 0 || newFile >= kFiles || newRank < 0 || newRank >= kRanks) {
                continue;
            }
            const BoardPosition newPosition = positionFromFileRank(newFile, newRank);
            set(controlledSquares, newPosition);
        }
    }
    clear(controlledSquares, origin);
    return controlledSquares;
}

BitBoard calculateSinglePieceControl(
        const GameState::PieceInfo& pieceInfo, const BitBoard anyPiece) {
    switch (getPiece(pieceInfo.coloredPiece)) {
        case Piece::Pawn: {
            BitBoard pawnBitBoard = BitBoard::Empty;
            set(pawnBitBoard, pieceInfo.position);
            const Side side = getSide(pieceInfo.coloredPiece);
            return computePawnControlledSquares(pawnBitBoard, side);
        }
        case Piece::Knight:
            return computeKnightControlledSquares(pieceInfo.position);
        case Piece::Bishop:
            return computeBishopControlledSquares(pieceInfo.position, anyPiece);
        case Piece::Rook:
            return computeRookControlledSquares(pieceInfo.position, anyPiece);
        case Piece::Queen:
            return computeQueenControlledSquares(pieceInfo.position, anyPiece);
        case Piece::King:
            return computeKingControlledSquares(pieceInfo.position);
        default:
            UNREACHABLE;
    }
}

void generateCastlingMoves(
        const Side sideToMove,
        const bool canCastleKingSide,
        const bool canCastleQueenSide,
        const PieceOccupationBitBoards& occupation,
        const BitBoard enemyControlledSquares,
        StackVector<Move>& moves) {
    MY_ASSERT(sideToMove == Side::White || sideToMove == Side::Black);

    const BoardPosition kingPosition =
            sideToMove == Side::White ? positionFromAlgebraic("e1") : positionFromAlgebraic("e8");
    const PieceIndex kingIndex = getKingIndex(sideToMove);

    const BitBoard anyPiece = any(occupation.ownPiece, occupation.enemyPiece);

    const bool inCheck = isSet(enemyControlledSquares, kingPosition);
    if (inCheck) {
        // No castle moves are possible while in check
        return;
    }

    const auto [kingFile, kingRank] = fileRankFromPosition(kingPosition);

    if (canCastleKingSide) {
        bool castleIsValid = true;
        for (int fileDelta = 1; fileDelta <= 2; ++fileDelta) {
            const BoardPosition position = positionFromFileRank(kingFile + fileDelta, kingRank);
            if (isSet(anyPiece, position)) {
                // Blocking piece
                castleIsValid = false;
                break;
            }
            if (isSet(enemyControlledSquares, position)) {
                // King passes through or into check
                castleIsValid = false;
                break;
            }
        }

        if (castleIsValid) {
            const BoardPosition targetPosition = positionFromFileRank(kingFile + 2, kingRank);
            moves.emplace_back(kingIndex, targetPosition, MoveFlags::IsCastle);
        }
    }
    if (canCastleQueenSide) {
        bool castleIsValid = true;
        for (int fileDelta = 1; fileDelta <= 3; ++fileDelta) {
            const BoardPosition position = positionFromFileRank(kingFile - fileDelta, kingRank);
            if (isSet(anyPiece, position)) {
                // Blocking piece
                castleIsValid = false;
                break;
            }
            if (fileDelta <= 2 && isSet(enemyControlledSquares, position)) {
                // King passes through or into check
                castleIsValid = false;
                break;
            }
        }

        if (castleIsValid) {
            const BoardPosition targetPosition = positionFromFileRank(kingFile - 2, kingRank);
            moves.emplace_back(kingIndex, targetPosition, MoveFlags::IsCastle);
        }
    }
}

// Can not be used for generating pawn non-captures
void generateSinglePieceMovesFromControl(
        const PieceIndex pieceToMove,
        BitBoard controlledSquares,
        const PieceOccupationBitBoards& occupation,
        const BitBoard piecePinBitBoard,
        StackVector<Move>& moves) {
    // Can't move to our own pieces
    controlledSquares = subtract(controlledSquares, occupation.ownPiece);

    // Pinned pieces can only move along the pin direction
    controlledSquares = intersection(controlledSquares, piecePinBitBoard);

    BitBoard captures    = intersection(controlledSquares, occupation.enemyPiece);
    BitBoard nonCaptures = subtract(controlledSquares, occupation.enemyPiece);

    while (captures != BitBoard::Empty) {
        const BoardPosition capturePosition =
                (BoardPosition)std::countr_zero((std::uint64_t)captures);
        moves.emplace_back(pieceToMove, capturePosition, MoveFlags::IsCapture);
        clear(captures, capturePosition);
    }
    while (nonCaptures != BitBoard::Empty) {
        const BoardPosition movePosition =
                (BoardPosition)std::countr_zero((std::uint64_t)nonCaptures);
        moves.emplace_back(pieceToMove, movePosition);
        clear(nonCaptures, movePosition);
    }
}

constexpr int signum(const int x) {
    return (x > 0) - (x < 0);
}

bool getFileRankIncrement(
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

bool isValidDeltaFileRankForPiece(const int deltaFile, const int deltaRank, Piece piece) {
    if (std::abs(deltaFile) == std::abs(deltaRank)) {
        return piece == Piece::Bishop || piece == Piece::Queen;
    } else {
        return piece == Piece::Rook || piece == Piece::Queen;
    }
}

}  // namespace

GameState GameState::startingPosition() {
    return fromFen(getStartingPositionFen());
}

bool GameState::isInCheck() const {
    return isInCheck(getEnemyControlledSquares());
}

StackVector<Move> GameState::generateMoves(StackOfVectors<Move>& stack) const {
    const BitBoard enemyControlledSquares = getEnemyControlledSquares();

    if (isInCheck(enemyControlledSquares)) {
        return generateMovesInCheck(stack, enemyControlledSquares);
    }

    StackVector<Move> moves = stack.makeStackVector();

    const std::array<BitBoard, kNumPiecesPerSide - 1> pinBitBoards =
            calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard pinBitBoard = calculatePinOrKingAttackBitBoard(pinBitBoards);

    auto getPiecePinBitBoard = [&](BoardPosition position) {
        if (!isSet(pinBitBoard, position)) {
            return BitBoard::Full;
        }
        // Piece is pinned: can only move along the pin direction
        // Find the pinning piece
        const int enemyStartIdx = kNumPiecesPerSide * (int)nextSide(sideToMove_);
        const int enemyEndIdx   = enemyStartIdx + kNumPiecesPerSide - 1;
        for (int enemyPieceIdx = enemyStartIdx; enemyPieceIdx < enemyEndIdx; ++enemyPieceIdx) {
            const int pinIdx = enemyPieceIdx - enemyStartIdx;
            if (isSet(pinBitBoards[pinIdx], position)) {
                return pinBitBoards[pinIdx];
            }
        }
        UNREACHABLE;
    };

    const BoardPosition enPassantTarget =
            enPassantWillPutUsInCheck() ? BoardPosition::Invalid : enPassantTarget_;

    // Generate moves for pawns
    const int ownStartIdx     = kNumPiecesPerSide * (int)sideToMove_;
    const int nonPawnStartIdx = ownStartIdx + kNumNonPawns;
    BitBoard pawnBitBoard     = BitBoard::Empty;
    for (int pawnIdx = ownStartIdx; pawnIdx < nonPawnStartIdx; ++pawnIdx) {
        const PieceInfo& pawnInfo = pieces_[pawnIdx];
        if (pawnInfo.captured || getPiece(pawnInfo.coloredPiece) != Piece::Pawn) {
            continue;
        }
        set(pawnBitBoard, pawnInfo.position);
    }
    const PawnTargetSquaresPerOrigin pawnTargetSquaresPerOrigin = generatePawnTargetSquares(
            pawnBitBoard, sideToMove_, occupation_, enPassantTarget, pinBitBoards, pinBitBoard);
    generateMovesFromPawnTargetSquares(
            pawnTargetSquaresPerOrigin, pieces_, sideToMove_, enPassantTarget, moves);

    // Generate moves for promoted pawns and normal pieces (non-pawns excl. king)
    for (int pieceIdx = ownStartIdx; pieceIdx < nonPawnStartIdx + kNumNonPawns - 1; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured || getPiece(pieceInfo.coloredPiece) == Piece::Pawn) {
            continue;
        }

        const BitBoard piecePinBitBoard = getPiecePinBitBoard(pieceInfo.position);

        generateSinglePieceMovesFromControl(
                (PieceIndex)pieceIdx,
                pieceInfo.controlledSquares,
                occupation_,
                piecePinBitBoard,
                moves);
    }

    // Generate king moves

    // Normal king moves
    const PieceIndex kingIndex     = getKingIndex(sideToMove_);
    BitBoard kingControlledSquares = pieces_[(int)kingIndex].controlledSquares;
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControlledSquares);
    generateSinglePieceMovesFromControl(
            kingIndex,
            kingControlledSquares,
            occupation_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
            moves);

    // Castling moves
    generateCastlingMoves(
            sideToMove_,
            canCastleKingSide(sideToMove_),
            canCastleQueenSide(sideToMove_),
            occupation_,
            enemyControlledSquares,
            moves);

    moves.lock();
    return moves;
}

StackVector<Move> GameState::generateMovesInCheck(
        StackOfVectors<Move>& stack, BitBoard enemyControlledSquares) const {
    StackVector<Move> moves = stack.makeStackVector();

    const PieceIndex kingIndex       = getKingIndex(sideToMove_);
    const BoardPosition kingPosition = getPieceInfo(kingIndex).position;

    PieceIndex checkingPieceIndex       = PieceIndex::Invalid;
    PieceIndex secondCheckingPieceIndex = PieceIndex::Invalid;

    const int enemyPieceStartIdx = kNumPiecesPerSide * (int)nextSide(sideToMove_);
    for (int enemyPieceIdx = enemyPieceStartIdx;
         enemyPieceIdx < enemyPieceStartIdx + kNumPiecesPerSide;
         ++enemyPieceIdx) {
        if (pieces_[enemyPieceIdx].captured) {
            continue;
        }
        if (!isSet(pieces_[enemyPieceIdx].controlledSquares, kingPosition)) {
            continue;
        }
        if (checkingPieceIndex != PieceIndex::Invalid) {
            secondCheckingPieceIndex = (PieceIndex)enemyPieceIdx;
            break;
        }
        checkingPieceIndex = (PieceIndex)enemyPieceIdx;
    }

    MY_ASSERT(checkingPieceIndex != PieceIndex::Invalid);
    const PieceInfo& checkingPieceInfo = getPieceInfo(checkingPieceIndex);

    const std::array<BitBoard, kNumPiecesPerSide - 1> piecePinOrKingAttackBitBoards =
            calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard pinOrKingAttackBitBoard =
            calculatePinOrKingAttackBitBoard(piecePinOrKingAttackBitBoards);

    BitBoard kingAttackBitBoard =
            piecePinOrKingAttackBitBoards[(int)checkingPieceIndex - enemyPieceStartIdx];
    if (secondCheckingPieceIndex != PieceIndex::Invalid) {
        kingAttackBitBoard = any(
                kingAttackBitBoard,
                piecePinOrKingAttackBitBoards[(int)secondCheckingPieceIndex - enemyPieceStartIdx]);
    }

    BitBoard kingControlledSquares = pieces_[(int)kingIndex].controlledSquares;
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControlledSquares);
    kingControlledSquares = subtract(kingControlledSquares, kingAttackBitBoard);
    generateSinglePieceMovesFromControl(
            kingIndex,
            kingControlledSquares,
            occupation_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
            moves);

    if (secondCheckingPieceIndex != PieceIndex::Invalid) {
        // Double check: only the king can move
        moves.lock();
        return moves;
    }

    BitBoard blockOrCaptureBitBoard = BitBoard::Empty;
    if (isPinningPiece(getPiece(checkingPieceInfo.coloredPiece))) {
        const int pinIdx = (int)checkingPieceIndex - enemyPieceStartIdx;
        const BitBoard checkingPieceKingAttackBitBoard = piecePinOrKingAttackBitBoards[pinIdx];
        blockOrCaptureBitBoard =
                intersection(checkingPieceKingAttackBitBoard, checkingPieceInfo.controlledSquares);
    }
    set(blockOrCaptureBitBoard, checkingPieceInfo.position);
    const BitBoard pinBitBoard = subtract(pinOrKingAttackBitBoard, blockOrCaptureBitBoard);

    bool canTakeCheckingPieceEnPassant = false;
    if (enPassantTarget_ != BoardPosition::Invalid) {
        const auto [enPassantFile, enPassantRank] = fileRankFromPosition(enPassantTarget_);
        const int enPassantPieceRank =
                sideToMove_ == Side::White ? enPassantRank - 1 : enPassantRank + 1;
        const BoardPosition enPassantPiecePosition =
                positionFromFileRank(enPassantFile, enPassantPieceRank);
        canTakeCheckingPieceEnPassant = enPassantPiecePosition == checkingPieceInfo.position;
    }
    const BoardPosition enPassantTarget =
            canTakeCheckingPieceEnPassant ? enPassantTarget_ : BoardPosition::Invalid;
    BitBoard pawnBlockOrCaptureBitBoard = blockOrCaptureBitBoard;
    if (canTakeCheckingPieceEnPassant) {
        set(pawnBlockOrCaptureBitBoard, enPassantTarget);
    }

    // Generate pawn moves that either capture the checking piece or block
    const int pieceStartIdx =
            sideToMove_ == Side::White ? (int)PieceIndex::WhitePawn0 : (int)PieceIndex::BlackPawn0;
    const int nonPawnStartIdx = pieceStartIdx + kNumPawns;
    BitBoard pawnBitBoard     = BitBoard::Empty;
    for (int pawnIdx = pieceStartIdx; pawnIdx < nonPawnStartIdx; ++pawnIdx) {
        const PieceInfo& pawnInfo = pieces_[pawnIdx];
        if (pawnInfo.captured || getPiece(pawnInfo.coloredPiece) != Piece::Pawn) {
            continue;
        }
        if (isSet(pinBitBoard, pawnInfo.position)) {
            // Piece is pinned; can't capture pinning piece or remain in pin because that wouldn't
            // resolve the check, so no moves.
            continue;
        }
        set(pawnBitBoard, pawnInfo.position);
    }
#pragma warning(suppress : 4269)
    const std::array<BitBoard, kNumPiecesPerSide - 1> unusedPiecePinBitBoards;  // NOLINT
    PawnTargetSquaresPerOrigin pawnTargetSquaresPerOrigin = generatePawnTargetSquares(
            pawnBitBoard,
            sideToMove_,
            occupation_,
            enPassantTarget,
            unusedPiecePinBitBoards,
            BitBoard::Empty,
            pawnBlockOrCaptureBitBoard);
    generateMovesFromPawnTargetSquares(
            pawnTargetSquaresPerOrigin, pieces_, sideToMove_, enPassantTarget, moves);

    // Generate non-pawn moves that either capture the checking piece or block
    const int pieceEndIdx = pieceStartIdx + kNumPiecesPerSide - 1;  // skip king
    for (int pieceIdx = pieceStartIdx; pieceIdx < pieceEndIdx; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured || getPiece(pieceInfo.coloredPiece) == Piece::Pawn) {
            continue;
        }
        if (isSet(pinBitBoard, pieceInfo.position)) {
            // Piece is pinned; can't capture pinning piece or remain in pin because that wouldn't
            // resolve the check, so no moves.
            continue;
        }

        // Treat blockOrCapture as a pin. This will cause only moves that block or capture to be generated.
        generateSinglePieceMovesFromControl(
                (PieceIndex)pieceIdx,
                pieceInfo.controlledSquares,
                occupation_,
                /*piecePinBitBoard =*/blockOrCaptureBitBoard,
                moves);
    }

    moves.lock();
    return moves;
}

GameState::UnmakeMoveInfo GameState::makeMove(const Move& move) {
    UnmakeMoveInfo unmakeInfo = {
            .from                  = getPieceInfo(move.pieceToMove).position,
            .enPassantTarget       = enPassantTarget_,
            .castlingRights        = castlingRights_,
            .plySinceCaptureOrPawn = plySinceCaptureOrPawn_};

    if (isCastle(move.flags)) {
        makeCastleMove(move);
    } else {
        unmakeInfo.capturedPieceIndex = makeSinglePieceMove(move);
    }

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
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);

    return unmakeInfo;
}

void GameState::unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    enPassantTarget_       = unmakeMoveInfo.enPassantTarget;
    castlingRights_        = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);

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
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);
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

    clear(occupation_.ownPiece, kingFromPosition);
    set(occupation_.ownPiece, kingToPosition);

    clear(occupation_.ownPiece, rookFromPosition);
    set(occupation_.ownPiece, rookToPosition);

    const PieceIndex kingIdx = getKingIndex(sideToMove_);
    PieceInfo& kingPieceInfo = getPieceInfo(kingIdx);

    // TODO: could get rid of this loop if we make the fen parsing smart enough to put rooks at a
    // consistent index if at their starting position. Not sure if this would speed things up.
    PieceIndex rookIdx = PieceIndex::Invalid;
    const int rookStartIdx =
            sideToMove_ == Side::White ? (int)PieceIndex::WhiteRook0 : (int)PieceIndex::BlackRook0;
    for (int pieceIdx = rookStartIdx; pieceIdx < rookStartIdx + 2; ++pieceIdx) {
        PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.position == rookFromPosition) {
            rookIdx = (PieceIndex)pieceIdx;
            break;
        }
    }
    MY_ASSERT(rookIdx != PieceIndex::Invalid);
    PieceInfo& rookPieceInfo = getPieceInfo(rookIdx);

    // Update king
    MY_ASSERT(getPiece(kingPieceInfo.coloredPiece) == Piece::King);
    MY_ASSERT(getSide(kingPieceInfo.coloredPiece) == sideToMove_);
    kingPieceInfo.position = kingToPosition;
    recalculateControlledSquares(kingPieceInfo);

    // Update rook
    MY_ASSERT(getPiece(rookPieceInfo.coloredPiece) == Piece::Rook);
    MY_ASSERT(getSide(rookPieceInfo.coloredPiece) == sideToMove_);
    rookPieceInfo.position = rookToPosition;
    recalculateControlledSquares(rookPieceInfo);

    // Possible optimization here: only rookToPosition needs to be considered.
    std::array<BoardPosition, 4> affectedSquares = {
            kingFromPosition, kingToPosition, rookFromPosition, rookToPosition};
    recalculateControlledSquaresForAffectedSquares(affectedSquares, 4);

    if (!reverse) {
        setCanCastleKingSide(sideToMove_, false);
        setCanCastleQueenSide(sideToMove_, false);

        sideToMove_      = nextSide(sideToMove_);
        enPassantTarget_ = BoardPosition::Invalid;
        ++plySinceCaptureOrPawn_;
        std::swap(occupation_.ownPiece, occupation_.enemyPiece);
    }
}

PieceIndex GameState::makeSinglePieceMove(const Move& move) {
    PieceInfo& movedPieceInfo    = getPieceInfo(move.pieceToMove);
    const BoardPosition moveFrom = movedPieceInfo.position;

    PieceIndex capturedPieceIndex     = PieceIndex::Invalid;
    BoardPosition captureTargetSquare = move.to;

    if (isEnPassant(move.flags)) {
        MY_ASSERT(isCapture(move.flags));
        MY_ASSERT(move.to == enPassantTarget_);

        const auto [fromFile, fromRank] = fileRankFromPosition(moveFrom);
        const auto [toFile, toRank]     = fileRankFromPosition(move.to);
        captureTargetSquare             = positionFromFileRank(toFile, fromRank);
    }

    enPassantTarget_ = BoardPosition::Invalid;

    bool isCaptureOrPawnMove = isCapture(move.flags);

    clear(occupation_.ownPiece, moveFrom);
    set(occupation_.ownPiece, move.to);

    MY_ASSERT(getSide(movedPieceInfo.coloredPiece) == sideToMove_);
    {
        const Piece piece = getPiece(movedPieceInfo.coloredPiece);
        if (piece == Piece::Pawn) {
            isCaptureOrPawnMove = true;
            handlePawnMove(move, moveFrom, movedPieceInfo.coloredPiece);
        } else if (piece == Piece::King) {
            handleNormalKingMove();
        } else if (piece == Piece::Rook) {
            updateRookCastlingRights(moveFrom, sideToMove_);
        }
    }
    movedPieceInfo.position = move.to;
    recalculateControlledSquares(movedPieceInfo);

    if (isCapture(move.flags)) {
        const int enemyStartIdx = kNumPiecesPerSide * (int)nextSide(sideToMove_);
        for (int pieceIdx = enemyStartIdx;; ++pieceIdx) {
            PieceInfo& pieceInfo = pieces_[pieceIdx];
            if (pieceInfo.captured || pieceInfo.position != captureTargetSquare) {
                continue;
            }
            MY_ASSERT(getSide(pieceInfo.coloredPiece) != sideToMove_);
            MY_ASSERT(isCapture(move.flags));

            capturedPieceIndex = (PieceIndex)pieceIdx;

            break;
        }
        MY_ASSERT(capturedPieceIndex != PieceIndex::Invalid);

        PieceInfo& capturedPieceInfo = getPieceInfo(capturedPieceIndex);

        if (getPiece(capturedPieceInfo.coloredPiece) == Piece::Rook) {
            updateRookCastlingRights(captureTargetSquare, getSide(capturedPieceInfo.coloredPiece));
        }
        capturedPieceInfo.captured = true;
        clear(occupation_.enemyPiece, captureTargetSquare);
    }

    std::array<BoardPosition, 4> affectedSquares{};
    int numAffectedSquares                = 0;
    affectedSquares[numAffectedSquares++] = moveFrom;
    affectedSquares[numAffectedSquares++] = move.to;
    if (isEnPassant(move.flags)) {
        affectedSquares[numAffectedSquares++] = captureTargetSquare;
    } else if (isCapture(move.flags)) {
        // The occupancy of the square on which we captured didn't change.
        --numAffectedSquares;
    }
    recalculateControlledSquaresForAffectedSquares(affectedSquares, numAffectedSquares);

    if (isCaptureOrPawnMove) {
        plySinceCaptureOrPawn_ = 0;
    } else {
        ++plySinceCaptureOrPawn_;
    }

    sideToMove_ = nextSide(sideToMove_);
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);

    return capturedPieceIndex;
}

void GameState::unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    set(occupation_.ownPiece, unmakeMoveInfo.from);
    clear(occupation_.ownPiece, move.to);
    if (isCapture(move.flags)) {
        MY_ASSERT(unmakeMoveInfo.capturedPieceIndex != PieceIndex::Invalid);
        set(occupation_.enemyPiece, getPieceInfo(unmakeMoveInfo.capturedPieceIndex).position);
    }

    const int ownStartIdx = kNumPiecesPerSide * (int)sideToMove_;
    for (int pieceIdx = ownStartIdx;; ++pieceIdx) {
        PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured || pieceInfo.position != move.to) {
            continue;
        }
        MY_ASSERT(getSide(pieceInfo.coloredPiece) == sideToMove_);

        pieceInfo.position = unmakeMoveInfo.from;
        if (isPromotion(move.flags)) {
            MY_ASSERT(getPiece(pieceInfo.coloredPiece) == getPromotionPiece(move.flags));
            pieceInfo.coloredPiece = getColoredPiece(Piece::Pawn, sideToMove_);
        }

        recalculateControlledSquares(pieceInfo);

        break;
    }

    std::array<BoardPosition, 4> affectedSquares{};
    int numAffectedSquares                = 0;
    affectedSquares[numAffectedSquares++] = unmakeMoveInfo.from;
    affectedSquares[numAffectedSquares++] = move.to;

    if (isCapture(move.flags)) {
        MY_ASSERT(unmakeMoveInfo.capturedPieceIndex != PieceIndex::Invalid);
        PieceInfo& capturedPieceInfo = getPieceInfo(unmakeMoveInfo.capturedPieceIndex);
        capturedPieceInfo.captured   = false;

        if (isEnPassant(move.flags)) {
            affectedSquares[numAffectedSquares++] = capturedPieceInfo.position;
        } else {
            // The capture square didn't change occupancy.
            --numAffectedSquares;
        }
    }

    recalculateControlledSquaresForAffectedSquares(affectedSquares, numAffectedSquares);
}

void GameState::handlePawnMove(
        const Move& move, BoardPosition moveFrom, ColoredPiece& pieceToMove) {
    const Piece promotionPiece = getPromotionPiece(move.flags);
    if (promotionPiece != Piece::None) {
        pieceToMove = getColoredPiece(promotionPiece, sideToMove_);
    }

    const auto [fromFile, fromRank] = fileRankFromPosition(moveFrom);
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
    if (rookSide == Side::White && rookPosition == positionFromAlgebraic("a1")) {
        setCanCastleQueenSide(rookSide, false);
    } else if (rookSide == Side::White && rookPosition == positionFromAlgebraic("h1")) {
        setCanCastleKingSide(rookSide, false);
    } else if (rookSide == Side::Black && rookPosition == positionFromAlgebraic("a8")) {
        setCanCastleQueenSide(rookSide, false);
    } else if (rookSide == Side::Black && rookPosition == positionFromAlgebraic("h8")) {
        setCanCastleKingSide(rookSide, false);
    }
}

std::array<BitBoard, kNumPiecesPerSide - 1> GameState::calculatePiecePinOrKingAttackBitBoards(
        Side kingSide) const {
    // TODO: can we speed this up using magic bitboards?

    std::array<BitBoard, kNumPiecesPerSide - 1> piecePinOrKingAttackBitBoards{};
    const PieceInfo& kingPieceInfo = getPieceInfo(getKingIndex(kingSide));
    const BitBoard kingSideOccupancy =
            sideToMove_ == kingSide ? occupation_.ownPiece : occupation_.enemyPiece;
    const BitBoard pinningSideOccupancy =
            sideToMove_ == kingSide ? occupation_.enemyPiece : occupation_.ownPiece;

    const int enemyPieceStartIdx =
            kingSide == Side::Black ? (int)PieceIndex::WhitePieces : (int)PieceIndex::BlackPieces;

    for (int pieceIdx = enemyPieceStartIdx; pieceIdx < enemyPieceStartIdx + kNumPiecesPerSide - 1;
         ++pieceIdx) {
        const PieceInfo& pinningPieceInfo = pieces_[pieceIdx];
        const int pinIdx                  = pieceIdx - enemyPieceStartIdx;

        MY_ASSERT(piecePinOrKingAttackBitBoards[pinIdx] == BitBoard::Empty);

        const Piece pinningPiece = getPiece(pinningPieceInfo.coloredPiece);
        if (!isPinningPiece(pinningPiece) || pinningPieceInfo.captured) {
            continue;
        }

        int deltaFile;
        int deltaRank;
        const bool incrementOk = getFileRankIncrement(
                getPiece(pinningPieceInfo.coloredPiece),
                pinningPieceInfo.position,
                kingPieceInfo.position,
                deltaFile,
                deltaRank);
        if (!incrementOk) {
            continue;
        }

        if (!isValidDeltaFileRankForPiece(deltaFile, deltaRank, pinningPiece)) {
            continue;
        }

        auto [file, rank]        = fileRankFromPosition(pinningPieceInfo.position);
        int numKingSidePieces    = 0;
        bool pinningSidePieces   = false;
        bool reachedKing         = false;
        BitBoard pinningBitBoard = BitBoard::Empty;
        while (true) {
            file += deltaFile;
            rank += deltaRank;

            if (file < 0 || file > 7 || rank < 0 || rank > 7) {
                break;
            }

            const BoardPosition position = positionFromFileRank(file, rank);

            if (reachedKing) {
                // This is the x-ray square behind the king. Mark it and stop.
                set(pinningBitBoard, position);
                break;
            }

            if (position == kingPieceInfo.position) {
                reachedKing = true;
                if (numKingSidePieces > 0) {
                    // We've seen a king side piece, so this is a pin. Stop.
                    break;
                }
                // Otherwise, this is a king attack. Continue to mark the 'x-ray' square behind the king.
                continue;
            }

            if (isSet(pinningSideOccupancy, position)) {
                // We haven't reached the king yet, this piece blocks the pin.
                pinningSidePieces = true;
                break;
            }

            if (isSet(kingSideOccupancy, position)) {
                ++numKingSidePieces;
                if (numKingSidePieces > 1) {
                    // If there's more than one king side piece, this can't be a pin, so stop.
                    break;
                }
            }

            set(pinningBitBoard, position);
        }
        if (reachedKing && numKingSidePieces <= 1 && !pinningSidePieces) {
            if (numKingSidePieces != 0) {
                // This is a pin; mark the pinning piece square to allow captures of it.
                set(pinningBitBoard, pinningPieceInfo.position);
            }
            piecePinOrKingAttackBitBoards[pinIdx] = pinningBitBoard;
        }
    }
    return piecePinOrKingAttackBitBoards;
}

BitBoard GameState::calculatePinOrKingAttackBitBoard(
        const std::array<BitBoard, kNumPiecesPerSide - 1>& piecePinOrKingAttackBitBoards) const {
    BitBoard pinOrKingAttackBitBoard = BitBoard::Empty;
    for (BitBoard piecePinOrKingAttackBitBoard : piecePinOrKingAttackBitBoards) {
        pinOrKingAttackBitBoard = any(pinOrKingAttackBitBoard, piecePinOrKingAttackBitBoard);
    }

    return pinOrKingAttackBitBoard;
}

bool GameState::enPassantWillPutUsInCheck() const {
    if (enPassantTarget_ == BoardPosition::Invalid) {
        return false;
    }
    const auto [enPassantTargetFile, enPassantTargetRank] = fileRankFromPosition(enPassantTarget_);
    const int enPassantOriginRank =
            sideToMove_ == Side::White ? enPassantTargetRank - 1 : enPassantTargetRank + 1;

    const BoardPosition kingPosition = getPieceInfo(getKingIndex(sideToMove_)).position;
    const auto [kingFile, kingRank]  = fileRankFromPosition(kingPosition);
    if (kingRank != enPassantOriginRank) {
        return false;
    }

    const bool kingIsLeft = kingFile < enPassantTargetFile;

    int numOwnPawns                     = 0;
    int closestEnemyRookOrQueenDistance = kFiles;
    int closestOtherDistance            = kFiles;
    bool foundKingBlockingPiece         = false;

    // TODO: could use the occupancy bitboards to short-circuit this in some cases

    for (int pieceIdx = 0; pieceIdx < kNumTotalPieces; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
            continue;
        }

        const auto [file, rank] = fileRankFromPosition(pieceInfo.position);
        if (rank != enPassantOriginRank) {
            continue;
        }
        if (file == kingFile || file == enPassantTargetFile) {
            // Skip king and the double-moved pawn
            continue;
        }

        const Side side   = getSide(pieceInfo.coloredPiece);
        const Piece piece = getPiece(pieceInfo.coloredPiece);

        if (side == sideToMove_ && piece == Piece::Pawn &&
            std::abs(file - enPassantTargetFile) == 1) {
            // Found an own pawn neighboring the double-moved pawn
            ++numOwnPawns;
            if (numOwnPawns > 1) {
                // If there's more than one, this can't create a discovered check on the king
                break;
            }
            continue;
        }

        const bool isLeft = file < enPassantTargetFile;
        if (isLeft == kingIsLeft) {
            if (std::abs(file - enPassantTargetFile) < std::abs(kingFile - enPassantTargetFile)) {
                // Found a piece between the king and the double-moved pawn (that isn't an immediately neighboring own pawn)
                // This piece blocks any would-be discovered check
                foundKingBlockingPiece = true;
                break;
            } else {
                // This piece is on the other side of the king, so not relevant.
                continue;
            }
        }

        MY_ASSERT(isLeft != kingIsLeft);
        const int distance = std::abs(file - enPassantTargetFile);

        if (side != sideToMove_ && (piece == Piece::Rook || piece == Piece::Queen)) {
            // An enemy rook or queen on the other side of the king can potentially create a discovered check.
            // Record the closest one.
            if (distance < closestEnemyRookOrQueenDistance) {
                closestEnemyRookOrQueenDistance = distance;
            }
            continue;
        }

        // Piece on the other side of the king that isn't an enemy rook or queen
        if (distance < closestOtherDistance) {
            closestOtherDistance = distance;
        }
    }

    if (numOwnPawns < 2 && closestEnemyRookOrQueenDistance < closestOtherDistance &&
        !foundKingBlockingPiece) {
        // Capturing en passant would put the king in check
        return true;
    }

    return false;
}

// TODO: can we speed this up using magic bitboards?
// And perhaps calculate controlled squares ad-hoc instead of incrementally.
void GameState::recalculateControlledSquaresForAffectedSquares(
        const std::array<BoardPosition, 4>& affectedSquares, const int numAffectedSquares) {
    BitBoard affectedSquaresBitBoard = BitBoard::Empty;
    for (int i = 0; i < numAffectedSquares; ++i) {
        set(affectedSquaresBitBoard, affectedSquares[i]);
    }

    for (auto& pieceInfo : pieces_) {
        if (pieceInfo.captured) {
            continue;
        }

        const Piece piece = getPiece(pieceInfo.coloredPiece);
        const bool controlledSquaresAffected =
                piece != Piece::Pawn && piece != Piece::King && piece != Piece::Knight &&
                (bool)intersection(affectedSquaresBitBoard, pieceInfo.controlledSquares);
        if (!controlledSquaresAffected) {
            continue;
        }

        for (int i = 0; i < numAffectedSquares; ++i) {
            const BoardPosition affectedSquare = affectedSquares[i];
            if (!isSet(pieceInfo.controlledSquares, affectedSquare)) {
                continue;
            }

            int deltaFile;
            int deltaRank;
            const bool deltaFileRankOk = getFileRankIncrement(
                    getPiece(pieceInfo.coloredPiece),
                    pieceInfo.position,
                    affectedSquare,
                    deltaFile,
                    deltaRank);
            MY_ASSERT(deltaFileRankOk);

            const BitBoard anyPiece = any(occupation_.ownPiece, occupation_.enemyPiece);

            const bool isOccupied = isSet(anyPiece, affectedSquare);

            auto [affectedFile, affectedRank] = fileRankFromPosition(affectedSquare);

            if (isOccupied) {
                int file = affectedFile + deltaFile;
                int rank = affectedRank + deltaRank;
                // Affected square is now occupied, but wasn't before.
                // Clear controlled squares starting from the square after the affected square.
                // We stop when we reach a square that was already marked not controlled.
                while (true) {
                    if (file < 0 || file > 7 || rank < 0 || rank > 7) {
                        break;
                    }
                    if (!isSet(pieceInfo.controlledSquares, positionFromFileRank(file, rank))) {
                        break;
                    }
                    clear(pieceInfo.controlledSquares, positionFromFileRank(file, rank));

                    file += deltaFile;
                    rank += deltaRank;
                }
            } else {
                int file = affectedFile;
                int rank = affectedRank;
                // Affected square is now unoccupied, but wasn't before.
                // Set controlled squares starting from the affected square.
                // We stop after reaching a square that is occupied.
                while (true) {
                    set(pieceInfo.controlledSquares, positionFromFileRank(file, rank));

                    if (isSet(anyPiece, positionFromFileRank(file, rank))) {
                        break;
                    }

                    file += deltaFile;
                    rank += deltaRank;

                    if (file < 0 || file > 7 || rank < 0 || rank > 7) {
                        break;
                    }
                }
            }
        }
    }
}

void GameState::recalculateControlledSquares(PieceInfo& pieceInfo) const {
    pieceInfo.controlledSquares = calculateSinglePieceControl(
            pieceInfo, any(occupation_.ownPiece, occupation_.enemyPiece));
}

BitBoard GameState::getEnemyControlledSquares() const {
    BitBoard controlledSquares = BitBoard::Empty;
    const Side enemySide       = nextSide(sideToMove_);
    const int enemyStartIdx    = kNumPiecesPerSide * (int)enemySide;
    for (int pieceIdx = enemyStartIdx; pieceIdx < enemyStartIdx + kNumPiecesPerSide; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
            continue;
        }

        controlledSquares = any(controlledSquares, pieceInfo.controlledSquares);
    }

    return controlledSquares;
}

bool GameState::isInCheck(const BitBoard enemyControlledSquares) const {
    const PieceIndex kingIdx       = getKingIndex(sideToMove_);
    const PieceInfo& kingPieceInfo = getPieceInfo(kingIdx);
    return isSet(enemyControlledSquares, kingPieceInfo.position);
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
