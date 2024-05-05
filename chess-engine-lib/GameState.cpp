#include "GameState.h"

#include "MyAssert.h"

#include <array>
#include <bit>

#define IMPLIES(a, b) (!(a) || (b))

namespace {

constexpr std::array kSigns = {+1, -1};

static constexpr std::array kPromotionPieces = {
        Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen};

constexpr std::uint64_t notWestFileMask = ~0x0101010101010101ULL;
constexpr std::uint64_t notEastFileMask = ~0x8080808080808080ULL;
constexpr std::uint64_t allMask         = ~0ULL;

BitBoard computePawnControlledSquares(const BitBoard pawnBitBoard, const Side side) {
    auto forwardShift = [=](const BitBoard bitBoard) {
        return side == Side::White ? (BitBoard)((std::uint64_t)bitBoard << 8)
                                   : (BitBoard)((std::uint64_t)bitBoard >> 8);
    };
    auto leftShift = [=](const BitBoard bitBoard) {
        return (BitBoard)(((std::uint64_t)bitBoard & notWestFileMask) >> 1);
    };
    auto rightShift = [=](const BitBoard bitBoard) {
        return (BitBoard)(((std::uint64_t)bitBoard & notEastFileMask) << 1);
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
        const PieceOccupancyBitBoards& occupancy,
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
        return (BitBoard)(((std::uint64_t)bitBoard & notWestFileMask) >> 1);
    };
    auto rightShift = [=](const BitBoard bitBoard) {
        return (BitBoard)(((std::uint64_t)bitBoard & notEastFileMask) << 1);
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

template <int N>
constexpr std::uint64_t shift(const std::uint64_t x) {
    if constexpr (N > 0) {
        return x << N;
    } else {
        return x >> -N;
    }
}

template <int N>
constexpr std::uint64_t fillRayNoMask(std::uint64_t seed, const std::uint64_t blockerMask) {
    std::uint64_t result = seed;

    for (int i = 0; i < 6; ++i) {
        seed = shift<N>(seed) & blockerMask;
        result |= seed;
    }
    result |= shift<N>(seed) & blockerMask;

    return shift<N>(result);
}

template <int N, std::uint64_t directionMask>
constexpr std::uint64_t fillRayWithMask(std::uint64_t seed, std::uint64_t blockerMask) {
    blockerMask &= directionMask;

    std::uint64_t result = seed;

    for (int i = 0; i < 6; ++i) {
        seed = shift<N>(seed) & blockerMask;
        result |= seed;
    }
    result |= shift<N>(seed) & blockerMask;

    return shift<N>(result) & directionMask;
}

template <int N, std::uint64_t directionMask>
constexpr std::array<std::uint64_t, kSquares> getFullRays() {
    std::array<std::uint64_t, kSquares> rays = {};

    for (int file = 0; file < kFiles; ++file) {
        for (int rank = 0; rank < kRanks; ++rank) {
            const BoardPosition position = positionFromFileRank(file, rank);
            rays[(int)position] = fillRayWithMask<N, directionMask>(1ULL << (int)position, allMask);
        }
    }

    return rays;
}

static constexpr int kNorth = 8;
static constexpr int kEast  = 1;
static constexpr int kSouth = -8;
static constexpr int kWest  = -1;

static constexpr int kNorthEast = kNorth + kEast;
static constexpr int kSouthEast = kSouth + kEast;
static constexpr int kSouthWest = kSouth + kWest;
static constexpr int kNorthWest = kNorth + kWest;

constexpr std::array<std::uint64_t, kSquares> fullNRays = getFullRays<kNorth, allMask>();
constexpr std::array<std::uint64_t, kSquares> fullERays = getFullRays<kEast, notWestFileMask>();
constexpr std::array<std::uint64_t, kSquares> fullSRays = getFullRays<kSouth, allMask>();
constexpr std::array<std::uint64_t, kSquares> fullWRays = getFullRays<kWest, notEastFileMask>();

constexpr std::array<std::uint64_t, kSquares> fullNERays =
        getFullRays<kNorthEast, notWestFileMask>();
constexpr std::array<std::uint64_t, kSquares> fullSERays =
        getFullRays<kSouthEast, notWestFileMask>();
constexpr std::array<std::uint64_t, kSquares> fullSWRays =
        getFullRays<kSouthWest, notEastFileMask>();
constexpr std::array<std::uint64_t, kSquares> fullNWRays =
        getFullRays<kNorthWest, notEastFileMask>();

constexpr std::uint64_t getFullRay(
        const BoardPosition position, const int fileIncrement, const int rankIncrement) {
    switch (fileIncrement) {
        // West
        case -1: {
            switch (rankIncrement) {
                case -1:
                    return fullSWRays[(int)position];
                case 0:
                    return fullWRays[(int)position];
                case 1:
                    return fullNWRays[(int)position];
                default:
                    UNREACHABLE;
            }
        }
        // North/South
        case 0: {
            switch (rankIncrement) {
                case -1:
                    return fullSRays[(int)position];
                case 1:
                    return fullNRays[(int)position];
                default:
                    // Stationary: not allowed
                    UNREACHABLE;
            }
        }
        // East
        case 1: {
            switch (rankIncrement) {
                case -1:
                    return fullSERays[(int)position];
                case 0:
                    return fullERays[(int)position];
                case 1:
                    return fullNERays[(int)position];
                default:
                    UNREACHABLE;
            }
        }
        default:
            UNREACHABLE;
    }
}

constexpr std::uint64_t getFillRay(
        const BoardPosition origin,
        const BitBoard anyPiece,
        const int fileIncrement,
        const int rankIncrement) {
    const std::uint64_t originBitBoard    = 1ULL << (int)origin;
    const std::uint64_t notOtherPieceMask = ~((std::uint64_t)anyPiece);

    switch (fileIncrement) {
        // West
        case -1: {
            switch (rankIncrement) {
                case -1:
                    return fillRayWithMask<kSouthWest, notEastFileMask>(
                            originBitBoard, notOtherPieceMask);
                case 0:
                    return fillRayWithMask<kWest, notEastFileMask>(
                            originBitBoard, notOtherPieceMask);
                case 1:
                    return fillRayWithMask<kNorthWest, notEastFileMask>(
                            originBitBoard, notOtherPieceMask);
                default:
                    UNREACHABLE;
            }
        }
        // North/South
        case 0: {
            switch (rankIncrement) {
                case -1:
                    return fillRayNoMask<kSouth>(originBitBoard, notOtherPieceMask);
                case 1:
                    return fillRayNoMask<kNorth>(originBitBoard, notOtherPieceMask);
                default:
                    // Stationary: not allowed
                    UNREACHABLE;
            }
        }
        // East
        case 1: {
            switch (rankIncrement) {
                case -1:
                    return fillRayWithMask<kSouthEast, notWestFileMask>(
                            originBitBoard, notOtherPieceMask);
                case 0:
                    return fillRayWithMask<kEast, notWestFileMask>(
                            originBitBoard, notOtherPieceMask);
                case 1:
                    return fillRayWithMask<kNorthEast, notWestFileMask>(
                            originBitBoard, notOtherPieceMask);
                default:
                    UNREACHABLE;
            }
        }
        default:
            UNREACHABLE;
    }
}

BitBoard computeBishopControlledSquares(const BoardPosition origin, const BitBoard anyPiece) {
    const std::uint64_t originBitBoard    = 1ULL << (int)origin;
    const std::uint64_t notOtherPieceMask = ~((std::uint64_t)anyPiece);

    const std::uint64_t northEastControlledSquares =
            fillRayWithMask<9, notWestFileMask>(originBitBoard, notOtherPieceMask);
    const std::uint64_t southEastControlledSquares =
            fillRayWithMask<-7, notWestFileMask>(originBitBoard, notOtherPieceMask);
    const std::uint64_t southWestControlledSquares =
            fillRayWithMask<-9, notEastFileMask>(originBitBoard, notOtherPieceMask);
    const std::uint64_t northWestControlledSquares =
            fillRayWithMask<7, notEastFileMask>(originBitBoard, notOtherPieceMask);

    return (BitBoard)(northEastControlledSquares | southEastControlledSquares |
                      southWestControlledSquares | northWestControlledSquares);
}

BitBoard computeRookControlledSquares(const BoardPosition origin, const BitBoard anyPiece) {
    const std::uint64_t originBitBoard    = 1ULL << (int)origin;
    const std::uint64_t notOtherPieceMask = ~((std::uint64_t)anyPiece);

    const std::uint64_t northControlledSquares =
            fillRayNoMask<8>(originBitBoard, notOtherPieceMask);
    const std::uint64_t eastControlledSquares =
            fillRayWithMask<1, notWestFileMask>(originBitBoard, notOtherPieceMask);
    const std::uint64_t southControlledSquares =
            fillRayNoMask<-8>(originBitBoard, notOtherPieceMask);
    const std::uint64_t westControlledSquares =
            fillRayWithMask<-1, notEastFileMask>(originBitBoard, notOtherPieceMask);

    return (BitBoard)(northControlledSquares | eastControlledSquares | southControlledSquares |
                      westControlledSquares);
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

BitBoard computePieceControlledSquares(
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
        const PieceOccupancyBitBoards& occupancy,
        const BitBoard enemyControlledSquares,
        StackVector<Move>& moves) {
    MY_ASSERT(sideToMove == Side::White || sideToMove == Side::Black);

    const BoardPosition kingPosition =
            sideToMove == Side::White ? positionFromAlgebraic("e1") : positionFromAlgebraic("e8");
    const PieceIndex kingIndex = getKingIndex(sideToMove);

    const BitBoard anyPiece = any(occupancy.ownPiece, occupancy.enemyPiece);

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
        const PieceOccupancyBitBoards& occupancy,
        const BitBoard piecePinBitBoard,
        StackVector<Move>& moves) {
    // Can't move to our own pieces
    controlledSquares = subtract(controlledSquares, occupancy.ownPiece);

    // Pinned pieces can only move along the pin direction
    controlledSquares = intersection(controlledSquares, piecePinBitBoard);

    BitBoard captures    = intersection(controlledSquares, occupancy.enemyPiece);
    BitBoard nonCaptures = subtract(controlledSquares, occupancy.enemyPiece);

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
            pawnBitBoard, sideToMove_, occupancy_, enPassantTarget, pinBitBoards, pinBitBoard);
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
                occupancy_,
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
            occupancy_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
            moves);

    // Castling moves
    generateCastlingMoves(
            sideToMove_,
            canCastleKingSide(sideToMove_),
            canCastleQueenSide(sideToMove_),
            occupancy_,
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
            occupancy_,
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
            occupancy_,
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
                occupancy_,
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
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);

    return unmakeInfo;
}

void GameState::unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_            = nextSide(sideToMove_);
    enPassantTarget_       = unmakeMoveInfo.enPassantTarget;
    castlingRights_        = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);

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
        std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);
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

    clear(occupancy_.ownPiece, moveFrom);
    set(occupancy_.ownPiece, move.to);

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
        clear(occupancy_.enemyPiece, captureTargetSquare);
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
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);

    return capturedPieceIndex;
}

void GameState::unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    set(occupancy_.ownPiece, unmakeMoveInfo.from);
    clear(occupancy_.ownPiece, move.to);
    if (isCapture(move.flags)) {
        MY_ASSERT(unmakeMoveInfo.capturedPieceIndex != PieceIndex::Invalid);
        set(occupancy_.enemyPiece, getPieceInfo(unmakeMoveInfo.capturedPieceIndex).position);
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
            sideToMove_ == kingSide ? occupancy_.ownPiece : occupancy_.enemyPiece;
    const BitBoard pinningSideOccupancy =
            sideToMove_ == kingSide ? occupancy_.enemyPiece : occupancy_.ownPiece;

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

    const BitBoard anyPiece = any(occupancy_.ownPiece, occupancy_.enemyPiece);

    for (auto& pieceInfo : pieces_) {
        if (pieceInfo.captured) {
            continue;
        }

        const Piece piece = getPiece(pieceInfo.coloredPiece);
        const bool controlledSquaresAffected =
                isPinningPiece(piece) &&
                (bool)intersection(affectedSquaresBitBoard, pieceInfo.controlledSquares);
        if (!controlledSquaresAffected) {
            continue;
        }

        for (int i = 0; i < numAffectedSquares; ++i) {
            const BoardPosition affectedSquare = affectedSquares[i];
            if (!isSet(pieceInfo.controlledSquares, affectedSquare)) {
                continue;
            }

            int fileIncrement;
            int rankIncrement;
            const bool deltaFileRankOk = getFileRankIncrement(
                    getPiece(pieceInfo.coloredPiece),
                    pieceInfo.position,
                    affectedSquare,
                    fileIncrement,
                    rankIncrement);
            MY_ASSERT(deltaFileRankOk);

            const bool isOccupied = isSet(anyPiece, affectedSquare);

            if (isOccupied) {
                // Clear what's behind the affected square from the controlledSquares
                const std::uint64_t fullRayBehind =
                        getFullRay(affectedSquare, fileIncrement, rankIncrement);
                pieceInfo.controlledSquares =
                        subtract(pieceInfo.controlledSquares, (BitBoard)fullRayBehind);
            } else {
                // Recalculate the ray from the piece in the direction of the affected square
                const std::uint64_t ray =
                        getFillRay(pieceInfo.position, anyPiece, fileIncrement, rankIncrement);
                pieceInfo.controlledSquares = any(pieceInfo.controlledSquares, (BitBoard)ray);
            }
        }
    }
}

void GameState::recalculateControlledSquares(PieceInfo& pieceInfo) const {
    pieceInfo.controlledSquares = computePieceControlledSquares(
            pieceInfo, any(occupancy_.ownPiece, occupancy_.enemyPiece));
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
