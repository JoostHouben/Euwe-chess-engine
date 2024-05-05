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

void generatePawnMoves(
        const BitBoard pawnBitBoard,
        const Side side,
        const PieceOccupancyBitBoards& occupancy,
        const BoardPosition enPassantTarget,
        const std::array<BitBoard, kNumPiecesPerSide - 1>& piecePinBitBoards,
        const BitBoard pinBitBoard,
        StackVector<Move>& moves,
        const BitBoard checkResolutionBitBoard = BitBoard::Full) {
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

    const int promotionRank = side == Side::White ? 7 : 0;
    const PieceIndex pieceIndex =
            side == Side::White ? PieceIndex::WhitePawn : PieceIndex::BlackPawn;
    auto generateMoves = [&](BitBoard targetBitBoard, const int originOffset, MoveFlags baseFlags) {
        while (targetBitBoard != BitBoard::Empty) {
            const int targetBit = std::countr_zero((std::uint64_t)targetBitBoard);

            const BoardPosition targetPosition = (BoardPosition)targetBit;
            clear(targetBitBoard, targetPosition);

            const int originIdx                = targetBit - originOffset;
            const BoardPosition originPosition = (BoardPosition)originIdx;

            if (isSet(pinBitBoard, originPosition)) {
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
                            pieceIndex,
                            originPosition,
                            targetPosition,
                            getFlags(flags, promotionPiece));
                }
            } else {
                moves.emplace_back(pieceIndex, originPosition, targetPosition, flags);
            }
        }
    };

    generateMoves(singlePushes, forwardBits, MoveFlags::None);
    generateMoves(doublePushes, 2 * forwardBits, MoveFlags::None);

    generateMoves(leftCaptures, forwardBits + leftBits, MoveFlags::IsCapture);
    generateMoves(rightCaptures, forwardBits + rightBits, MoveFlags::IsCapture);
}

constexpr BitBoard computeKnightControlledSquares(const BoardPosition origin) {
    constexpr std::array kFileRankDsts = {std::pair{1, 2}, std::pair{2, 1}};

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

constexpr std::array<BitBoard, kSquares> getKnightControlledSquaresArray() {
    std::array<BitBoard, kSquares> knightControlledSquares = {};
    for (int square = 0; square < kSquares; ++square) {
        knightControlledSquares[square] = computeKnightControlledSquares((BoardPosition)square);
    }
    return knightControlledSquares;
}

constexpr std::array<BitBoard, kSquares> kKnightControlledSquares =
        getKnightControlledSquaresArray();

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

constexpr BitBoard computeKingControlledSquares(const BoardPosition origin) {
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

constexpr std::array<BitBoard, kSquares> getKingControlledSquaresArray() {
    std::array<BitBoard, kSquares> kingControlledSquares = {};
    for (int square = 0; square < kSquares; ++square) {
        kingControlledSquares[square] = computeKingControlledSquares((BoardPosition)square);
    }
    return kingControlledSquares;
}

constexpr std::array<BitBoard, kSquares> kKingControlledSquares = getKingControlledSquaresArray();

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
            return kKnightControlledSquares[(int)pieceInfo.position];
        case Piece::Bishop:
            return computeBishopControlledSquares(pieceInfo.position, anyPiece);
        case Piece::Rook:
            return computeRookControlledSquares(pieceInfo.position, anyPiece);
        case Piece::Queen:
            return computeQueenControlledSquares(pieceInfo.position, anyPiece);
        case Piece::King:
            return kKingControlledSquares[(int)pieceInfo.position];
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
            moves.emplace_back(kingIndex, kingPosition, targetPosition, MoveFlags::IsCastle);
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
            moves.emplace_back(kingIndex, kingPosition, targetPosition, MoveFlags::IsCastle);
        }
    }
}

// Can not be used for generating pawn non-captures
void generateSinglePieceMovesFromControl(
        const PieceIndex pieceToMove,
        const BoardPosition piecePosition,
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
        moves.emplace_back(pieceToMove, piecePosition, capturePosition, MoveFlags::IsCapture);
        clear(captures, capturePosition);
    }
    while (nonCaptures != BitBoard::Empty) {
        const BoardPosition movePosition =
                (BoardPosition)std::countr_zero((std::uint64_t)nonCaptures);
        moves.emplace_back(pieceToMove, piecePosition, movePosition);
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
    generatePawnMoves(
            pawnBitBoards_[(int)sideToMove_],
            sideToMove_,
            occupancy_,
            enPassantTarget,
            pinBitBoards,
            pinBitBoard,
            moves);

    // Generate moves for promoted pawns and normal pieces (non-pawns excl. king)
    const int ownStartIdx = kNumPiecesPerSide * (int)sideToMove_ + 1;
    for (int pieceIdx = ownStartIdx; pieceIdx < ownStartIdx + numNonPawns_[(int)sideToMove_] - 1;
         ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured || getPiece(pieceInfo.coloredPiece) == Piece::Pawn) {
            continue;
        }

        const BitBoard piecePinBitBoard = getPiecePinBitBoard(pieceInfo.position);

        generateSinglePieceMovesFromControl(
                (PieceIndex)pieceIdx,
                pieceInfo.position,
                pieceInfo.controlledSquares,
                occupancy_,
                piecePinBitBoard,
                moves);
    }

    // Generate king moves

    // Normal king moves
    const PieceIndex kingIndex     = getKingIndex(sideToMove_);
    const PieceInfo& kingInfo      = getPieceInfo(kingIndex);
    BitBoard kingControlledSquares = kingInfo.controlledSquares;
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControlledSquares);
    generateSinglePieceMovesFromControl(
            kingIndex,
            kingInfo.position,
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

    BoardPosition checkingPawnPosition = BoardPosition::Invalid;
    bool inCheckByPawn                 = false;

    // TODO: pass this in instead of recalculating
    const BitBoard enemyPawnControl = computePawnControlledSquares(
            pawnBitBoards_[(int)nextSide(sideToMove_)], nextSide(sideToMove_));

    if (isSet(enemyPawnControl, kingPosition)) {
        // King is in check by pawn
        inCheckByPawn = true;

        const auto [kingFile, kingRank]   = fileRankFromPosition(kingPosition);
        const BitBoard& enemyPawnBitBoard = pawnBitBoards_[(int)nextSide(sideToMove_)];
        const int enemyPawnForward        = sideToMove_ == Side::White ? -1 : 1;
        if (kingFile > 0) {
            const BoardPosition leftPawnPosition =
                    positionFromFileRank(kingFile - 1, kingRank - enemyPawnForward);
            if (isSet(enemyPawnBitBoard, leftPawnPosition)) {
                checkingPawnPosition = leftPawnPosition;
            }
        }
        if (kingFile < 7) {
            const BoardPosition rightPawnPosition =
                    positionFromFileRank(kingFile + 1, kingRank - enemyPawnForward);
            if (isSet(enemyPawnBitBoard, rightPawnPosition)) {
                checkingPawnPosition = rightPawnPosition;
            }
        }
        MY_ASSERT(checkingPawnPosition != BoardPosition::Invalid);
    }

    const int enemyPieceStartIdx = kNumPiecesPerSide * (int)nextSide(sideToMove_) + 1;
    const int enemyPieceEndIdx = enemyPieceStartIdx - 1 + numNonPawns_[(int)nextSide(sideToMove_)];
    for (int enemyPieceIdx = enemyPieceStartIdx; enemyPieceIdx < enemyPieceEndIdx;
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
        if (inCheckByPawn) {
            break;
        }
    }
    MY_ASSERT(IMPLIES(!inCheckByPawn, checkingPieceIndex != PieceIndex::Invalid));

    const std::array<BitBoard, kNumPiecesPerSide - 1> piecePinOrKingAttackBitBoards =
            calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard pinOrKingAttackBitBoard =
            calculatePinOrKingAttackBitBoard(piecePinOrKingAttackBitBoards);

    BitBoard kingAttackBitBoard = BitBoard::Empty;
    if (checkingPieceIndex != PieceIndex::Invalid) {
        kingAttackBitBoard =
                piecePinOrKingAttackBitBoards[(int)checkingPieceIndex - enemyPieceStartIdx];
        if (secondCheckingPieceIndex != PieceIndex::Invalid) {
            kingAttackBitBoard =
                    any(kingAttackBitBoard,
                        piecePinOrKingAttackBitBoards
                                [(int)secondCheckingPieceIndex - enemyPieceStartIdx]);
        }
    }

    BitBoard kingControlledSquares = pieces_[(int)kingIndex].controlledSquares;
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControlledSquares);
    kingControlledSquares = subtract(kingControlledSquares, kingAttackBitBoard);
    generateSinglePieceMovesFromControl(
            kingIndex,
            kingPosition,
            kingControlledSquares,
            occupancy_,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */,
            moves);

    const bool doubleCheck = secondCheckingPieceIndex != PieceIndex::Invalid ||
                             (inCheckByPawn && checkingPieceIndex != PieceIndex::Invalid);
    if (doubleCheck) {
        // Double check: only the king can move
        moves.lock();
        return moves;
    }

    BitBoard blockOrCaptureBitBoard = BitBoard::Empty;
    if (inCheckByPawn) {
        set(blockOrCaptureBitBoard, checkingPawnPosition);
    } else {
        const PieceInfo& checkingPieceInfo = getPieceInfo(checkingPieceIndex);
        if (isPinningPiece(getPiece(checkingPieceInfo.coloredPiece))) {
            const int pinIdx = (int)checkingPieceIndex - enemyPieceStartIdx;
            const BitBoard checkingPieceKingAttackBitBoard = piecePinOrKingAttackBitBoards[pinIdx];

            blockOrCaptureBitBoard = intersection(
                    checkingPieceKingAttackBitBoard, checkingPieceInfo.controlledSquares);
        }
        set(blockOrCaptureBitBoard, checkingPieceInfo.position);
    }
    const BitBoard pinBitBoard = subtract(pinOrKingAttackBitBoard, blockOrCaptureBitBoard);

    bool canTakeCheckingPieceEnPassant = false;
    if (inCheckByPawn && enPassantTarget_ != BoardPosition::Invalid) {
        const auto [enPassantFile, enPassantRank] = fileRankFromPosition(enPassantTarget_);
        const int enPassantPieceRank =
                sideToMove_ == Side::White ? enPassantRank - 1 : enPassantRank + 1;
        const BoardPosition enPassantPiecePosition =
                positionFromFileRank(enPassantFile, enPassantPieceRank);
        const BitBoard enPassantPieceBitBoard = (BitBoard)(1ULL << (int)enPassantPiecePosition);
        const BitBoard enPassantPieceAttack =
                computePawnControlledSquares(enPassantPieceBitBoard, nextSide(sideToMove_));
        canTakeCheckingPieceEnPassant = isSet(enPassantPieceAttack, kingPosition);
    }
    const BoardPosition enPassantTarget =
            canTakeCheckingPieceEnPassant ? enPassantTarget_ : BoardPosition::Invalid;
    BitBoard pawnBlockOrCaptureBitBoard = blockOrCaptureBitBoard;
    if (canTakeCheckingPieceEnPassant) {
        set(pawnBlockOrCaptureBitBoard, enPassantTarget);
    }

    // Generate pawn moves that either capture the checking piece or block
#pragma warning(suppress : 4269)
    const std::array<BitBoard, kNumPiecesPerSide - 1> unusedPiecePinBitBoards;  // NOLINT
    const BitBoard nonPinnedPawns = subtract(pawnBitBoards_[(int)sideToMove_], pinBitBoard);
    generatePawnMoves(
            nonPinnedPawns,
            sideToMove_,
            occupancy_,
            enPassantTarget,
            unusedPiecePinBitBoards,
            BitBoard::Empty,
            moves,
            pawnBlockOrCaptureBitBoard);

    // Generate non-pawn moves that either capture the checking piece or block
    const int pieceStartIdx = kNumPiecesPerSide * (int)sideToMove_ + 1;  // skip king
    const int pieceEndIdx   = pieceStartIdx + numNonPawns_[(int)sideToMove_] - 1;
    for (int pieceIdx = pieceStartIdx; pieceIdx < pieceEndIdx; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
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
                pieceInfo.position,
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

    // TODO: could get rid of these loops if we make the fen parsing smart enough to put rooks at a
    // consistent index if at their starting position. Not sure if this would speed things up.
    PieceIndex rookIdx      = PieceIndex::Invalid;
    const int pieceStartIdx = (int)sideToMove_ * kNumPiecesPerSide + 1;  // skip king
    const int pieceEndIdx   = pieceStartIdx + numNonPawns_[(int)sideToMove_] - 1;
    for (int pieceIdx = pieceStartIdx; pieceIdx < pieceEndIdx; ++pieceIdx) {
        PieceInfo& pieceInfo = pieces_[pieceIdx];
        // Don't need to check for captured: if we're allowed to castle here, there can't be a captured
        // rook on rookFromPosition
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
    rookPieceInfo.position          = rookToPosition;
    rookPieceInfo.controlledSquares = BitBoard::Empty;

    // Micro optimization: only control along the back rank can change, and only on the non-castle side.
    // So for recalculating control we only need to consider the square closest to that side of the board.
    // That is the king's origin position (before swapping for reverse).
    std::array<BoardPosition, 3> affectedSquares;
    if (!reverse) {
        affectedSquares[0] = kingFromPosition;
    } else {
        affectedSquares[0] = kingToPosition;
    }
    recalculateControlledSquaresForAffectedSquares(affectedSquares, 1);
    recalculateControlledSquares(rookPieceInfo);

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
    const bool isPawnMove =
            move.pieceToMove == PieceIndex::WhitePawn || move.pieceToMove == PieceIndex::BlackPawn;

    PieceIndex capturedPieceIndex     = PieceIndex::Invalid;
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

        BitBoard& enemyPawnBitBoard = pawnBitBoards_[(int)nextSide(sideToMove_)];
        if (isSet(enemyPawnBitBoard, captureTargetSquare)) {
            capturedPieceIndex =
                    sideToMove_ == Side::White ? PieceIndex::BlackPawn : PieceIndex::WhitePawn;
            clear(enemyPawnBitBoard, captureTargetSquare);
        } else {
            const int enemyStartIdx = kNumPiecesPerSide * (int)nextSide(sideToMove_);
            const int enemyEndIdx   = enemyStartIdx + numNonPawns_[(int)nextSide(sideToMove_)];
            for (int pieceIdx = enemyStartIdx; pieceIdx < enemyEndIdx; ++pieceIdx) {
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
                updateRookCastlingRights(
                        captureTargetSquare, getSide(capturedPieceInfo.coloredPiece));
            }
            capturedPieceInfo.captured = true;
        }
    }

    if (isPawnMove) {
        handlePawnMove(move);
    } else {
        PieceInfo& movedPieceInfo = getPieceInfo(move.pieceToMove);
        const Piece piece         = getPiece(movedPieceInfo.coloredPiece);
        if (piece == Piece::King) {
            handleNormalKingMove();
        } else if (piece == Piece::Rook) {
            updateRookCastlingRights(move.from, sideToMove_);
        }

        movedPieceInfo.position          = move.to;
        movedPieceInfo.controlledSquares = BitBoard::Empty;
    }

    std::array<BoardPosition, 3> affectedSquares{};
    int numAffectedSquares                = 0;
    affectedSquares[numAffectedSquares++] = move.from;
    affectedSquares[numAffectedSquares++] = move.to;
    if (isEnPassant(move.flags)) {
        affectedSquares[numAffectedSquares++] = captureTargetSquare;
    } else if (isCapture(move.flags)) {
        // The occupancy of the square on which we captured didn't change.
        --numAffectedSquares;
    }
    recalculateControlledSquaresForAffectedSquares(affectedSquares, numAffectedSquares);
    if (!isPawnMove) {
        PieceInfo& movedPieceInfo = getPieceInfo(move.pieceToMove);
        recalculateControlledSquares(movedPieceInfo);
    }

    if (isCapture(move.flags) || isPawnMove) {
        plySinceCaptureOrPawn_ = 0;
    } else {
        ++plySinceCaptureOrPawn_;
    }

    sideToMove_ = nextSide(sideToMove_);
    std::swap(occupancy_.ownPiece, occupancy_.enemyPiece);

    return capturedPieceIndex;
}

void GameState::unmakeSinglePieceMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    set(occupancy_.ownPiece, move.from);
    clear(occupancy_.ownPiece, move.to);

    const bool isPawnMove =
            move.pieceToMove == PieceIndex::WhitePawn || move.pieceToMove == PieceIndex::BlackPawn;

    if (isPawnMove) {
        BitBoard& pawnBitBoard = pawnBitBoards_[(int)sideToMove_];
        set(pawnBitBoard, move.from);
        clear(pawnBitBoard, move.to);

        if (isPromotion(move.flags)) {
            numNonPawns_[(int)sideToMove_]--;
            const int promotedPieceIndex =
                    (int)sideToMove_ * kNumPiecesPerSide + numNonPawns_[(int)sideToMove_];
            PieceInfo& promotedPieceInfo = getPieceInfo((PieceIndex)promotedPieceIndex);
            MY_ASSERT(getPiece(promotedPieceInfo.coloredPiece) == getPromotionPiece(move.flags));
            MY_ASSERT(promotedPieceInfo.position == move.to);
            promotedPieceInfo.captured = true;
        }
    } else {
        PieceInfo& movedPieceInfo = getPieceInfo(move.pieceToMove);
        MY_ASSERT(getSide(movedPieceInfo.coloredPiece) == sideToMove_);

        movedPieceInfo.position          = move.from;
        movedPieceInfo.controlledSquares = BitBoard::Empty;
    }

    std::array<BoardPosition, 3> affectedSquares{};
    int numAffectedSquares                = 0;
    affectedSquares[numAffectedSquares++] = move.from;
    affectedSquares[numAffectedSquares++] = move.to;

    if (isCapture(move.flags)) {
        MY_ASSERT(unmakeMoveInfo.capturedPieceIndex != PieceIndex::Invalid);

        BoardPosition captureTarget = move.to;

        const bool capturedPieceIsPawn =
                unmakeMoveInfo.capturedPieceIndex == PieceIndex::WhitePawn ||
                unmakeMoveInfo.capturedPieceIndex == PieceIndex::BlackPawn;
        if (capturedPieceIsPawn) {
            if (isEnPassant(move.flags)) {
                const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
                const auto [toFile, toRank]     = fileRankFromPosition(move.to);
                captureTarget                   = positionFromFileRank(toFile, fromRank);
            }

            BitBoard& enemyPawnBitBoard = pawnBitBoards_[(int)nextSide(sideToMove_)];
            set(enemyPawnBitBoard, captureTarget);

            if (isEnPassant(move.flags)) {
                affectedSquares[numAffectedSquares++] = captureTarget;
            } else {
                // The capture square didn't change occupancy.
                --numAffectedSquares;
            }
        } else {
            PieceInfo& capturedPieceInfo = getPieceInfo(unmakeMoveInfo.capturedPieceIndex);
            capturedPieceInfo.captured   = false;

            // The capture square didn't change occupancy.
            --numAffectedSquares;
        }

        set(occupancy_.enemyPiece, captureTarget);
    }

    recalculateControlledSquaresForAffectedSquares(affectedSquares, numAffectedSquares);
    if (!isPawnMove) {
        recalculateControlledSquares(getPieceInfo(move.pieceToMove));
    }
}

void GameState::handlePawnMove(const Move& move) {
    BitBoard& pawnBitBoard = pawnBitBoards_[(int)sideToMove_];
    clear(pawnBitBoard, move.from);
    set(pawnBitBoard, move.to);

    const Piece promotionPiece = getPromotionPiece(move.flags);
    if (promotionPiece != Piece::None) {
        const int sidePromotionPieceIdx = numNonPawns_[(int)sideToMove_]++;
        const int promotionPieceIdx = sidePromotionPieceIdx + (int)sideToMove_ * kNumPiecesPerSide;

        PieceInfo& promotionPieceInfo        = pieces_[promotionPieceIdx];
        promotionPieceInfo.coloredPiece      = getColoredPiece(promotionPiece, sideToMove_);
        promotionPieceInfo.captured          = false;
        promotionPieceInfo.position          = move.to;
        promotionPieceInfo.controlledSquares = computePieceControlledSquares(
                promotionPieceInfo, any(occupancy_.ownPiece, occupancy_.enemyPiece));

        clear(pawnBitBoard, move.to);
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
        const Side kingSide) const {
    // TODO: can we speed this up using magic bitboards?

    std::array<BitBoard, kNumPiecesPerSide - 1> piecePinOrKingAttackBitBoards{};
    const PieceInfo& kingPieceInfo = getPieceInfo(getKingIndex(kingSide));
    const BitBoard kingSideOccupancy =
            sideToMove_ == kingSide ? occupancy_.ownPiece : occupancy_.enemyPiece;
    const BitBoard pinningSideOccupancy =
            sideToMove_ == kingSide ? occupancy_.enemyPiece : occupancy_.ownPiece;

    const int enemyPieceStartIdx = (int)nextSide(kingSide) * kNumPiecesPerSide + 1;  // skip king
    const int enemyPieceEndIdx   = enemyPieceStartIdx + numNonPawns_[(int)nextSide(kingSide)] - 1;
    for (int pieceIdx = enemyPieceStartIdx; pieceIdx < enemyPieceEndIdx; ++pieceIdx) {
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

    //{
    //    GameState copyState(*this);
    //    const BoardPosition leftPosition =
    //            positionFromFileRank(enPassantTargetFile - 1, enPassantOriginRank);
    //    const BoardPosition rightPosition =
    //            positionFromFileRank(enPassantTargetFile + 1, enPassantOriginRank);
    //    const bool hasLeftPawn = enPassantTargetFile > 0 &&
    //                             isSet(copyState.pawnBitBoards_[(int)sideToMove_], leftPosition);
    //    const bool hasRightPawn = enPassantTargetFile < 7 &&
    //                              isSet(copyState.pawnBitBoards_[(int)sideToMove_], rightPosition);

    //    Move move;
    //    if (hasLeftPawn && hasRightPawn) {
    //        return false;
    //    } else if (hasLeftPawn) {
    //        move = {(PieceIndex)((int)PieceIndex::WhitePawn + (int)sideToMove_),
    //                leftPosition,
    //                enPassantTarget_,
    //                getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)};
    //    } else {
    //        move = {(PieceIndex)((int)PieceIndex::WhitePawn + (int)sideToMove_),
    //                rightPosition,
    //                enPassantTarget_,
    //                getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)};
    //    }
    //    copyState.makeMove(move);
    //    copyState.makeNullMove();
    //    return copyState.isInCheck();
    //}

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
    const BitBoard neighboringPawns =
            intersection(pawnBitBoards_[(int)sideToMove_], nextToEnPassantOriginMask);
    const int numOwnPawns = std::popcount((std::uint64_t)neighboringPawns);

    if (numOwnPawns != 1) {
        // If zero: no en passant capture is possible.
        // If greater than 1: rank would still be blocked after en passant capture
        return false;
    }

    std::uint64_t otherSideRankMask;
    std::uint64_t kingSideRankMask;
    if (kingIsLeft) {
        // Set bits for file > enPassantTargetFile on rank 0
        otherSideRankMask = (0xffULL << (enPassantTargetFile + 1)) & 0xffUL;

        // Set bits for file < enPassantTargetFile on rank 0
        kingSideRankMask = 0xffULL >> (kFiles - enPassantTargetFile);
        // Clear bits for file < kingFile on rank 0
        kingSideRankMask &= ~(0xffULL >> (kFiles - kingFile));
    } else {
        // Set bits for file < enPassantTargetFile on rank 0
        otherSideRankMask = 0xffULL >> (kFiles - enPassantTargetFile);

        // Set bits for file > enPassantTargetFile on rank 0
        kingSideRankMask = (0xffULL << (enPassantTargetFile + 1)) & 0xffUL;
        // Clear bits for file > kingFile on rank 0
        kingSideRankMask &= ~((0xffULL << (kingFile + 1)) & 0xffUL);
    }
    otherSideRankMask <<= enPassantOriginRank * kFiles;
    kingSideRankMask <<= enPassantOriginRank * kFiles;

    const BitBoard enemyPawnsOnOtherSide =
            intersection(pawnBitBoards_[(int)nextSide(sideToMove_)], (BitBoard)otherSideRankMask);
    const BitBoard enemyPawnsOnKingSide =
            intersection(pawnBitBoards_[(int)nextSide(sideToMove_)], (BitBoard)kingSideRankMask);

    if (enemyPawnsOnKingSide != BitBoard::Empty) {
        // Found enemy pawn between the king and the double-moved pawn
        // This piece blocks any would-be discovered check
        return false;
    }

    // Own pawns on the king side that aren't immediately neighboring the double-moved pawn
    const BitBoard ownPawnsOnKingSide = intersection(
            pawnBitBoards_[(int)sideToMove_],
            (BitBoard)(kingSideRankMask & ~((std::uint64_t)nextToEnPassantOriginMask)));
    // Own pawns on the other side that aren't immediately neighboring the double-moved pawn
    const BitBoard ownPawnsOnOtherSide = intersection(
            pawnBitBoards_[(int)sideToMove_],
            (BitBoard)(otherSideRankMask & ~((std::uint64_t)nextToEnPassantOriginMask)));

    if (ownPawnsOnKingSide != BitBoard::Empty) {
        // Found own pawn between the king and the double-moved pawn (that isn't an immediately neighboring own pawn)
        // This piece blocks any would-be discovered check
        return false;
    }

    int closestEnemyRookOrQueenDistance = kFiles;
    int closestOtherDistance            = kFiles;

    const BitBoard pawnsOnOtherSide = any(ownPawnsOnOtherSide, enemyPawnsOnOtherSide);

    if (pawnsOnOtherSide != BitBoard::Empty) {
        if (kingIsLeft) {
            // King is on the LSB side: find lowest set bit
            const int closestPawnPosition = std::countr_zero((std::uint64_t)pawnsOnOtherSide);
            const int closestPawnFile     = fileFromPosition((BoardPosition)closestPawnPosition);
            closestOtherDistance          = closestPawnFile - enPassantTargetFile;
        } else {
            // King is on the MSB side: find highest set bit
            const int closestPawnPosition =
                    kSquares - 1 - std::countl_zero((std::uint64_t)pawnsOnOtherSide);
            const int closestPawnFile = fileFromPosition((BoardPosition)closestPawnPosition);
            closestOtherDistance      = enPassantTargetFile - closestPawnFile;
        }
        MY_ASSERT(closestOtherDistance > 0);
    }

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
        if (file == kingFile) {
            // Skip king and the double-moved pawn
            continue;
        }

        const Side side   = getSide(pieceInfo.coloredPiece);
        const Piece piece = getPiece(pieceInfo.coloredPiece);

        const bool isLeft = file < enPassantTargetFile;
        if (isLeft == kingIsLeft) {
            if (std::abs(file - enPassantTargetFile) < std::abs(kingFile - enPassantTargetFile)) {
                // Found a piece between the king and the double-moved pawn (that isn't an immediately neighboring own pawn)
                // This piece blocks any would-be discovered check
                return false;
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

    if (numOwnPawns < 2 && closestEnemyRookOrQueenDistance < closestOtherDistance) {
        // Capturing en passant would put the king in check
        return true;
    }

    return false;
}

// TODO: can we speed this up using magic bitboards?
// And perhaps calculate controlled squares ad-hoc instead of incrementally.
void GameState::recalculateControlledSquaresForAffectedSquares(
        const std::array<BoardPosition, 3>& affectedSquares, const int numAffectedSquares) {
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
    const Side enemySide = nextSide(sideToMove_);

    BitBoard controlledSquares =
            computePawnControlledSquares(pawnBitBoards_[(int)enemySide], enemySide);

    const int enemyStartIdx = kNumPiecesPerSide * (int)enemySide;
    const int enemyEndIdx   = enemyStartIdx + numNonPawns_[(int)enemySide];
    for (int pieceIdx = enemyStartIdx; pieceIdx < enemyEndIdx; ++pieceIdx) {
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
