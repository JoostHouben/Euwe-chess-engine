#include "GameState.h"

#include "MyAssert.h"

#include <array>
#include <bit>

#define IMPLIES(a, b) (!(a) || (b))

namespace {

constexpr std::array kSigns = {+1, -1};
static constexpr std::array kPromotionPieces = {
        Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen};

template <typename FuncT>
void generatePawnCapturesOnTarget(
        const BoardPosition target,
        const PieceIndex pieceToMove,
        FuncT&& addMove,
        const bool getControlledSquares = false) {
    const auto [_, newRank] = fileRankFromPosition(target);
    if (!getControlledSquares && (newRank == 0 || newRank == 7)) {
        // Capture + promotion
        for (const auto promotionPiece : kPromotionPieces) {
            addMove(Move{pieceToMove, target, getFlags(MoveFlags::IsCapture, promotionPiece)});
        }
    } else {
        // Normal capture
        addMove(Move{pieceToMove, target, MoveFlags::IsCapture});
    }
}

template <typename FuncT>
void generateSinglePawnMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const BoardPosition enPassantTarget,
        const Side side,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares,
        BitBoard piecePinBitBoard = BitBoard::Full) {
    const auto [file, rank] = fileRankFromPosition(origin);

    const int forwardDirection = side == Side::White ? 1 : -1;
    const int startingRank = side == Side::White ? 1 : 6;

    const int newRank = rank + forwardDirection;

    const BitBoard anyPiece = any(occupation.ownPiece, occupation.enemyPiece);

    // Push pawn
    // Skip this if getControlledSquares: pawns don't control squares they can push to
    if (!getControlledSquares) {
        const BoardPosition forwardPosition = positionFromFileRank(file, newRank);
        const bool allowedForPin = isSet(piecePinBitBoard, forwardPosition);
        if (allowedForPin && !isSet(anyPiece, forwardPosition)) {
            if (newRank == 0 || newRank == 7) {
                // Promotion
                for (const auto promotionPiece : kPromotionPieces) {
                    addMove(Move{pieceToMove, forwardPosition, getFlags(promotionPiece)});
                }
            } else {
                // Normal push
                addMove(Move{pieceToMove, forwardPosition});
            }

            // Double push
            if (rank == startingRank) {
                const BoardPosition doubleForwardPosition =
                        positionFromFileRank(file, rank + 2 * forwardDirection);
                if (!isSet(anyPiece, doubleForwardPosition)) {
                    addMove(Move{pieceToMove, doubleForwardPosition});
                }
                // No need to check for promotion: this can never happen from the starting rank
            }
        }
    }

    // Captures
    for (int fileDelta : {-1, 1}) {
        const int newFile = file + fileDelta;
        if (newFile < 0 || newFile > 7) {
            // Can't go off the side
            continue;
        }

        // En passant capture
        // No need to check for a blocking piece: the en passant target square is always empty
        // No need to check for promotion: this can never happen on an en passant target square
        const BoardPosition capturePosition = positionFromFileRank(newFile, newRank);
        if (!isSet(piecePinBitBoard, capturePosition)) {
            // Piece is pinned: only moves along the pinning direction are allowed
            continue;
        }
        if (capturePosition == enPassantTarget) {
            addMove(
                    Move{pieceToMove,
                         capturePosition,
                         getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)});
            continue;
        }

        const bool isEnemyPiece = isSet(occupation.enemyPiece, capturePosition);
        if (isEnemyPiece || getControlledSquares) {
            generatePawnCapturesOnTarget(
                    capturePosition, pieceToMove, addMove, getControlledSquares);
        }
    }
}

template <typename FuncT>
void generateSingleKnightMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares) {
    static constexpr std::array kFileRankDsts = {std::pair{1, 2}, std::pair{2, 1}};

    const auto [file, rank] = fileRankFromPosition(origin);

    for (const auto [fileDst, rankDst] : kFileRankDsts) {
        for (const auto fileSign : kSigns) {
            for (const auto rankSign : kSigns) {
                const int newFile = file + fileSign * fileDst;
                const int newRank = rank + rankSign * rankDst;

                if (newFile < 0 || newFile > 7 || newRank < 0 || newRank > 7) {
                    continue;
                }
                const BoardPosition newPosition = positionFromFileRank(newFile, newRank);

                const bool isOwn = isSet(occupation.ownPiece, newPosition);
                const bool isEnemyPiece = isSet(occupation.enemyPiece, newPosition);
                const bool isOccupied = isOwn || isEnemyPiece;
                if (!isOwn || getControlledSquares) {
                    const MoveFlags flags = isOccupied ? MoveFlags::IsCapture : MoveFlags::None;
                    addMove({pieceToMove, newPosition, flags});
                }
            }
        }
    }
}

template <typename FuncT>
void generateSliderMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        const int deltaFile,
        const int deltaRank,
        const bool isKing,
        FuncT&& addMove,
        const bool getControlledSquares,
        BitBoard enemyControlledSquares,
        BitBoard piecePinBitBoard) {
    const auto [file, rank] = fileRankFromPosition(origin);

    const BoardPosition firstPosition = positionFromFileRank(file + deltaFile, rank + deltaRank);
    if (!isSet(piecePinBitBoard, firstPosition)) {
        return;
    }

    int newFile = file;
    int newRank = rank;
    do {
        newFile += deltaFile;
        newRank += deltaRank;

        if (newFile < 0 || newFile > 7 || newRank < 0 || newRank > 7) {
            break;
        }

        const BoardPosition newPosition = positionFromFileRank(newFile, newRank);
        const bool isOwn = isSet(occupation.ownPiece, newPosition);
        const bool isEnemyPiece = isSet(occupation.enemyPiece, newPosition);

        if (!getControlledSquares && isKing && isSet(enemyControlledSquares, newPosition)) {
            // King can't move into check
            break;
        }

        if (isOwn && !getControlledSquares) {
            // Further moves are blocked
            break;
        }

        if (isEnemyPiece || (isOwn && getControlledSquares)) {
            // Capture
            addMove({pieceToMove, newPosition, MoveFlags::IsCapture});
            // Further moves are blocked
            break;
        }

        // Normal move
        addMove({pieceToMove, newPosition});

        if (isKing) {
            // King only moves a distance of 1
            break;
        }
    } while (true);
}

template <typename FuncT>
void generateSingleBishopMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares,
        BitBoard piecePinBitBoard) {
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(
                    origin,
                    pieceToMove,
                    occupation,
                    deltaFile,
                    deltaRank,
                    false,
                    addMove,
                    getControlledSquares,
                    BitBoard::Empty,
                    piecePinBitBoard);
        }
    }
}

template <typename FuncT>
void generateSingleRookMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares,
        BitBoard piecePinBitBoard) {
    for (const auto sign : kSigns) {
        generateSliderMoves(
                origin,
                pieceToMove,
                occupation,
                sign,
                0,
                false,
                addMove,
                getControlledSquares,
                BitBoard::Empty,
                piecePinBitBoard);
        generateSliderMoves(
                origin,
                pieceToMove,
                occupation,
                0,
                sign,
                false,
                addMove,
                getControlledSquares,
                BitBoard::Empty,
                piecePinBitBoard);
    }
}

template <typename FuncT>
void generateSingleQueenMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares,
        BitBoard piecePinBitBoard) {
    // Diagonals
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(
                    origin,
                    pieceToMove,
                    occupation,
                    deltaFile,
                    deltaRank,
                    false,
                    addMove,
                    getControlledSquares,
                    BitBoard::Empty,
                    piecePinBitBoard);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(
                origin,
                pieceToMove,
                occupation,
                sign,
                0,
                false,
                addMove,
                getControlledSquares,
                BitBoard::Empty,
                piecePinBitBoard);
        generateSliderMoves(
                origin,
                pieceToMove,
                occupation,
                0,
                sign,
                false,
                addMove,
                getControlledSquares,
                BitBoard::Empty,
                piecePinBitBoard);
    }
}

template <typename FuncT>
void generateNormalKingMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares,
        BitBoard enemyControlledSquares) {
    // Diagonals
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(
                    origin,
                    pieceToMove,
                    occupation,
                    deltaFile,
                    deltaRank,
                    true,
                    addMove,
                    getControlledSquares,
                    enemyControlledSquares,
                    BitBoard::Full);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(
                origin,
                pieceToMove,
                occupation,
                sign,
                0,
                true,
                addMove,
                getControlledSquares,
                enemyControlledSquares,
                BitBoard::Full);
        generateSliderMoves(
                origin,
                pieceToMove,
                occupation,
                0,
                sign,
                true,
                addMove,
                getControlledSquares,
                enemyControlledSquares,
                BitBoard::Full);
    }
}

template <typename FuncT>
void generateCastlingMoves(
        const Side sideToMove,
        const bool canCastleKingSide,
        const bool canCastleQueenSide,
        const PieceOccupationBitBoards& occupation,
        const BitBoard enemyControlledSquares,
        FuncT&& addMove) {
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
            addMove({kingIndex, targetPosition, MoveFlags::IsCastle});
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
            addMove({kingIndex, targetPosition, MoveFlags::IsCastle});
        }
    }
}

template <typename FuncT>
void generateSinglePieceMoves(
        const GameState::PieceInfo& pieceInfo,
        const PieceIndex pieceToMove,
        const BoardPosition enPassantTarget,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares = false,
        BitBoard enemyControlledSquares = BitBoard::Empty,
        BitBoard piecePinBitBoard = BitBoard::Full) {
    switch (getPiece(pieceInfo.coloredPiece)) {
        case Piece::Pawn:
            generateSinglePawnMoves(
                    pieceInfo.position,
                    pieceToMove,
                    enPassantTarget,
                    getSide(pieceInfo.coloredPiece),
                    occupation,
                    addMove,
                    getControlledSquares,
                    piecePinBitBoard);
            break;
        case Piece::Knight:
            if (piecePinBitBoard != BitBoard::Full) {
                // Pinned knights can't move, because they can't move in the pin direction
                return;
            }
            generateSingleKnightMoves(
                    pieceInfo.position, pieceToMove, occupation, addMove, getControlledSquares);
            break;
        case Piece::Bishop:
            generateSingleBishopMoves(
                    pieceInfo.position,
                    pieceToMove,
                    occupation,
                    addMove,
                    getControlledSquares,
                    piecePinBitBoard);
            break;
        case Piece::Rook:
            generateSingleRookMoves(
                    pieceInfo.position,
                    pieceToMove,
                    occupation,
                    addMove,
                    getControlledSquares,
                    piecePinBitBoard);
            break;
        case Piece::Queen:
            generateSingleQueenMoves(
                    pieceInfo.position,
                    pieceToMove,
                    occupation,
                    addMove,
                    getControlledSquares,
                    piecePinBitBoard);
            break;
        case Piece::King:
            generateNormalKingMoves(
                    pieceInfo.position,
                    pieceToMove,
                    occupation,
                    addMove,
                    getControlledSquares,
                    enemyControlledSquares);
            break;
        default:
            UNREACHABLE;
    }
}

// Can not be used for generating pawn non-captures
template <typename FuncT>
void generateSinglePieceMovesFromControl(
        const PieceIndex pieceToMove,
        BitBoard controlledSquares,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const BitBoard piecePinBitBoard) {
    // Can't move to our own pieces
    controlledSquares = subtract(controlledSquares, occupation.ownPiece);

    // Pinned pieces can only move along the pin direction
    controlledSquares = intersection(controlledSquares, piecePinBitBoard);

    BitBoard captures = intersection(controlledSquares, occupation.enemyPiece);
    BitBoard nonCaptures = subtract(controlledSquares, occupation.enemyPiece);

    while (true) {
        const BoardPosition capturePosition =
                (BoardPosition)std::countr_zero((std::uint64_t)captures);
        if (capturePosition == BoardPosition::Invalid) {
            break;
        }
        addMove({pieceToMove, capturePosition, MoveFlags::IsCapture});
        clear(captures, capturePosition);
    }
    while (true) {
        const BoardPosition movePosition =
                (BoardPosition)std::countr_zero((std::uint64_t)nonCaptures);
        if (movePosition == BoardPosition::Invalid) {
            break;
        }
        addMove({pieceToMove, movePosition});
        clear(nonCaptures, movePosition);
    }
}

bool getDeltaFileRank(
        const BoardPosition from, const BoardPosition to, int& deltaFile, int& deltaRank) {
    const auto [fromFile, fromRank] = fileRankFromPosition(from);
    const auto [toFile, toRank] = fileRankFromPosition(to);
    const int numSteps = std::max(std::abs(toFile - fromFile), std::abs(toRank - fromRank));
    deltaFile = (toFile - fromFile) / numSteps;
    int fileRemainder = (toFile - fromFile) % numSteps;
    deltaRank = (toRank - fromRank) / numSteps;
    int rankRemainder = (toRank - fromRank) % numSteps;

    return fileRemainder == 0 && rankRemainder == 0;
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
    auto addMove = [&](const Move& move) {
        moves.push_back(move);
    };

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
        const int enemyEndIdx = enemyStartIdx + kNumPiecesPerSide - 1;
        for (int enemyPieceIdx = enemyStartIdx; enemyPieceIdx < enemyEndIdx; ++enemyPieceIdx) {
            const int pinIdx = enemyPieceIdx - enemyStartIdx;
            if (isSet(pinBitBoards[pinIdx], position)) {
                BitBoard piecePinBitBoard = pinBitBoards[pinIdx];
                set(piecePinBitBoard, pieces_[enemyPieceIdx].position);
                return piecePinBitBoard;
            }
        }
        UNREACHABLE;
    };

    const BoardPosition enPassantTarget =
            enPassantWillPutUsInCheck() ? BoardPosition::Invalid : enPassantTarget_;

    // Generate moves for pawns and for promoted pawns
    const int ownStartIdx = kNumPiecesPerSide * (int)sideToMove_;
    const int nonPawnStartIdx = ownStartIdx + kNumNonPawns;
    for (int pieceIdx = ownStartIdx; pieceIdx < nonPawnStartIdx; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
            continue;
        }

        const BitBoard piecePinBitBoard = getPiecePinBitBoard(pieceInfo.position);

        const Piece piece = getPiece(pieceInfo.coloredPiece);
        if (piece == Piece::Pawn) {
            // Use pawn move generation
            generateSinglePawnMoves(
                    pieceInfo.position,
                    (PieceIndex)pieceIdx,
                    enPassantTarget,
                    sideToMove_,
                    occupation_,
                    addMove,
                    /*getControlledSquares =*/false,
                    piecePinBitBoard);
        } else {
            // Promoted piece: use normal move generation
            generateSinglePieceMovesFromControl(
                    (PieceIndex)pieceIdx,
                    pieceInfo.controlledSquares,
                    occupation_,
                    addMove,
                    piecePinBitBoard);
        }
    }
    // Generate moves for normal pieces (non-pawns excl. king)
    for (int pieceIdx = nonPawnStartIdx; pieceIdx < nonPawnStartIdx + kNumNonPawns - 1;
         ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
            continue;
        }

        const BitBoard piecePinBitBoard = getPiecePinBitBoard(pieceInfo.position);

        generateSinglePieceMovesFromControl(
                (PieceIndex)pieceIdx,
                pieceInfo.controlledSquares,
                occupation_,
                addMove,
                piecePinBitBoard);
    }

    // Generate king moves

    // Normal king moves
    const PieceIndex kingIndex = getKingIndex(sideToMove_);
    BitBoard kingControlledSquares = pieces_[(int)kingIndex].controlledSquares;
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControlledSquares);
    generateSinglePieceMovesFromControl(
            kingIndex,
            kingControlledSquares,
            occupation_,
            addMove,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */);

    // Castling moves
    generateCastlingMoves(
            sideToMove_,
            canCastleKingSide(sideToMove_),
            canCastleQueenSide(sideToMove_),
            occupation_,
            enemyControlledSquares,
            addMove);

    moves.lock();
    return moves;
}

StackVector<Move> GameState::generateMovesInCheck(
        StackOfVectors<Move>& stack, BitBoard enemyControlledSquares) const {
    StackVector<Move> moves = stack.makeStackVector();

    const PieceIndex kingIndex = getKingIndex(sideToMove_);
    const BoardPosition kingPosition = getPieceInfo(kingIndex).position;

    const auto addMoveIfEscapesCheck = [&](const Move& move) {
        // Can we do better here? The controlled squares change when the king move, so we can't rely
        // solely on the current controlled squares value.
        // TODO: We should just be able to check for the checking piece(s), if they're pinning pieces, whether
        // there's now a blocking piece between the king and checking piece.
        // Or maybe by calculating a controlled squares bitboard that ignores the king?
        GameState copyState(*this);
        (void)copyState.makeMove(move);
        (void)copyState.makeNullMove();
        if (!copyState.isInCheck()) {
            moves.push_back(move);
        }
    };

    BitBoard kingControlledSquares = pieces_[(int)kingIndex].controlledSquares;
    // King can't walk into check
    kingControlledSquares = subtract(kingControlledSquares, enemyControlledSquares);
    generateSinglePieceMovesFromControl(
            kingIndex,
            kingControlledSquares,
            occupation_,
            addMoveIfEscapesCheck,
            /*piecePinBitBoard =*/BitBoard::Full /* King can't be pinned. */);

    bool isDoubleCheck = false;
    PieceIndex checkingPieceIndex = PieceIndex::Invalid;

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
            isDoubleCheck = true;
            break;
        }
        checkingPieceIndex = (PieceIndex)enemyPieceIdx;
    }

    if (isDoubleCheck) {
        // Only the king can move in a double check
        moves.lock();
        return moves;
    }

    MY_ASSERT(checkingPieceIndex != PieceIndex::Invalid);
    const PieceInfo& checkingPieceInfo = getPieceInfo(checkingPieceIndex);

    const std::array<BitBoard, kNumPiecesPerSide - 1> piecePinOrKingAttackBitBoards =
            calculatePiecePinOrKingAttackBitBoards(sideToMove_);
    const BitBoard pinOrKingAttackBitBoard =
            calculatePinOrKingAttackBitBoard(piecePinOrKingAttackBitBoards);

    BitBoard blockBitBoard = BitBoard::Empty;
    if (isPinningPiece(getPiece(checkingPieceInfo.coloredPiece))) {
        const int pinIdx = (int)checkingPieceIndex - enemyPieceStartIdx;
        const BitBoard checkingPieceKingAttackBitBoard = piecePinOrKingAttackBitBoards[pinIdx];
        blockBitBoard =
                intersection(checkingPieceKingAttackBitBoard, checkingPieceInfo.controlledSquares);
    }
    const BitBoard pinBitBoard = subtract(pinOrKingAttackBitBoard, blockBitBoard);

    auto genMoveForNonPawn = [&](int pieceIdx, const PieceInfo& pieceInfo) {
        // Capture checking piece
        if (isSet(pieceInfo.controlledSquares, checkingPieceInfo.position)) {
            moves.emplace_back(
                    (PieceIndex)pieceIdx, checkingPieceInfo.position, MoveFlags::IsCapture);
        }

        // Block checking piece
        BitBoard blockingMovesBitboard = intersection(blockBitBoard, pieceInfo.controlledSquares);
        while (true) {
            const BoardPosition blockingPosition =
                    (BoardPosition)std::countr_zero((std::uint64_t)blockingMovesBitboard);
            if (blockingPosition == BoardPosition::Invalid) {
                break;
            }
            clear(blockingMovesBitboard, blockingPosition);
            moves.emplace_back((PieceIndex)pieceIdx, blockingPosition);
        }
    };

    bool canTakeCheckingPieceEnPassant = false;
    if (enPassantTarget_ != BoardPosition::Invalid) {
        const auto [enPassantFile, enPassantRank] = fileRankFromPosition(enPassantTarget_);
        const int enPassantPieceRank =
                sideToMove_ == Side::White ? enPassantRank - 1 : enPassantRank + 1;
        const BoardPosition enPassantPiecePosition =
                positionFromFileRank(enPassantFile, enPassantPieceRank);
        canTakeCheckingPieceEnPassant = enPassantPiecePosition == checkingPieceInfo.position;
    }

    // Generate pawn (and promoted piece) moves that either capture the checking piece or block
    auto addMoveIfCapturesOrBlocks = [&](const Move& move) {
        if (canTakeCheckingPieceEnPassant && move.to == enPassantTarget_) {
            moves.push_back(move);
        } else if (move.to == checkingPieceInfo.position || isSet(blockBitBoard, move.to)) {
            moves.push_back(move);
        }
    };
    const int pieceStartIdx =
            sideToMove_ == Side::White ? (int)PieceIndex::WhitePawn0 : (int)PieceIndex::BlackPawn0;
    const int nonPawnStartIdx = pieceStartIdx + kNumPawns;
    for (int pieceIdx = pieceStartIdx; pieceIdx < nonPawnStartIdx; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
            continue;
        }
        if (isSet(pinBitBoard, pieceInfo.position)) {
            // Piece is pinned; can't capture pinning piece or remain in pin because already in check, so no moves.
            continue;
        }

        if (getPiece(pieceInfo.coloredPiece) == Piece::Pawn) {
            generateSinglePawnMoves(
                    pieceInfo.position,
                    (PieceIndex)pieceIdx,
                    enPassantTarget_,
                    sideToMove_,
                    occupation_,
                    addMoveIfCapturesOrBlocks,
                    /*getControlledSquares =*/false);
        } else {
            genMoveForNonPawn(pieceIdx, pieceInfo);
        }
    }

    // Generate non-pawn moves that either capture the checking piece or block
    const int ownNonPawnStartIdx = sideToMove_ == Side::White ? (int)PieceIndex::WhiteNonPawns
                                                              : (int)PieceIndex::BlackNonPawns;
    const int ownNonPawnEndIdx = ownNonPawnStartIdx + kNumNonPawns - 1;  // skip king
    for (int pieceIdx = ownNonPawnStartIdx; pieceIdx < ownNonPawnEndIdx; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
            continue;
        }
        if (isSet(pinBitBoard, pieceInfo.position)) {
            // Piece is pinned; can't capture pinning piece or remain in pin because already in check, so no moves.
            continue;
        }

        genMoveForNonPawn(pieceIdx, pieceInfo);
    }

    moves.lock();
    return moves;
}

GameState::UnmakeMoveInfo GameState::makeMove(const Move& move) {
    UnmakeMoveInfo unmakeInfo = {
            .from = getPieceInfo(move.pieceToMove).position,
            .enPassantTarget = enPassantTarget_,
            .castlingRights = castlingRights_,
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
            .enPassantTarget = enPassantTarget_,
            .castlingRights = castlingRights_,
            .plySinceCaptureOrPawn = plySinceCaptureOrPawn_};

    sideToMove_ = nextSide(sideToMove_);
    enPassantTarget_ = BoardPosition::Invalid;
    // Do not increment plySinceCaptureOrPawn_
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);

    return unmakeInfo;
}

void GameState::unmakeMove(const Move& move, const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_ = nextSide(sideToMove_);
    enPassantTarget_ = unmakeMoveInfo.enPassantTarget;
    castlingRights_ = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);

    if (isCastle(move.flags)) {
        makeCastleMove(move, /*reverse*/ true);
    } else {
        unmakeSinglePieceMove(move, unmakeMoveInfo);
    }
}

void GameState::unmakeNullMove(const UnmakeMoveInfo& unmakeMoveInfo) {
    sideToMove_ = nextSide(sideToMove_);
    enPassantTarget_ = unmakeMoveInfo.enPassantTarget;
    castlingRights_ = unmakeMoveInfo.castlingRights;
    plySinceCaptureOrPawn_ = unmakeMoveInfo.plySinceCaptureOrPawn;
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);
}

void GameState::makeCastleMove(const Move& move, const bool reverse) {
    const int kingFromFile = 4;  // e
    const int kingFromRank = sideToMove_ == Side::White ? 0 : 7;

    const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
    const bool isQueenSide = kingToFile == 2;  // c

    MY_ASSERT(IMPLIES(isQueenSide, canCastleQueenSide(sideToMove_)));
    MY_ASSERT(IMPLIES(!isQueenSide, canCastleKingSide(sideToMove_)));

    const int rookFromFile = isQueenSide ? /*a*/ 0 : /*h*/ 7;
    BoardPosition rookFromPosition = positionFromFileRank(rookFromFile, kingFromRank);
    BoardPosition rookToPosition =
            positionFromFileRank((kingFromFile + kingToFile) / 2, kingFromRank);

    BoardPosition kingFromPosition = positionFromFileRank(kingFromFile, kingFromRank);
    BoardPosition kingToPosition = move.to;

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

        sideToMove_ = nextSide(sideToMove_);
        enPassantTarget_ = BoardPosition::Invalid;
        ++plySinceCaptureOrPawn_;
        std::swap(occupation_.ownPiece, occupation_.enemyPiece);
    }
}

PieceIndex GameState::makeSinglePieceMove(const Move& move) {
    PieceInfo& movedPieceInfo = getPieceInfo(move.pieceToMove);
    const BoardPosition moveFrom = movedPieceInfo.position;

    PieceIndex capturedPieceIndex = PieceIndex::Invalid;
    BoardPosition captureTargetSquare = move.to;

    if (isEnPassant(move.flags)) {
        MY_ASSERT(isCapture(move.flags));
        MY_ASSERT(move.to == enPassantTarget_);

        const auto [fromFile, fromRank] = fileRankFromPosition(moveFrom);
        const auto [toFile, toRank] = fileRankFromPosition(move.to);
        captureTargetSquare = positionFromFileRank(toFile, fromRank);
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

    std::array<BoardPosition, 4> affectedSquares;
    int numAffectedSquares = 0;
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

    std::array<BoardPosition, 4> affectedSquares;
    int numAffectedSquares = 0;
    affectedSquares[numAffectedSquares++] = unmakeMoveInfo.from;
    affectedSquares[numAffectedSquares++] = move.to;

    if (isCapture(move.flags)) {
        MY_ASSERT(unmakeMoveInfo.capturedPieceIndex != PieceIndex::Invalid);
        PieceInfo& capturedPieceInfo = getPieceInfo(unmakeMoveInfo.capturedPieceIndex);
        capturedPieceInfo.captured = false;

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
    const auto [_, toRank] = fileRankFromPosition(move.to);

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
    std::array<BitBoard, kNumPiecesPerSide - 1> piecePinOrKingAttackBitBoards;
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
        const int pinIdx = pieceIdx - enemyPieceStartIdx;
        piecePinOrKingAttackBitBoards[pinIdx] = BitBoard::Empty;

        const Piece pinningPiece = getPiece(pinningPieceInfo.coloredPiece);
        if (!isPinningPiece(pinningPiece) || pinningPieceInfo.captured) {
            continue;
        }

        int deltaFile;
        int deltaRank;
        const bool deltaFileRankOk = getDeltaFileRank(
                pinningPieceInfo.position, kingPieceInfo.position, deltaFile, deltaRank);
        if (!deltaFileRankOk) {
            continue;
        }

        if (!isValidDeltaFileRankForPiece(deltaFile, deltaRank, pinningPiece)) {
            continue;
        }

        auto [file, rank] = fileRankFromPosition(pinningPieceInfo.position);
        int numKingSidePieces = 0;
        bool pinningSidePieces = false;
        bool reachedKing = false;
        BitBoard pinningBitBoard = BitBoard::Empty;
        while (true) {
            file += deltaFile;
            rank += deltaRank;

            if (file < 0 || file > 7 || rank < 0 || rank > 7) {
                break;
            }

            const BoardPosition position = positionFromFileRank(file, rank);
            if (position == kingPieceInfo.position) {
                reachedKing = true;
                break;
            }

            if (isSet(pinningSideOccupancy, position)) {
                pinningSidePieces = true;
                break;
            }
            if (isSet(kingSideOccupancy, position)) {
                ++numKingSidePieces;
                if (numKingSidePieces > 1) {
                    break;
                }
            }

            set(pinningBitBoard, position);
        }
        if (reachedKing && numKingSidePieces <= 1 && !pinningSidePieces) {
            piecePinOrKingAttackBitBoards[pinIdx] = pinningBitBoard;
        }
    }
    return piecePinOrKingAttackBitBoards;
}

BitBoard GameState::calculatePinOrKingAttackBitBoard(
        const std::array<BitBoard, kNumPiecesPerSide - 1>& piecePinOrKingAttackBitBoards) const {
    BitBoard pinOrKingAttackBitBoard = BitBoard::Empty;
    for (BitBoard piecePinOrKinigAttackBitBoard : piecePinOrKingAttackBitBoards) {
        pinOrKingAttackBitBoard = any(pinOrKingAttackBitBoard, piecePinOrKinigAttackBitBoard);
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
    const auto [kingFile, kingRank] = fileRankFromPosition(kingPosition);
    if (kingRank != enPassantOriginRank) {
        return false;
    }

    const bool kingIsLeft = kingFile < enPassantTargetFile;

    int numOwnPawns = 0;
    int closestEnemyRookOrQueenDistance = kFiles;
    int closestOtherDistance = kFiles;
    bool foundKingBlockingPiece = false;

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

        const Side side = getSide(pieceInfo.coloredPiece);
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
            const bool deltaFileRankOk =
                    getDeltaFileRank(pieceInfo.position, affectedSquare, deltaFile, deltaRank);
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
    PieceOccupationBitBoards pieceSideOccupation = occupation_;
    if (getSide(pieceInfo.coloredPiece) != sideToMove_) {
        std::swap(pieceSideOccupation.ownPiece, pieceSideOccupation.enemyPiece);
    }

    pieceInfo.controlledSquares = BitBoard::Empty;
    auto addMove = [&](const Move& move) {
        set(pieceInfo.controlledSquares, move.to);
    };

    generateSinglePieceMoves(
            pieceInfo,
            PieceIndex::Invalid,  // This is unused... TODO: clean this stuff up
            /*en passant*/ BoardPosition::Invalid,
            pieceSideOccupation,
            addMove,
            /*getControlledSquares=*/true);
}

BitBoard GameState::getEnemyControlledSquares() const {
    BitBoard controlledSquares = BitBoard::Empty;
    const Side enemySide = nextSide(sideToMove_);
    const int enemyStartIdx = kNumPiecesPerSide * (int)enemySide;
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
    const PieceIndex kingIdx = getKingIndex(sideToMove_);
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
