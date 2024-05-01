#include "GameState.h"

#include <array>
#include <functional>

#include <cassert>

#define IMPLIES(a, b) (!(a) || (b))

namespace {

constexpr std::array kSigns = {+1, -1};

template <typename FuncT>
void generateSinglePawnMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const BoardPosition enPassantTarget,
        const Side side,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares) {
    static constexpr std::array kPromotionPieces = {
            Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen};

    const auto [file, rank] = fileRankFromPosition(origin);

    const int forwardDirection = side == Side::White ? 1 : -1;
    const int startingRank = side == Side::White ? 1 : 6;

    const int newRank = rank + forwardDirection;

    const BitBoard anyPiece = any(occupation.ownPiece, occupation.enemyPiece);

    // Push pawn
    // Skip this if getControlledSquares: pawns don't control squares they can push to
    if (!getControlledSquares) {
        const BoardPosition forwardPosition = positionFromFileRank(file, newRank);
        if (!isSet(anyPiece, forwardPosition)) {
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
        if (capturePosition == enPassantTarget) {
            addMove(
                    Move{pieceToMove,
                         capturePosition,
                         getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)});
            continue;
        }

        const bool isEnemyPiece = isSet(occupation.enemyPiece, capturePosition);
        if (isEnemyPiece || getControlledSquares) {
            if (!getControlledSquares && (newRank == 0 || newRank == 7)) {
                // Capture + promotion
                for (const auto promotionPiece : kPromotionPieces) {
                    addMove(
                            Move{pieceToMove,
                                 capturePosition,
                                 getFlags(MoveFlags::IsCapture, promotionPiece)});
                }
            } else {
                // Normal capture
                addMove(Move{pieceToMove, capturePosition, MoveFlags::IsCapture});
            }
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
        const bool getControlledSquares) {
    const auto [file, rank] = fileRankFromPosition(origin);

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
        const bool getControlledSquares) {
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
                    getControlledSquares);
        }
    }
}

template <typename FuncT>
void generateSingleRookMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares) {
    for (const auto sign : kSigns) {
        generateSliderMoves(
                origin, pieceToMove, occupation, sign, 0, false, addMove, getControlledSquares);
        generateSliderMoves(
                origin, pieceToMove, occupation, 0, sign, false, addMove, getControlledSquares);
    }
}

template <typename FuncT>
void generateSingleQueenMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares) {
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
                    getControlledSquares);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(
                origin, pieceToMove, occupation, sign, 0, false, addMove, getControlledSquares);
        generateSliderMoves(
                origin, pieceToMove, occupation, 0, sign, false, addMove, getControlledSquares);
    }
}

template <typename FuncT>
void generateNormalKingMoves(
        const BoardPosition origin,
        const PieceIndex pieceToMove,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares) {
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
                    getControlledSquares);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(
                origin, pieceToMove, occupation, sign, 0, true, addMove, getControlledSquares);
        generateSliderMoves(
                origin, pieceToMove, occupation, 0, sign, true, addMove, getControlledSquares);
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
    assert(sideToMove == Side::White || sideToMove == Side::Black);

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
        const bool getControlledSquares = false) {
    switch (getPiece(pieceInfo.coloredPiece)) {
        case Piece::Pawn:
            generateSinglePawnMoves(
                    pieceInfo.position,
                    pieceToMove,
                    enPassantTarget,
                    getSide(pieceInfo.coloredPiece),
                    occupation,
                    addMove,
                    getControlledSquares);
            break;
        case Piece::Knight:
            generateSingleKnightMoves(
                    pieceInfo.position, pieceToMove, occupation, addMove, getControlledSquares);
            break;
        case Piece::Bishop:
            generateSingleBishopMoves(
                    pieceInfo.position, pieceToMove, occupation, addMove, getControlledSquares);
            break;
        case Piece::Rook:
            generateSingleRookMoves(
                    pieceInfo.position, pieceToMove, occupation, addMove, getControlledSquares);
            break;
        case Piece::Queen:
            generateSingleQueenMoves(
                    pieceInfo.position, pieceToMove, occupation, addMove, getControlledSquares);
            break;
        case Piece::King:
            generateNormalKingMoves(
                    pieceInfo.position, pieceToMove, occupation, addMove, getControlledSquares);
            break;
        default:
            std::unreachable();
            break;
    }
}

}  // namespace

GameState GameState::startingPosition() {
    return fromFen(getStartingPositionFen());
}

bool GameState::isInCheck() const {
    return isInCheck(generateEnemyControlledSquares());
}

std::vector<Move> GameState::generateMoves() {
    const BitBoard enemyControlledSquares = generateEnemyControlledSquares();

    std::vector<Move> moves;
    auto addMove = [&](const Move& move) {
        moves.push_back(move);
    };

    const int ownStartIdx = kNumPiecesPerSide * (int)sideToMove_;
    const int endIdx = ownStartIdx + kNumPiecesPerSide;
    for (int pieceIdx = ownStartIdx; pieceIdx < endIdx; ++pieceIdx) {
        const PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured) {
            continue;
        }

        generateSinglePieceMoves(
                pieceInfo, (PieceIndex)pieceIdx, enPassantTarget_, occupation_, addMove);
    }

    generateCastlingMoves(
            sideToMove_,
            canCastleKingSide(sideToMove_),
            canCastleQueenSide(sideToMove_),
            occupation_,
            enemyControlledSquares,
            addMove);

    // Remove moves that put us in check. Very slow!!
    for (int moveIdx = 0; moveIdx < moves.size();) {
        GameState copyState(*this);
        const Move move = moves[moveIdx];
        (void)copyState.makeMove(move);
        (void)copyState.makeNullMove();
        if (copyState.isInCheck()) {
            moves[moveIdx] = moves.back();
            moves.pop_back();
        } else {
            ++moveIdx;
        }
    }

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

    assert(IMPLIES(isQueenSide, canCastleQueenSide(sideToMove_)));
    assert(IMPLIES(!isQueenSide, canCastleKingSide(sideToMove_)));

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
    assert(rookIdx != PieceIndex::Invalid);
    PieceInfo& rookPieceInfo = getPieceInfo(rookIdx);

    // Update king
    assert(getPiece(kingPieceInfo.coloredPiece) == Piece::King);
    assert(getSide(kingPieceInfo.coloredPiece) == sideToMove_);
    kingPieceInfo.position = kingToPosition;
    recalculateControlledSquares(kingPieceInfo);

    // Update rook
    assert(getPiece(rookPieceInfo.coloredPiece) == Piece::Rook);
    assert(getSide(rookPieceInfo.coloredPiece) == sideToMove_);
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
        assert(isCapture(move.flags));
        assert(move.to == enPassantTarget_);

        const auto [fromFile, fromRank] = fileRankFromPosition(moveFrom);
        const auto [toFile, toRank] = fileRankFromPosition(move.to);
        captureTargetSquare = positionFromFileRank(toFile, fromRank);
    }

    enPassantTarget_ = BoardPosition::Invalid;

    bool isCaptureOrPawnMove = isCapture(move.flags);

    clear(occupation_.ownPiece, moveFrom);
    set(occupation_.ownPiece, move.to);

    assert(getSide(movedPieceInfo.coloredPiece) == sideToMove_);
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
            assert(getSide(pieceInfo.coloredPiece) != sideToMove_);
            assert(isCapture(move.flags));

            capturedPieceIndex = (PieceIndex)pieceIdx;

            break;
        }
        assert(capturedPieceIndex != PieceIndex::Invalid);

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
        assert(unmakeMoveInfo.capturedPieceIndex != PieceIndex::Invalid);
        set(occupation_.enemyPiece, getPieceInfo(unmakeMoveInfo.capturedPieceIndex).position);
    }

    const int ownStartIdx = kNumPiecesPerSide * (int)sideToMove_;
    for (int pieceIdx = ownStartIdx;; ++pieceIdx) {
        PieceInfo& pieceInfo = pieces_[pieceIdx];
        if (pieceInfo.captured || pieceInfo.position != move.to) {
            continue;
        }
        assert(getSide(pieceInfo.coloredPiece) == sideToMove_);

        pieceInfo.position = unmakeMoveInfo.from;
        if (isPromotion(move.flags)) {
            assert(getPiece(pieceInfo.coloredPiece) == getPromotionPiece(move.flags));
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
        assert(unmakeMoveInfo.capturedPieceIndex != PieceIndex::Invalid);
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

            const auto [affectedFile, affectedRank] = fileRankFromPosition(affectedSquare);
            const auto [pieceFile, pieceRank] = fileRankFromPosition(pieceInfo.position);
            const int numSteps = std::max(
                    std::abs(affectedFile - pieceFile), std::abs(affectedRank - pieceRank));
            const int deltaFile = (affectedFile - pieceFile) / numSteps;
            const int deltaRank = (affectedRank - pieceRank) / numSteps;

            const BitBoard anyPiece = any(occupation_.ownPiece, occupation_.enemyPiece);

            const bool isOccupied = isSet(anyPiece, affectedSquare);

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

BitBoard GameState::generateEnemyControlledSquares() const {
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
