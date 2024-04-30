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
        const BoardPosition enPassantTarget,
        const Side side,
        const PieceOccupationBitBoards& occupation,
        FuncT&& addMove,
        const bool getControlledSquares = false) {
    static constexpr std::array kPromotionPieces = {
            Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen};

    const auto [file, rank] = fileRankFromPosition(origin);

    const int forwardDirection = side == Side::White ? 1 : -1;
    const int startingRank = side == Side::White ? 1 : 6;

    const int newRank = rank + forwardDirection;

    const BitBoard anyPiece = either(occupation.ownPiece, occupation.enemyPiece);

    // Push pawn
    // Skip this if getControlledSquares: pawns don't control squares they can push to
    if (!getControlledSquares) {
        const BoardPosition forwardPosition = positionFromFileRank(file, newRank);
        if (!isSet(anyPiece, forwardPosition)) {
            if (newRank == 0 || newRank == 7) {
                // Promotion
                for (const auto promotionPiece : kPromotionPieces) {
                    addMove(Move{origin, forwardPosition, getFlags(promotionPiece)});
                }
            } else {
                // Normal push
                addMove(Move{origin, forwardPosition});
            }

            // Double push
            if (rank == startingRank) {
                const BoardPosition doubleForwardPosition =
                        positionFromFileRank(file, rank + 2 * forwardDirection);
                if (!isSet(anyPiece, doubleForwardPosition)) {
                    addMove(Move{origin, doubleForwardPosition});
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
                    Move{origin,
                         capturePosition,
                         getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)});
            continue;
        }

        const bool isEmpty = !isSet(anyPiece, capturePosition);
        const bool isEnemyPiece = isSet(occupation.enemyPiece, capturePosition);
        if (isEnemyPiece || (getControlledSquares && isEmpty)) {
            if (!getControlledSquares && (newRank == 0 || newRank == 7)) {
                // Capture + promotion
                for (const auto promotionPiece : kPromotionPieces) {
                    addMove(
                            Move{origin,
                                 capturePosition,
                                 getFlags(MoveFlags::IsCapture, promotionPiece)});
                }
            } else {
                // Normal capture
                addMove(Move{origin, capturePosition, MoveFlags::IsCapture});
            }
        }
    }
}

template <typename FuncT>
void generateSingleKnightMoves(
        const BoardPosition origin, const PieceOccupationBitBoards& occupation, FuncT&& addMove) {
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
                if (!isOwn) {
                    const MoveFlags flags = isEnemyPiece ? MoveFlags::IsCapture : MoveFlags::None;
                    addMove({origin, newPosition, flags});
                }
            }
        }
    }
}

template <typename FuncT>
void generateSliderMoves(
        const BoardPosition origin,
        const PieceOccupationBitBoards& occupation,
        const int deltaFile,
        const int deltaRank,
        const bool isKing,
        FuncT&& addMove) {
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

        if (isOwn) {
            // Further moves are blocked
            break;
        }

        if (isEnemyPiece) {
            // Capture
            addMove({origin, newPosition, MoveFlags::IsCapture});
            // Further moves are blocked
            break;
        }

        // Normal move
        addMove({origin, newPosition});

        if (isKing) {
            // King only moves a distance of 1
            break;
        }
    } while (true);
}

template <typename FuncT>
void generateSingleBishopMoves(
        const BoardPosition origin, const PieceOccupationBitBoards& occupation, FuncT&& addMove) {
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(origin, occupation, deltaFile, deltaRank, false, addMove);
        }
    }
}

template <typename FuncT>
void generateSingleRookMoves(
        const BoardPosition origin, const PieceOccupationBitBoards& occupation, FuncT&& addMove) {
    for (const auto sign : kSigns) {
        generateSliderMoves(origin, occupation, sign, 0, false, addMove);
        generateSliderMoves(origin, occupation, 0, sign, false, addMove);
    }
}

template <typename FuncT>
void generateSingleQueenMoves(
        const BoardPosition origin, const PieceOccupationBitBoards& occupation, FuncT&& addMove) {
    // Diagonals
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(origin, occupation, deltaFile, deltaRank, false, addMove);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(origin, occupation, sign, 0, false, addMove);
        generateSliderMoves(origin, occupation, 0, sign, false, addMove);
    }
}

template <typename FuncT>
void generateNormalKingMoves(
        const BoardPosition origin, const PieceOccupationBitBoards& occupation, FuncT&& addMove) {
    // Diagonals
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(origin, occupation, deltaFile, deltaRank, true, addMove);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(origin, occupation, sign, 0, true, addMove);
        generateSliderMoves(origin, occupation, 0, sign, true, addMove);
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

    const BitBoard anyPiece = either(occupation.ownPiece, occupation.enemyPiece);

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
            addMove({kingPosition, targetPosition, MoveFlags::IsCastle});
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
            addMove({kingPosition, targetPosition, MoveFlags::IsCastle});
        }
    }
}

}  // namespace

GameState GameState::startingPosition() {
    return fromFen(kStartingPositionFen);
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

    for (const auto& [coloredPiece, position] : pieces_) {
        if (getSide(coloredPiece) != sideToMove_) {
            // Skip enemy pieces
            continue;
        }

        switch (getPiece(coloredPiece)) {
            case Piece::Pawn:
                generateSinglePawnMoves(
                        position, enPassantTarget_, sideToMove_, occupation_, addMove);
                break;
            case Piece::Knight:
                generateSingleKnightMoves(position, occupation_, addMove);
                break;
            case Piece::Bishop:
                generateSingleBishopMoves(position, occupation_, addMove);
                break;
            case Piece::Rook:
                generateSingleRookMoves(position, occupation_, addMove);
                break;
            case Piece::Queen:
                generateSingleQueenMoves(position, occupation_, addMove);
                break;
            case Piece::King:
                generateNormalKingMoves(position, occupation_, addMove);
                break;
            default:
                std::unreachable();
                break;
        }
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
        const Move move = moves[moveIdx];
        const UnmakeMoveInfo unmakeInfo = makeMove(move);
        const UnmakeMoveInfo unmakeNullInfo = makeNullMove();
        if (isInCheck()) {
            moves[moveIdx] = moves.back();
            moves.pop_back();
        } else {
            ++moveIdx;
        }
        unmakeNullMove(unmakeNullInfo);
        unmakeMove(move, unmakeInfo);
    }

    return moves;
}

GameState::UnmakeMoveInfo GameState::makeMove(const Move& move) {
    UnmakeMoveInfo unmakeInfo = {
            .enPassantTarget = enPassantTarget_,
            .castlingRights = castlingRights_,
            .plySinceCaptureOrPawn = plySinceCaptureOrPawn_};

    if (isCastle(move.flags)) {
        makeCastleMove(move);
    } else {
        unmakeInfo.capturedPiece = makeSinglePieceMove(move);
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
    // Not increment plySinceCaptureOrPawn_
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
        unmakeSinglePieceMove(move);
    }

    if (isCapture(move.flags)) {
        assert(unmakeMoveInfo.capturedPiece.first != ColoredPiece::None);
        assert(unmakeMoveInfo.capturedPiece.second != BoardPosition::Invalid);

        pieces_.push_back(unmakeMoveInfo.capturedPiece);
        set(occupation_.enemyPiece, unmakeMoveInfo.capturedPiece.second);
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
    const auto [kingFromFile, kingFromRank] = fileRankFromPosition(move.from);
    const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
    const bool isQueenSide = kingToFile == 2;  // c

    assert(IMPLIES(isQueenSide, canCastleQueenSide(sideToMove_)));
    assert(IMPLIES(!isQueenSide, canCastleKingSide(sideToMove_)));

    const int rookFromFile = isQueenSide ? /*a*/ 0 : /*h*/ 7;
    BoardPosition rookFromPosition = positionFromFileRank(rookFromFile, kingFromRank);
    BoardPosition rookToPosition =
            positionFromFileRank((kingFromFile + kingToFile) / 2, kingFromRank);

    BoardPosition kingFromPosition = move.from;
    BoardPosition kingToPosition = move.to;

    if (reverse) {
        std::swap(rookFromPosition, rookToPosition);
        std::swap(kingFromPosition, kingToPosition);
    }

    for (auto& [coloredPiece, position] : pieces_) {
        if (position == kingFromPosition) {
            assert(getPiece(coloredPiece) == Piece::King);
            assert(getSide(coloredPiece) == sideToMove_);

            position = kingToPosition;
        } else if (position == rookFromPosition) {
            assert(getPiece(coloredPiece) == Piece::Rook);
            assert(getSide(coloredPiece) == sideToMove_);

            position = rookToPosition;
        }
    }

    clear(occupation_.ownPiece, kingFromPosition);
    set(occupation_.ownPiece, kingToPosition);

    clear(occupation_.ownPiece, rookFromPosition);
    set(occupation_.ownPiece, rookToPosition);

    if (!reverse) {
        setCanCastleKingSide(sideToMove_, false);
        setCanCastleQueenSide(sideToMove_, false);

        sideToMove_ = nextSide(sideToMove_);
        enPassantTarget_ = BoardPosition::Invalid;
        ++plySinceCaptureOrPawn_;
        std::swap(occupation_.ownPiece, occupation_.enemyPiece);
    }
}

PiecePosition GameState::makeSinglePieceMove(const Move& move) {
    PiecePosition capturedPiece = {ColoredPiece::None, BoardPosition::Invalid};
    BoardPosition captureTargetSquare = BoardPosition::Invalid;

    if (isEnPassant(move.flags)) {
        assert(isCapture(move.flags));
        assert(move.to == enPassantTarget_);

        const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
        const auto [toFile, toRank] = fileRankFromPosition(move.to);
        captureTargetSquare = positionFromFileRank(toFile, fromRank);
    } else if (isCapture(move.flags)) {
        captureTargetSquare = move.to;
    }

    enPassantTarget_ = BoardPosition::Invalid;

    bool isCaptureOrPawnMove = isCapture(move.flags);
    auto capturedPieceIt = pieces_.end();

    for (auto pieceIt = pieces_.begin(); pieceIt != pieces_.end(); ++pieceIt) {
        auto& [coloredPiece, position] = *pieceIt;

        if (position == move.from) {
            assert(getSide(coloredPiece) == sideToMove_);

            position = move.to;

            const Piece piece = getPiece(coloredPiece);
            if (piece == Piece::Pawn) {
                isCaptureOrPawnMove = true;
                handlePawnMove(move, coloredPiece);
            } else if (piece == Piece::King) {
                handleNormalKingMove();
            } else if (piece == Piece::Rook) {
                updateRookCastlingRights(move.from, sideToMove_);
            }
        } else if (position == captureTargetSquare) {
            assert(getSide(coloredPiece) != sideToMove_);
            assert(isCapture(move.flags));

            capturedPieceIt = pieceIt;
        }
    }

    assert(IMPLIES(isCapture(move.flags), capturedPieceIt != pieces_.end()));

    clear(occupation_.ownPiece, move.from);
    set(occupation_.ownPiece, move.to);

    if (capturedPieceIt != pieces_.end()) {
        if (getPiece(capturedPieceIt->first) == Piece::Rook) {
            updateRookCastlingRights(captureTargetSquare, getSide(capturedPieceIt->first));
        }

        capturedPiece = *capturedPieceIt;
        *capturedPieceIt = pieces_.back();
        pieces_.pop_back();

        clear(occupation_.enemyPiece, captureTargetSquare);
    }

    if (isCaptureOrPawnMove) {
        plySinceCaptureOrPawn_ = 0;
    } else {
        ++plySinceCaptureOrPawn_;
    }

    sideToMove_ = nextSide(sideToMove_);
    std::swap(occupation_.ownPiece, occupation_.enemyPiece);

    return capturedPiece;
}

void GameState::unmakeSinglePieceMove(const Move& move) {
    for (auto& [coloredPiece, position] : pieces_) {
        if (position == move.to) {
            assert(getSide(coloredPiece) == sideToMove_);

            position = move.from;
            if (isPromotion(move.flags)) {
                assert(getPiece(coloredPiece) == getPromotionPiece(move.flags));
                coloredPiece = getColoredPiece(Piece::Pawn, sideToMove_);
            }
        }
    }

    clear(occupation_.ownPiece, move.to);
    set(occupation_.ownPiece, move.from);
}

void GameState::handlePawnMove(const Move& move, ColoredPiece& pieceToMove) {
    const Piece promotionPiece = getPromotionPiece(move.flags);
    if (promotionPiece != Piece::None) {
        pieceToMove = getColoredPiece(promotionPiece, sideToMove_);
    }

    const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
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

BitBoard GameState::generateEnemyControlledSquares() const {
    BitBoard controlledSquares = BitBoard::Empty;
    auto addMove = [&](const Move& move) {
        set(controlledSquares, move.to);
    };
    const Side enemySide = nextSide(sideToMove_);

    const PieceOccupationBitBoards invertedOccupation{
            .ownPiece = occupation_.enemyPiece, .enemyPiece = occupation_.ownPiece};

    for (const auto& [coloredPiece, position] : pieces_) {
        if (getSide(coloredPiece) != enemySide) {
            // Skip own pieces
            continue;
        }

        switch (getPiece(coloredPiece)) {
            case Piece::Pawn:
                // No en passant target square: enemy just moved so the last move definitely wasn't a
                // double pawn push by us.
                generateSinglePawnMoves(
                        position,
                        BoardPosition::Invalid,
                        enemySide,
                        invertedOccupation,
                        addMove,
                        /*getControlledSquares =*/true);
                break;
            case Piece::Knight:
                generateSingleKnightMoves(position, invertedOccupation, addMove);
                break;
            case Piece::Bishop:
                generateSingleBishopMoves(position, invertedOccupation, addMove);
                break;
            case Piece::Rook:
                generateSingleRookMoves(position, invertedOccupation, addMove);
                break;
            case Piece::Queen:
                generateSingleQueenMoves(position, invertedOccupation, addMove);
                break;
            case Piece::King:
                generateNormalKingMoves(position, invertedOccupation, addMove);
                break;
            default:
                std::unreachable();
                break;
        }
    }

    // Skip castling: control is not exerted through castling.

    return controlledSquares;
}

bool GameState::isInCheck(const BitBoard enemyControlledSquares) const {
    const ColoredPiece myKing = getColoredPiece(Piece::King, sideToMove_);
    for (const auto [piece, position] : pieces_) {
        if (piece == myKing) {
            return isSet(enemyControlledSquares, position);
        }
    }
    std::unreachable();
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
