#include "GameState.h"

#include <array>

#include <cassert>

#define IMPLIES(a, b) (!(a) || (b))

namespace {

constexpr std::array kSigns = {+1, -1};

void generateSinglePawnMoves(
        const BoardPosition origin, const BoardPosition enPassantTarget,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves, const bool getControlledSquares = false) {
    static constexpr std::array kPromotionPieces = {
            Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen};

    assert(positionToPiece.find(origin) != positionToPiece.end());
    assert(getPiece(positionToPiece.find(origin)->second) == Piece::Pawn);

    const auto [file, rank] = fileRankFromPosition(origin);

    const Side side = getSide(positionToPiece.at(origin));
    assert(side == Side::White || side == Side::Black);

    const int forwardDirection = side == Side::White ? 1 : -1;
    const int startingRank = side == Side::White ? 1 : 6;

    const int newRank = rank + forwardDirection;

    // Push pawn
    // Skip this if getControlledSquares: pawns don't control squares they can push to
    if (!getControlledSquares) {
        const BoardPosition forwardPosition =
                positionFromFileRank(file, newRank);
        if (positionToPiece.count(forwardPosition) == 0) {
            if (newRank == 0 || newRank == 7) {
                // Promotion
                for (const auto promotionPiece : kPromotionPieces) {
                    moves.push_back(Move{origin, forwardPosition,
                                         getFlags(promotionPiece)});
                }
            } else {
                // Normal push
                moves.push_back(Move{origin, forwardPosition});
            }

            // Double push
            if (rank == startingRank) {
                const BoardPosition doubleForwardPosition =
                        positionFromFileRank(file, rank + 2 * forwardDirection);
                if (positionToPiece.count(doubleForwardPosition) == 0) {
                    moves.push_back(Move{origin, doubleForwardPosition});
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
        const BoardPosition capturePosition =
                positionFromFileRank(newFile, newRank);
        if (capturePosition == enPassantTarget) {
            moves.push_back(Move{
                    origin, capturePosition,
                    getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)});
            continue;
        }

        const auto targetPieceIt = positionToPiece.find(capturePosition);
        const bool isEmpty = targetPieceIt == positionToPiece.end();
        const bool isEnemyPiece =
                !isEmpty && getSide(targetPieceIt->second) != side;
        if (isEnemyPiece || (getControlledSquares && isEmpty)) {
            if (!getControlledSquares && (newRank == 0 || newRank == 7)) {
                // Capture + promotion
                for (const auto promotionPiece : kPromotionPieces) {
                    moves.push_back(Move{
                            origin, capturePosition,
                            getFlags(MoveFlags::IsCapture, promotionPiece)});
                }
            } else {
                // Normal capture
                moves.push_back(
                        Move{origin, capturePosition, MoveFlags::IsCapture});
            }
        }
    }
}

void generateSingleKnightMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves) {
    static constexpr std::array kFileRankDsts = {std::pair{1, 2},
                                                 std::pair{2, 1}};

    assert(positionToPiece.find(origin) != positionToPiece.end());
    assert(getPiece(positionToPiece.find(origin)->second) == Piece::Knight);

    const auto [file, rank] = fileRankFromPosition(origin);

    const Side side = getSide(positionToPiece.at(origin));
    assert(side == Side::White || side == Side::Black);

    for (const auto [fileDst, rankDst] : kFileRankDsts) {
        for (const auto fileSign : kSigns) {
            for (const auto rankSign : kSigns) {
                const int newFile = file + fileSign * fileDst;
                const int newRank = rank + rankSign * rankDst;

                if (newFile < 0 || newFile > 7 || newRank < 0 || newRank > 7) {
                    continue;
                }
                const BoardPosition newPosition =
                        positionFromFileRank(newFile, newRank);

                const auto targetPieceIt = positionToPiece.find(newPosition);
                const bool isEmpty = targetPieceIt == positionToPiece.end();
                const bool isEnemyPiece =
                        !isEmpty && getSide(targetPieceIt->second) != side;
                if (isEmpty || isEnemyPiece) {
                    const MoveFlags flags = isEnemyPiece ? MoveFlags::IsCapture
                                                         : MoveFlags::None;
                    moves.push_back({origin, newPosition, flags});
                }
            }
        }
    }
}

void generateSliderMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        const int deltaFile, const int deltaRank, const bool isKing,
        std::vector<Move>& moves) {
    const auto [file, rank] = fileRankFromPosition(origin);

    const Side side = getSide(positionToPiece.at(origin));
    assert(side == Side::White || side == Side::Black);

    int newFile = file;
    int newRank = rank;
    do {
        newFile += deltaFile;
        newRank += deltaRank;

        if (newFile < 0 || newFile > 7 || newRank < 0 || newRank > 7) {
            break;
        }

        const BoardPosition newPosition =
                positionFromFileRank(newFile, newRank);
        const auto targetPieceIt = positionToPiece.find(newPosition);

        if (targetPieceIt != positionToPiece.end()) {
            const Side targetSide = getSide(targetPieceIt->second);
            if (targetSide != side) {
                // Capture
                moves.push_back({origin, newPosition, MoveFlags::IsCapture});
            }
            // Further moves are blocked
            break;
        }

        // Normal move
        moves.push_back({origin, newPosition});

        if (isKing) {
            // King only moves a distance of 1
            break;
        }
    } while (true);
}

void generateSingleBishopMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves) {
    assert(positionToPiece.find(origin) != positionToPiece.end());
    assert(getPiece(positionToPiece.find(origin)->second) == Piece::Bishop);

    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(origin, positionToPiece, deltaFile, deltaRank,
                                false, moves);
        }
    }
}

void generateSingleRookMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves) {
    assert(positionToPiece.find(origin) != positionToPiece.end());
    assert(getPiece(positionToPiece.find(origin)->second) == Piece::Rook);

    for (const auto sign : kSigns) {
        generateSliderMoves(origin, positionToPiece, sign, 0, false, moves);
        generateSliderMoves(origin, positionToPiece, 0, sign, false, moves);
    }
}

void generateSingleQueenMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves) {
    assert(positionToPiece.find(origin) != positionToPiece.end());
    assert(getPiece(positionToPiece.find(origin)->second) == Piece::Queen);

    // Diagonals
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(origin, positionToPiece, deltaFile, deltaRank,
                                false, moves);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(origin, positionToPiece, sign, 0, false, moves);
        generateSliderMoves(origin, positionToPiece, 0, sign, false, moves);
    }
}

void generateNormalKingMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves) {
    assert(positionToPiece.find(origin) != positionToPiece.end());
    assert(getPiece(positionToPiece.find(origin)->second) == Piece::King);

    // Diagonals
    for (const auto deltaFile : kSigns) {
        for (const auto deltaRank : kSigns) {
            generateSliderMoves(origin, positionToPiece, deltaFile, deltaRank,
                                true, moves);
        }
    }
    // Cardinal
    for (const auto sign : kSigns) {
        generateSliderMoves(origin, positionToPiece, sign, 0, true, moves);
        generateSliderMoves(origin, positionToPiece, 0, sign, true, moves);
    }
}

void generateCastlingMoves(
        const Side sideToMove, const bool canCastleKingSide,
        const bool canCastleQueenSide,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        const std::set<BoardPosition>& enemeyControlledSquares,
        std::vector<Move>& moves) {
    assert(sideToMove == Side::White || sideToMove == Side::Black);

    const BoardPosition kingPosition = sideToMove == Side::White
                                               ? positionFromAlgebraic("e1")
                                               : positionFromAlgebraic("e8");

    const bool inCheck = enemeyControlledSquares.count(kingPosition);
    if (inCheck) {
        // No castle moves are possible while in check
        return;
    }

    const auto [kingFile, kingRank] = fileRankFromPosition(kingPosition);

    if (canCastleKingSide) {
        bool castleIsValid = true;
        for (int fileDelta = 1; fileDelta <= 2; ++fileDelta) {
            const BoardPosition position =
                    positionFromFileRank(kingFile + fileDelta, kingRank);
            if (positionToPiece.count(position)) {
                // Blocking piece
                castleIsValid = false;
                break;
            }
            if (enemeyControlledSquares.count(position)) {
                // King passes through or into check
                castleIsValid = false;
                break;
            }
        }

        if (castleIsValid) {
            const BoardPosition targetPosition =
                    positionFromFileRank(kingFile + 2, kingRank);
            moves.push_back(
                    {kingPosition, targetPosition, MoveFlags::IsCastle});
        }
    }
    if (canCastleQueenSide) {
        bool castleIsValid = true;
        for (int fileDelta = 1; fileDelta <= 3; ++fileDelta) {
            const BoardPosition position =
                    positionFromFileRank(kingFile - fileDelta, kingRank);
            if (positionToPiece.count(position)) {
                // Blocking piece
                castleIsValid = false;
                break;
            }
            if (fileDelta <= 2 && enemeyControlledSquares.count(position)) {
                // King passes through or into check
                castleIsValid = false;
                break;
            }
        }

        if (castleIsValid) {
            const BoardPosition targetPosition =
                    positionFromFileRank(kingFile - 2, kingRank);
            moves.push_back(
                    {kingPosition, targetPosition, MoveFlags::IsCastle});
        }
    }
}

}  // namespace

std::map<BoardPosition, ColoredPiece> getPositionToPieceMap(
        const std::vector<PiecePosition>& pieces) {
    std::map<BoardPosition, ColoredPiece> positionToPiece;
    for (const auto [piece, position] : pieces) {
        positionToPiece.emplace(position, piece);
    }
    return positionToPiece;
}

GameState GameState::startingPosition() {
    return fromFen(kStartingPositionFen);
}

bool GameState::isInCheck() const {
    return isInCheck(
            generateEnemyControlledSquares(getPositionToPieceMap(pieces_)));
}

std::vector<Move> GameState::generateMoves() const {
    const std::map<BoardPosition, ColoredPiece> positionToPiece =
            getPositionToPieceMap(pieces_);
    const std::set<BoardPosition> enemeyControlledSquares =
            generateEnemyControlledSquares(positionToPiece);

    std::vector<Move> moves;

    for (const auto& [coloredPiece, position] : pieces_) {
        if (getSide(coloredPiece) != sideToMove_) {
            // Skip enemy pieces
            continue;
        }

        switch (getPiece(coloredPiece)) {
            case Piece::Pawn:
                generateSinglePawnMoves(position, enPassantTarget_,
                                        positionToPiece, moves);
                break;
            case Piece::Knight:
                generateSingleKnightMoves(position, positionToPiece, moves);
                break;
            case Piece::Bishop:
                generateSingleBishopMoves(position, positionToPiece, moves);
                break;
            case Piece::Rook:
                generateSingleRookMoves(position, positionToPiece, moves);
                break;
            case Piece::Queen:
                generateSingleQueenMoves(position, positionToPiece, moves);
                break;
            case Piece::King:
                generateNormalKingMoves(position, positionToPiece, moves);
                break;
            default:
                std::unreachable();
                break;
        }
    }

    generateCastlingMoves(sideToMove_, canCastleKingSide(sideToMove_),
                          canCastleQueenSide(sideToMove_), positionToPiece,
                          enemeyControlledSquares, moves);

    // Remove moves that put us in check. Very slow!!
    for (int moveIdx = 0; moveIdx < moves.size();) {
        GameState copyState(*this);
        copyState.makeMove(moves[moveIdx]);
        copyState.sideToMove_ = sideToMove_;
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
    if (isCastle(move.flags)) {
        handleCastle(move);
    } else {
        handleSinglePieceMove(move);
    }

    return {};  // TODO
}

void GameState::unmakeMove(const Move& move,
                           const UnmakeMoveInfo& unmakeMoveInfo) {
    // TODO
}

void GameState::handleCastle(const Move& move) {
    const auto [kingFromFile, kingFromRank] = fileRankFromPosition(move.from);
    const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
    const bool isQueenSide = kingToFile == 2;  // c

    assert(IMPLIES(isQueenSide, canCastleQueenSide(sideToMove_)));
    assert(IMPLIES(!isQueenSide, canCastleKingSide(sideToMove_)));

    const int rookFromFile = isQueenSide ? /*a*/ 0 : /*h*/ 7;
    const BoardPosition rookFromPosition =
            positionFromFileRank(rookFromFile, kingFromRank);
    const BoardPosition rookToPosition =
            positionFromFileRank((kingFromFile + kingToFile) / 2, kingFromRank);

    for (auto& [coloredPiece, position] : pieces_) {
        if (position == move.from) {
            assert(getPiece(coloredPiece) == Piece::King);
            assert(getSide(coloredPiece) == sideToMove_);

            position = move.to;
        } else if (position == rookFromPosition) {
            assert(getPiece(coloredPiece) == Piece::Rook);
            assert(getSide(coloredPiece) == sideToMove_);

            position = rookToPosition;
        }
    }

    setCanCastleKingSide(sideToMove_, false);
    setCanCastleQueenSide(sideToMove_, false);

    sideToMove_ = nextSide(sideToMove_);
    enPassantTarget_ = BoardPosition::Invalid;
    ++plySinceCaptureOrPawn_;
}

void GameState::handleSinglePieceMove(const Move& move) {
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

    if (capturedPieceIt != pieces_.end()) {
        if (getPiece(capturedPieceIt->first) == Piece::Rook) {
            updateRookCastlingRights(captureTargetSquare,
                                     getSide(capturedPieceIt->first));
        }

        *capturedPieceIt = pieces_.back();
        pieces_.pop_back();
    }

    if (isCaptureOrPawnMove) {
        plySinceCaptureOrPawn_ = 0;
    } else {
        ++plySinceCaptureOrPawn_;
    }

    sideToMove_ = nextSide(sideToMove_);
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
        enPassantTarget_ =
                positionFromFileRank(fromFile, (fromRank + toRank) / 2);
    }
}

void GameState::handleNormalKingMove() {
    setCanCastleKingSide(sideToMove_, false);
    setCanCastleQueenSide(sideToMove_, false);
}

void GameState::updateRookCastlingRights(BoardPosition rookPosition,
                                         Side rookSide) {
    if (rookSide == Side::White &&
        rookPosition == positionFromAlgebraic("a1")) {
        setCanCastleQueenSide(rookSide, false);
    } else if (rookSide == Side::White &&
               rookPosition == positionFromAlgebraic("h1")) {
        setCanCastleKingSide(rookSide, false);
    } else if (rookSide == Side::Black &&
               rookPosition == positionFromAlgebraic("a8")) {
        setCanCastleQueenSide(rookSide, false);
    } else if (rookSide == Side::Black &&
               rookPosition == positionFromAlgebraic("h8")) {
        setCanCastleKingSide(rookSide, false);
    }
}

std::set<BoardPosition> GameState::generateEnemyControlledSquares(
        const std::map<BoardPosition, ColoredPiece>& positionToPiece) const {
    std::vector<Move> moves;

    for (const auto& [coloredPiece, position] : pieces_) {
        if (getSide(coloredPiece) == sideToMove_) {
            // Skip own pieces
            continue;
        }

        switch (getPiece(coloredPiece)) {
            case Piece::Pawn:
                // No en passant target square: enemy just moved so the last move definitely wasn't a
                // double pawn push by us.
                generateSinglePawnMoves(position, BoardPosition::Invalid,
                                        positionToPiece, moves,
                                        /*getControlledSquares =*/true);
                break;
            case Piece::Knight:
                generateSingleKnightMoves(position, positionToPiece, moves);
                break;
            case Piece::Bishop:
                generateSingleBishopMoves(position, positionToPiece, moves);
                break;
            case Piece::Rook:
                generateSingleRookMoves(position, positionToPiece, moves);
                break;
            case Piece::Queen:
                generateSingleQueenMoves(position, positionToPiece, moves);
                break;
            case Piece::King:
                generateNormalKingMoves(position, positionToPiece, moves);
                break;
            default:
                std::unreachable();
                break;
        }
    }

    std::set<BoardPosition> controlledSquares;
    for (const auto& move : moves) {
        controlledSquares.insert(move.to);
    }

    return controlledSquares;
}

bool GameState::isInCheck(
        const std::set<BoardPosition>& enemeyControlledSquares) const {
    const ColoredPiece myKing = getColoredPiece(Piece::King, sideToMove_);
    for (const auto [piece, position] : pieces_) {
        if (piece == myKing) {
            return enemeyControlledSquares.count(position);
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

void GameState::setCanCastle(const Side side, const CastlingRights castlingSide,
                             const bool canCastle) {
    const int bit = (int)castlingSide << ((int)side * 2);
    if (canCastle) {
        castlingRights_ = (CastlingRights)((int)castlingRights_ | bit);
    } else {
        castlingRights_ = (CastlingRights)((int)castlingRights_ & ~bit);
    }
}
