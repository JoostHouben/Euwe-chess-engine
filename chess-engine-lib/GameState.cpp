#include "GameState.h"

#include <cassert>
#include <cstdlib>

#include <map>
#include <sstream>

#define IMPLIES(a, b) (!(a) || (b))

namespace {
    constexpr char kLowerCaseBit = 1 << 5;
    constexpr std::array kSigns = { +1, -1 };

    constexpr bool isNumber(char c) {
        return c >= '0' && c <= '9';
    }

    constexpr Piece pieceFromFenChar(char c) {
        char upperCase = c & ~kLowerCaseBit;
        switch (upperCase) {
        case 'P':
            return Piece::Pawn;
        case 'N':
            return Piece::Knight;
        case 'B':
            return Piece::Bishop;
        case 'R':
            return Piece::Rook;
        case 'Q':
            return Piece::Queen;
        case 'K':
            return Piece::King;
        default:
            assert(false);
            return Piece{};
        }
    }

    constexpr Side sideFromFenChar(char c) {
        bool isUpperCase = !(c & kLowerCaseBit);
        if (isUpperCase) {
            return Side::White;
        }
        else {
            return Side::Black;
        }
    }

    constexpr char toFenChar(Side side) {
        switch (side) {
        case Side::White:
            return 'w';
        case Side::Black:
            return 'b';
        case Side::None:
            return 'x';
        default:
            assert(false);
            return '\0';
        }
    }

    constexpr ColoredPiece coloredPieceFromFenChar(char c) {
        return getColoredPiece(pieceFromFenChar(c), sideFromFenChar(c));
    }

    constexpr char toFenChar(Piece piece) {
        switch (piece) {
        case Piece::Pawn:
            return 'P';
        case Piece::Knight:
            return 'N';
        case Piece::Bishop:
            return 'B';
        case Piece::Rook:
            return 'R';
        case Piece::Queen:
            return 'Q';
        case Piece::King:
            return 'K';
        default:
            assert(false);
            return '\0';
        }

    }

    constexpr char toFenChar(ColoredPiece coloredPiece) {
        char c = toFenChar(getPiece(coloredPiece));
        if (getSide(coloredPiece) == Side::Black) {
            c |= kLowerCaseBit;
        }
        return c;
    }

    std::vector<PiecePosition> parseBoardConfigurationFromFen(
        std::string::const_iterator& strIt
    ) {
        std::vector<PiecePosition> pieces;

        for (int rank = 7; rank >= 0; --rank) {
            for (int file = 0; file < 8; ++strIt) {
                if (isNumber(*strIt)) {
                    file += (*strIt - '0');
                    continue;
                }
                ColoredPiece piece = coloredPieceFromFenChar(*strIt);
                BoardPosition position = positionFromFileRank(file, rank);
                pieces.emplace_back(piece, position);
                file += 1;
            }
            assert((rank > 0 && *strIt == '/') || (rank == 0 && *strIt == ' '));
            if (rank > 0) {
                ++strIt;
            }
        }

        return pieces;
    }

    Side parseSideToMoveFromFen(std::string::const_iterator& strIt) {
        switch (*(strIt++)) {
        case 'w':
            return Side::White;
        case 'b':
            return Side::Black;
        default:
            assert(false);
            return Side::None;
        }
    }

    void parseCastlingRightsFromFen(
        std::string::const_iterator& strIt,
        std::array<bool, kNumSides>& mayCastleKingSide,
        std::array<bool, kNumSides>& mayCastleQueenSide
    ) {
        if (*strIt == '-') {
            ++strIt;
            return;
        }

        for (; *strIt != ' '; ++strIt) {
            Side side = sideFromFenChar(*strIt);
            Piece piece = pieceFromFenChar(*strIt);
            switch (piece) {
            case Piece::King:
                mayCastleKingSide[(std::size_t)side] = true;
                break;
            case Piece::Queen:
                mayCastleQueenSide[(std::size_t)side] = true;
                break;
            default:
                assert(false);
            }
        }
    }

    BoardPosition parseEnPassantTargetFromFen(std::string::const_iterator& strIt) {
        if (*strIt == '-') {
            ++strIt;
            return BoardPosition::Invalid;
        }

        BoardPosition enPassantTarget = positionFromAlgebraic({ &*strIt, 2 });
        strIt += 2;
        return enPassantTarget;
    }

    std::uint16_t parsePlySinceCaptureOrPawnFromFen(std::string::const_iterator& strIt) {
        int plySinceCaptureOrPawn = std::atoi(&*strIt);
        do {
            ++strIt;
        } while (*strIt != ' ');
        return static_cast<std::uint16_t>(plySinceCaptureOrPawn);
    }

    std::map<BoardPosition, ColoredPiece> getPositionToPieceMap(const std::vector<PiecePosition>& pieces) {
        std::map<BoardPosition, ColoredPiece> positionToPiece;
        for (const auto [piece, position] : pieces) {
            positionToPiece.emplace(position, piece);
        }
        return positionToPiece;
    }

    void boardConfigurationToFen(const std::vector<PiecePosition>& pieces, std::ostream& out) {
        std::map<BoardPosition, ColoredPiece> positionToPiece = getPositionToPieceMap(pieces);
        for (int rank = 7; rank >= 0; --rank) {
            int numEmptyTiles = 0;
            for (int file = 0; file < 8; ++file) {
                auto pieceIt = positionToPiece.find(positionFromFileRank(file, rank));
                if (pieceIt == positionToPiece.end()) {
                    ++numEmptyTiles;
                    continue;
                }

                if (numEmptyTiles) {
                    out << numEmptyTiles;
                    numEmptyTiles = 0;
                }

                out << toFenChar(pieceIt->second);
            }

            if (numEmptyTiles) {
                out << numEmptyTiles;
            }

            if (rank > 0) {
                out << "/";
            }
        }
    }

    void sideToMoveToFen(Side side, std::ostream& out) {
        out << toFenChar(side);
    }

    void castlingRightsToFen(const GameState& gameState, std::ostream& out) {
        bool any = false;
        for (auto side : { Side::White, Side::Black }) {
            if (gameState.canCastleKingSide(side)) {
                any = true;
                out << toFenChar(getColoredPiece(Piece::King, side));
            }
            if (gameState.canCastleQueenSide(side)) {
                any = true;
                out << toFenChar(getColoredPiece(Piece::Queen, side));
            }
        }
        if (!any) {
            out << '-';
        }
    }

    void enPassantTargetToFen(BoardPosition enPassantTarget, std::ostream& out) {
        if (enPassantTarget == BoardPosition::Invalid) {
            out << '-';
        }
        else {
            out << algebraicFromPosition(enPassantTarget);
        }
    }

    void generateSinglePawnMoves(
        const BoardPosition origin,
        const BoardPosition enPassantTarget,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves,
        const bool getControlledSquares = false
    ) {
        static constexpr std::array kPromotionPieces = { Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen };

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
            const BoardPosition forwardPosition = positionFromFileRank(file, newRank);
            if (positionToPiece.count(forwardPosition) == 0) {
                if (newRank == 0 || newRank == 7) {
                    // Promotion
                    for (const auto promotionPiece : kPromotionPieces) {
                        moves.push_back(Move{ origin, forwardPosition, getFlags(promotionPiece) });
                    }
                }
                else {
                    // Normal push
                    moves.push_back(Move{ origin, forwardPosition });
                }

                // Double push
                if (rank == startingRank) {
                    const BoardPosition doubleForwardPosition = positionFromFileRank(file, rank + 2 * forwardDirection);
                    if (positionToPiece.count(doubleForwardPosition) == 0) {
                        moves.push_back(Move{ origin, doubleForwardPosition });
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
                moves.push_back(Move{
                    origin,
                    capturePosition,
                    getFlags(MoveFlags::IsCapture, MoveFlags::IsEnPassant)
                });
                continue;
            }

            const auto targetPieceIt = positionToPiece.find(capturePosition);
            const bool isEmpty = targetPieceIt == positionToPiece.end();
            const bool isEnemyPiece = !isEmpty && getSide(targetPieceIt->second) != side;
            if (isEnemyPiece || (getControlledSquares && isEmpty)) {
                if (!getControlledSquares && (newRank == 0 || newRank == 7)) {
                    // Capture + promotion
                    for (const auto promotionPiece : kPromotionPieces) {
                        moves.push_back(Move{
                            origin,
                            capturePosition,
                            getFlags(MoveFlags::IsCapture, promotionPiece) });
                    }
                }
                else {
                    // Normal capture
                    moves.push_back(Move{ origin, capturePosition, MoveFlags::IsCapture });
                }
            }
        }
    }

    void generateSingleKnightMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves
    ) {
        static constexpr std::array kFileRankDsts = {
            std::pair { 1, 2 },
            std::pair { 2, 1 }
        };

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
                    const BoardPosition newPosition = positionFromFileRank(newFile, newRank);

                    const auto targetPieceIt = positionToPiece.find(newPosition);
                    const bool isEmpty = targetPieceIt == positionToPiece.end();
                    const bool isEnemyPiece = !isEmpty && getSide(targetPieceIt->second) != side;
                    if (isEmpty || isEnemyPiece) {
                        const MoveFlags flags = isEnemyPiece ? MoveFlags::IsCapture : MoveFlags::None;
                        moves.push_back({ origin, newPosition, flags });
                    }
                }
            }
        }
    }

    void generateSliderMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        const int deltaFile,
        const int deltaRank,
        const bool isKing,
        std::vector<Move>& moves
    ) {
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

            const BoardPosition newPosition = positionFromFileRank(newFile, newRank);
            const auto targetPieceIt = positionToPiece.find(newPosition);

            if (targetPieceIt != positionToPiece.end()) {
                const Side targetSide = getSide(targetPieceIt->second);
                if (targetSide != side) {
                    // Capture
                    moves.push_back({ origin, newPosition, MoveFlags::IsCapture });
                }
                // Further moves are blocked
                break;
            }

            // Normal move
            moves.push_back({ origin, newPosition });

            if (isKing) {
                // King only moves a distance of 1
                break;
            }
        } while (true);
    }

    void generateSingleBishopMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves
    ) {
        assert(positionToPiece.find(origin) != positionToPiece.end());
        assert(getPiece(positionToPiece.find(origin)->second) == Piece::Bishop);

        for (const auto deltaFile : kSigns) {
            for (const auto deltaRank : kSigns) {
                generateSliderMoves(origin, positionToPiece, deltaFile, deltaRank, false, moves);
            }
        }
    }

    void generateSingleRookMoves(
        const BoardPosition origin,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        std::vector<Move>& moves
    ) {
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
        std::vector<Move>& moves
    ) {
        assert(positionToPiece.find(origin) != positionToPiece.end());
        assert(getPiece(positionToPiece.find(origin)->second) == Piece::Queen);

        // Diagonals
        for (const auto deltaFile : kSigns) {
            for (const auto deltaRank : kSigns) {
                generateSliderMoves(origin, positionToPiece, deltaFile, deltaRank, false, moves);
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
        std::vector<Move>& moves
    ) {
        assert(positionToPiece.find(origin) != positionToPiece.end());
        assert(getPiece(positionToPiece.find(origin)->second) == Piece::King);

        // Diagonals
        for (const auto deltaFile : kSigns) {
            for (const auto deltaRank : kSigns) {
                generateSliderMoves(origin, positionToPiece, deltaFile, deltaRank, true, moves);
            }
        }
        // Cardinal
        for (const auto sign : kSigns) {
            generateSliderMoves(origin, positionToPiece, sign, 0, true, moves);
            generateSliderMoves(origin, positionToPiece, 0, sign, true, moves);
        }
    }

    void generateCastlingMoves(
        const Side sideToMove,
        const bool canCastleKingSide,
        const bool canCastleQueenSide,
        const std::map<BoardPosition, ColoredPiece>& positionToPiece,
        const std::set<BoardPosition>& enemeyControlledSquares,
        std::vector<Move>& moves
    ) {
        assert(sideToMove == Side::White || sideToMove == Side::Black);

        const BoardPosition kingPosition =
            sideToMove == Side::White
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
                const BoardPosition position = positionFromFileRank(kingFile + fileDelta, kingRank);
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
                const BoardPosition targetPosition = positionFromFileRank(kingFile + 2, kingRank);
                moves.push_back({ kingPosition, targetPosition, MoveFlags::IsCastle });
            }
        }
        if (canCastleQueenSide) {
            bool castleIsValid = true;
            for (int fileDelta = 1; fileDelta <= 3; ++fileDelta) {
                const BoardPosition position = positionFromFileRank(kingFile - fileDelta, kingRank);
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
                const BoardPosition targetPosition = positionFromFileRank(kingFile - 2, kingRank);
                moves.push_back({ kingPosition, targetPosition, MoveFlags::IsCastle });
            }
        }
    }
}

GameState GameState::fromFen(const std::string& fenString) {
    GameState gameState{};

    auto strIt = fenString.begin();

    gameState.pieces_ = parseBoardConfigurationFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    gameState.sideToMove_ = parseSideToMoveFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    parseCastlingRightsFromFen(strIt, gameState.mayCastleKingSide_, gameState.mayCastleQueenSide_);
    assert(*strIt == ' ');
    ++strIt;

    gameState.enPassantTarget_ = parseEnPassantTargetFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    gameState.plySinceCaptureOrPawn_ = parsePlySinceCaptureOrPawnFromFen(strIt);
    assert(*strIt == ' ');
    ++strIt;

    // Ignore: move counter

    assert(strIt < fenString.end());

    return gameState;
}

GameState GameState::startingPosition() {
    std::string startingPositionFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    return fromFen(startingPositionFen);
}

std::string GameState::toFen(int moveCounter) const {
    std::ostringstream ss;

    boardConfigurationToFen(pieces_, ss);
    ss << ' ';
    sideToMoveToFen(sideToMove_, ss);
    ss << ' ';
    castlingRightsToFen(*this, ss);
    ss << ' ';
    enPassantTargetToFen(enPassantTarget_, ss);
    ss << ' ';
    ss << plySinceCaptureOrPawn_;
    ss << ' ';
    ss << moveCounter;

    return ss.str();
}

std::string GameState::toVisualString() const {
    std::map<BoardPosition, ColoredPiece> positionToPiece = getPositionToPieceMap(pieces_);

    std::string boardTopBottom = "  ---------------------------------\n";
    std::string rowSeparator = "  |-------------------------------|\n";

    std::ostringstream ss;
    ss << boardTopBottom;

    for (int rank = 7; rank >= 0; --rank) {
        ss << rank + 1 << " |";
        for (int file = 0; file < 8; ++file) {
            ss << ' ';
            auto pieceIt = positionToPiece.find(positionFromFileRank(file, rank));
            if (pieceIt == positionToPiece.end()) {
                ss << ' ';
            }
            else {
                ss << toFenChar(pieceIt->second);
            }
            ss << " |";
        }
        ss << "\n";
        if (rank > 0) {
            ss << rowSeparator;
        }
    }
    ss << boardTopBottom;
    ss << "    a   b   c   d   e   f   g   h\n";

    return ss.str();
}

bool GameState::isInCheck() const {
    return isInCheck(generateEnemyControlledSquares(getPositionToPieceMap(pieces_)));
}

std::vector<Move> GameState::generateMoves() const {
    const std::map<BoardPosition, ColoredPiece> positionToPiece = getPositionToPieceMap(pieces_);
    const std::set<BoardPosition> enemeyControlledSquares = generateEnemyControlledSquares(positionToPiece);

    std::vector<Move> moves;

    for (const auto& [coloredPiece, position] : pieces_) {
        if (getSide(coloredPiece) != sideToMove_) {
            // Skip enemy pieces
            continue;
        }

        switch (getPiece(coloredPiece)) {
        case Piece::Pawn:
            generateSinglePawnMoves(position, enPassantTarget_, positionToPiece, moves);
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
            generateNormalKingMoves(
                position,
                positionToPiece,
                moves);
            break;
        default:
            assert(false);
            break;
        }
    }

    generateCastlingMoves(
        sideToMove_,
        canCastleKingSide(sideToMove_),
        canCastleQueenSide(sideToMove_),
        positionToPiece,
        enemeyControlledSquares,
        moves);

    // Remove moves that put us in check. Very slow!!
    for (auto moveIt = moves.begin(); moveIt != moves.end(); ) {
        GameState copyState(*this);
        copyState.makeMove(*moveIt);
        copyState.sideToMove_ = sideToMove_;
        if (copyState.isInCheck()) {
            *moveIt = moves.back();
            moves.pop_back();
        }
        else {
            ++moveIt;
        }
    }

    return moves;
}

void GameState::makeMove(Move move) {
    if (isCastle(move.flags)) {
        handleCastle(move);
    }
    else {
        handleSinglePieceMove(move);
    }
}

void GameState::handleCastle(Move move) {
    const auto [kingFromFile, kingFromRank] = fileRankFromPosition(move.from);
    const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
    const bool isQueenSide = kingToFile == 2; // c

    assert(IMPLIES(isQueenSide, canCastleQueenSide(sideToMove_)));
    assert(IMPLIES(!isQueenSide, canCastleKingSide(sideToMove_)));

    const int rookFromFile = isQueenSide ? /*a*/ 0 : /*h*/ 7;
    const BoardPosition rookFromPosition = positionFromFileRank(rookFromFile, kingFromRank);
    const BoardPosition rookToPosition = positionFromFileRank((kingFromFile + kingToFile) / 2, kingFromRank);

    for (auto& [coloredPiece, position] : pieces_) {
        if (position == move.from) {
            assert(getPiece(coloredPiece) == Piece::King);
            assert(getSide(coloredPiece) == sideToMove_);

            position = move.to;
        }
        else if (position == rookFromPosition) {
            assert(getPiece(coloredPiece) == Piece::Rook);
            assert(getSide(coloredPiece) == sideToMove_);

            position = rookToPosition;
        }
    }

    mayCastleQueenSide_[(std::size_t)sideToMove_] = false;
    mayCastleKingSide_[(std::size_t)sideToMove_] = false;

    sideToMove_ = nextSide(sideToMove_);
    enPassantTarget_ = BoardPosition::Invalid;
    ++plySinceCaptureOrPawn_;
}

void GameState::handleSinglePieceMove(Move move) {
    BoardPosition captureTargetSquare = BoardPosition::Invalid;

    if (isEnPassant(move.flags)) {
        assert(isCapture(move.flags));
        assert(move.to == enPassantTarget_);

        const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
        const auto [toFile, toRank] = fileRankFromPosition(move.to);
        captureTargetSquare = positionFromFileRank(toFile, fromRank);
    }
    else if (isCapture(move.flags)) {
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
            }
            else if (piece == Piece::King) {
                handleNormalKingMove();
            }
            else if (piece == Piece::Rook) {
                updateRookCastlingRights(move.from, sideToMove_);
            }
        }
        else if (position == captureTargetSquare) {
            assert(getSide(coloredPiece) != sideToMove_);
            assert(isCapture(move.flags));

            capturedPieceIt = pieceIt;
        }
    }

    assert(IMPLIES(isCapture(move.flags), capturedPieceIt != pieces_.end()));

    if (capturedPieceIt != pieces_.end()) {
        if (getPiece(capturedPieceIt->first) == Piece::Rook) {
            updateRookCastlingRights(captureTargetSquare, getSide(capturedPieceIt->first));
        }

        *capturedPieceIt = pieces_.back();
        pieces_.pop_back();
    }

    if (isCaptureOrPawnMove) {
        plySinceCaptureOrPawn_ = 0;
    }
    else {
        ++plySinceCaptureOrPawn_;
    }

    sideToMove_ = nextSide(sideToMove_);
}

void GameState::handlePawnMove(Move move, ColoredPiece& pieceToMove) {
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
    mayCastleKingSide_[(std::size_t)sideToMove_] = false;
    mayCastleQueenSide_[(std::size_t)sideToMove_] = false;
}

void GameState::updateRookCastlingRights(BoardPosition rookPosition, Side rookSide) {
    if (rookSide == Side::White && rookPosition == positionFromAlgebraic("a1")) {
        mayCastleQueenSide_[(std::size_t)rookSide] = false;
    }
    else if (rookSide == Side::White && rookPosition == positionFromAlgebraic("h1")) {
        mayCastleKingSide_[(std::size_t)rookSide] = false;
    }
    else if (rookSide == Side::Black && rookPosition == positionFromAlgebraic("a8")) {
        mayCastleQueenSide_[(std::size_t)rookSide] = false;
    }
    else if (rookSide == Side::Black && rookPosition == positionFromAlgebraic("h8")) {
        mayCastleKingSide_[(std::size_t)rookSide] = false;
    }
}

std::set<BoardPosition> GameState::generateEnemyControlledSquares(
    const std::map<BoardPosition, ColoredPiece>& positionToPiece
) const {
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
            generateSinglePawnMoves(
                position, BoardPosition::Invalid, positionToPiece, moves, /*getControlledSquares =*/ true);
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
            generateNormalKingMoves(
                position,
                positionToPiece,
                moves);
            break;
        default:
            assert(false);
            break;
        }
    }

    std::set<BoardPosition> controlledSquares;
    for (const auto& move : moves) {
        controlledSquares.insert(move.to);
    }

    return controlledSquares;
}

bool GameState::isInCheck(const std::set<BoardPosition>& enemeyControlledSquares) const {
    const ColoredPiece myKing = getColoredPiece(Piece::King, sideToMove_);
    for (const auto [piece, position] : pieces_) {
        if (piece == myKing) {
            return enemeyControlledSquares.count(position);
        }
    }
    assert(false);
    return {};
}
