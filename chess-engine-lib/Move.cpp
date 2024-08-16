#include "Move.h"

#include "GameState.h"
#include "MyAssert.h"

#include <algorithm>
#include <format>

namespace {

[[nodiscard]] std::string algebraicFromPawnMove(const Move& move) {
    std::string result = "";

    if (isCapture(move.flags)) {
        result += (char)('a' + fileFromPosition(move.from));
        result += 'x';
    }
    result += algebraicFromPosition(move.to);

    if (isPromotion(move.flags)) {
        result += "=" + pieceToString(getPromotionPiece(move.flags));
    }

    return result;
}

[[nodiscard]] std::string algebraicFromKingMove(const Move& move) {
    const auto [kingFromFile, kingFromRank] = fileRankFromPosition(move.from);
    const auto [kingToFile, kingToRank]     = fileRankFromPosition(move.to);

    if (std::abs(kingFromFile - kingToFile) == 2) {
        const bool isQueenSide = kingToFile == 2;  // c
        return isQueenSide ? "O-O-O" : "O-O";
    }

    return std::format("K{}", algebraicFromPosition(move.to));
}

[[nodiscard]] std::string algebraicFromPieceMove(
        const Move& move, const GameState& gameState, StackOfVectors<Move>& stack) {
    const StackVector<Move> moves = gameState.generateMoves(stack);

    StackVector<Move> ambiguousMoves = stack.makeStackVector();
    std::copy_if(
            moves.begin(),
            moves.end(),
            std::back_inserter(ambiguousMoves),
            [&move](const Move& otherMove) {
                return otherMove.pieceToMove == move.pieceToMove && otherMove.to == move.to
                    && otherMove.from != move.from;
            });
    ambiguousMoves.lock();

    std::string result = pieceToString(move.pieceToMove);

    if (ambiguousMoves.size() > 0) {
        const int numSameFile = std::count_if(
                ambiguousMoves.begin(), ambiguousMoves.end(), [&move](const Move& otherMove) {
                    return fileFromPosition(otherMove.from) == fileFromPosition(move.from);
                });

        const int numSameRank = std::count_if(
                ambiguousMoves.begin(), ambiguousMoves.end(), [&move](const Move& otherMove) {
                    return rankFromPosition(otherMove.from) == rankFromPosition(move.from);
                });

        if (numSameFile == 0) {
            result += (char)('a' + fileFromPosition(move.from));
        } else if (numSameRank == 0) {
            result += (char)('1' + rankFromPosition(move.from));
        } else {
            result += algebraicFromPosition(move.from);
        }
    }

    if (isCapture(move.flags)) {
        result += 'x';
    }

    result += algebraicFromPosition(move.to);

    return result;
}

}  // namespace

std::string Move::toAlgebraic(const GameState& gameState) const {
    StackOfVectors<Move> stack;

    std::string algebraic;

    switch (pieceToMove) {
        case Piece::Pawn:
            algebraic = algebraicFromPawnMove(*this);
            break;
        case Piece::King:
            algebraic = algebraicFromKingMove(*this);
            break;
        default:
            algebraic = algebraicFromPieceMove(*this, gameState, stack);
            break;
    }

    GameState copyState(gameState);
    copyState.makeMove(*this);

    const bool isCheck     = copyState.isInCheck();
    const bool isCheckMate = isCheck && copyState.generateMoves(stack).empty();

    if (isCheckMate) {
        algebraic += '#';
    } else if (isCheck) {
        algebraic += '+';
    }

    return algebraic;
}

std::string Move::toUci() const {
    std::string uciString = algebraicFromPosition(from) + algebraicFromPosition(to);

    if (auto promotionPiece = getPromotionPiece(flags); promotionPiece != Piece::Pawn) {
        uciString += toLowerCaseFenChar(promotionPiece);
    }

    return uciString;
}

std::string Move::toExtendedString() const {
    if (isCastle(flags)) {
        const auto [kingToFile, kingToRank] = fileRankFromPosition(to);
        const bool isQueenSide              = kingToFile == 2;  // c
        return isQueenSide ? "O-O-O" : "O-O";
    }

    const char positionSeparator = isCapture(flags) ? 'x' : '-';
    const Piece promotionPiece   = getPromotionPiece(flags);
    const std::string promotionString =
            promotionPiece == Piece::Pawn ? "" : std::format("={}", pieceToString(promotionPiece));

    const std::string enPassant = isEnPassant(flags) ? " e.p." : "";

    return pieceToString(pieceToMove) + algebraicFromPosition(from) + positionSeparator
         + algebraicFromPosition(to) + promotionString + enPassant;
}

Move Move::fromUci(std::string_view uciString, const GameState& gameState) {
    if (uciString.size() != 4 && uciString.size() != 5) [[unlikely]] {
        throw std::invalid_argument(std::format("Invalid UCI string: {}", uciString));
    }

    const BoardPosition from = positionFromAlgebraic(uciString.substr(0, 2));
    const BoardPosition to   = positionFromAlgebraic(uciString.substr(2, 2));

    Piece promotionPiece = Piece::Pawn;
    if (uciString.length() == 5) {
        promotionPiece = pieceFromFenChar(uciString[4]);
    }

    const Piece pieceToMove = getPiece(gameState.getPieceOnSquare(from));

    MoveFlags moveFlags = (MoveFlags)promotionPiece;

    if (pieceToMove == Piece::Pawn && to == gameState.getEnPassantTarget()) {
        moveFlags = moveFlags | MoveFlags::IsCapture | MoveFlags::IsEnPassant;
    } else if (getPiece(gameState.getPieceOnSquare(to)) != Piece::Invalid) {
        moveFlags = moveFlags | MoveFlags::IsCapture;
    }

    if (pieceToMove == Piece::King) {
        const auto [kingFromFile, _1] = fileRankFromPosition(from);
        const auto [kintToFile, _2]   = fileRankFromPosition(to);

        if (std::abs(kingFromFile - kintToFile) == 2) {
            moveFlags = moveFlags | MoveFlags::IsCastle;
        }
    }

    return Move{.pieceToMove = pieceToMove, .from = from, .to = to, .flags = moveFlags};
}

Move Move::fromAlgebraic(std::string_view algebraic, const GameState& gameState) {
    StackOfVectors<Move> stack;

    const StackVector<Move> moves = gameState.generateMoves(stack);
    for (const auto& move : moves) {
        if (move.toAlgebraic(gameState) == algebraic) {
            return move;
        }
    }
    throw std::invalid_argument(
            std::format("Move {} is not a legal move in this position.", algebraic));
}

void doBasicSanityChecks(const Move& move, const GameState& gameState) {
    if (move.pieceToMove == Piece::Invalid) [[unlikely]] {
        throw std::invalid_argument("Invalid piece to move.");
    }

    const Side sideToMove = gameState.getSideToMove();

    const ColoredPiece coloredPieceToMove = gameState.getPieceOnSquare(move.from);
    if (getSide(coloredPieceToMove) != sideToMove) [[unlikely]] {
        throw std::invalid_argument("Piece to move is of the wrong side.");
    }

    if (isCapture(move.flags)) {
        if (isEnPassant(move.flags)) {
            if (move.pieceToMove != Piece::Pawn) [[unlikely]] {
                throw std::invalid_argument("Only pawns can capture en passant.");
            }
            if (move.to != gameState.getEnPassantTarget()) [[unlikely]] {
                throw std::invalid_argument("En passant target square is incorrect.");
            }
        }

        const BoardPosition captureTarget =
                isEnPassant(move.flags) ? getEnPassantPiecePosition(move.to, sideToMove) : move.to;

        const ColoredPiece capturedPiece = gameState.getPieceOnSquare(captureTarget);
        if (getPiece(capturedPiece) == Piece::Invalid) [[unlikely]] {
            throw std::invalid_argument(std::format(
                    "No piece to capture on the capture target square {}.",
                    algebraicFromPosition(captureTarget)));
        }
        if (getSide(capturedPiece) == sideToMove) [[unlikely]] {
            throw std::invalid_argument("Capture target is of own side.");
        }
    } else {
        const Piece targetPiece = getPiece(gameState.getPieceOnSquare(move.to));
        if (targetPiece != Piece::Invalid) [[unlikely]] {
            throw std::invalid_argument("Target square for non-capture is occupied.");
        }
    }

    if (isPromotion(move.flags)) {
        if (move.pieceToMove != Piece::Pawn) [[unlikely]] {
            throw std::invalid_argument("Only pawns can be promoted.");
        }

        const int expectedRank = sideToMove == Side::White ? 7 : 0;
        if (rankFromPosition(move.to) != expectedRank) [[unlikely]] {
            throw std::invalid_argument("Only pawns on the last rank can be promoted.");
        }
    }
}
