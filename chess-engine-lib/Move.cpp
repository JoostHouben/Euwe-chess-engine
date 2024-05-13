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
                return otherMove.pieceToMove == move.pieceToMove && otherMove.to == move.to &&
                       otherMove.from != move.from;
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

Move moveFromAlgebraic(std::string_view algebraic, const GameState& gameState) {
    StackOfVectors<Move> stack;

    const StackVector<Move> moves = gameState.generateMoves(stack);
    for (const auto& move : moves) {
        if (algebraicFromMove(move, gameState) == algebraic) {
            return move;
        }
    }
    UNREACHABLE;
}

std::string algebraicFromMove(const Move& move, const GameState& gameState) {
    StackOfVectors<Move> stack;

    std::string algebraic;

    switch (move.pieceToMove) {
        case Piece::Pawn:
            algebraic = algebraicFromPawnMove(move);
            break;
        case Piece::King:
            algebraic = algebraicFromKingMove(move);
            break;
        default:
            algebraic = algebraicFromPieceMove(move, gameState, stack);
            break;
    }

    GameState copyState(gameState);
    copyState.makeMove(move);

    const bool isCheck     = copyState.isInCheck();
    const bool isCheckMate = isCheck && copyState.generateMoves(stack).empty();

    if (isCheckMate) {
        algebraic += '#';
    } else if (isCheck) {
        algebraic += '+';
    }

    return algebraic;
}

std::string moveToExtendedString(const Move& move) {
    if (isCastle(move.flags)) {
        const auto [kingToFile, kingToRank] = fileRankFromPosition(move.to);
        const bool isQueenSide              = kingToFile == 2;  // c
        return isQueenSide ? "O-O-O" : "O-O";
    }

    const char positionSeparator = isCapture(move.flags) ? 'x' : '-';
    const Piece promotionPiece   = getPromotionPiece(move.flags);
    const std::string promotionString =
            promotionPiece == Piece::Pawn ? "" : std::format("={}", pieceToString(promotionPiece));

    const std::string enPassant = isEnPassant(move.flags) ? " e.p." : "";

    return pieceToString(move.pieceToMove) + algebraicFromPosition(move.from) + positionSeparator +
           algebraicFromPosition(move.to) + promotionString + enPassant;
}

std::string moveToUciString(const Move& move) {
    std::string uciString = algebraicFromPosition(move.from) + algebraicFromPosition(move.to);

    if (auto promotionPiece = getPromotionPiece(move.flags); promotionPiece != Piece::Pawn) {
        uciString += toLowerCaseFenChar(promotionPiece);
    }

    return uciString;
}

Move moveFromUciString(std::string_view uciString, const GameState& gameState) {
    const BoardPosition from = positionFromAlgebraic(uciString.substr(0, 2));
    const BoardPosition to   = positionFromAlgebraic(uciString.substr(2, 2));

    Piece promotionPiece = Piece::Pawn;
    if (uciString.length() == 5) {
        promotionPiece = pieceFromFenChar(uciString[4]);
    }

    const Piece pieceToMove = getPiece(gameState.getPieceOnSquare(from));

    MoveFlags moveFlags = getFlags(promotionPiece);

    if (pieceToMove == Piece::Pawn && to == gameState.getEnPassantTarget()) {
        moveFlags = getFlags(moveFlags, MoveFlags::IsCapture, MoveFlags::IsEnPassant);
    } else if (getPiece(gameState.getPieceOnSquare(to)) != Piece::Invalid) {
        moveFlags = getFlags(moveFlags, MoveFlags::IsCapture);
    }

    if (pieceToMove == Piece::King) {
        const auto [kingFromFile, _1] = fileRankFromPosition(from);
        const auto [kintToFile, _2]   = fileRankFromPosition(to);

        if (std::abs(kingFromFile - kintToFile) == 2) {
            moveFlags = getFlags(moveFlags, MoveFlags::IsCastle);
        }
    }

    return Move{.pieceToMove = pieceToMove, .from = from, .to = to, .flags = moveFlags};
}
