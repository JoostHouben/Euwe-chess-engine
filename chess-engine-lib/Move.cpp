#include "Move.h"

#include "GameState.h"

[[nodiscard]] std::string moveToExtendedString(const Move& move) {
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

[[nodiscard]] std::string moveToUciString(const Move& move) {
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
