#include "Move.h"

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