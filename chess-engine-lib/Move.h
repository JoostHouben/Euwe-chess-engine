#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BoardPosition.h"
#include "Piece.h"

#include <format>

#include <cstdint>

enum class MoveFlags : std::uint8_t {
    None = 0,
    // Lowest 3 bits: Piece if promoting
    IsCapture   = 1 << 3,
    IsEnPassant = 1 << 4,
    IsCastle    = 1 << 5,
};

[[nodiscard]] constexpr Piece getPromotionPiece(MoveFlags flags) {
    return static_cast<Piece>((int)flags & 7);
}

[[nodiscard]] constexpr bool isPromotion(MoveFlags flags) {
    return getPromotionPiece(flags) != Piece::Pawn;
}

[[nodiscard]] constexpr bool isCapture(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsCapture;
}

[[nodiscard]] constexpr bool isEnPassant(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsEnPassant;
}

[[nodiscard]] constexpr bool isCastle(MoveFlags flags) {
    return (int)flags & (int)MoveFlags::IsCastle;
}

template <typename... FlagTs>
[[nodiscard]] constexpr MoveFlags getFlags(FlagTs... flags) {
    static_assert(((std::is_same_v<FlagTs, MoveFlags> || std::is_same_v<FlagTs, Piece>)&&...));
    return static_cast<MoveFlags>((static_cast<int>(flags) | ...));
}

struct Move {
    Piece pieceToMove  = Piece::Invalid;
    BoardPosition from = BoardPosition::Invalid;
    BoardPosition to   = BoardPosition::Invalid;
    MoveFlags flags    = MoveFlags::None;

    bool operator==(const Move& other) const = default;
};

class GameState;

Move moveFromAlgebraic(std::string_view algebraic,
                       const GameState& gameState);  // TODO

std::string algebraicFromMove(Move move, const GameState& gameState);  // TODO

// Long algebraic notation for moves, except no indicators for check or checkmate
// {piece}{from}-{to}
// If capture: use 'x' instead of '-'
// For promotions: suffix '={promotion piece}'
// For en passant: suffix ' e.p.'
// For castling: normal algebraic notation
// Examples: Pe2-e4, Rd3xd7, Pe5xd6 e.p.
[[nodiscard]] std::string moveToExtendedString(const Move& move);

[[nodiscard]] std::string moveToUciString(const Move& move);
[[nodiscard]] Move moveFromUciString(std::string_view uciString, const GameState& gameState);
