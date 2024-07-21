#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

#include <array>
#include <numeric>
#include <optional>

#include <cstdint>

using EvalT = std::int16_t;

inline constexpr EvalT kInfiniteEval = std::numeric_limits<EvalT>::max();
inline constexpr EvalT kMateEval     = (EvalT)30'000;

[[nodiscard]] int getPieceValue(Piece piece);

[[nodiscard]] int getPieceSquareValue(Piece piece, BoardPosition position, Side side);

[[nodiscard]] bool isInsufficientMaterial(const GameState& gameState);

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

[[nodiscard]] EvalT evaluate(
        const GameState& gameState, StackOfVectors<Move>& stack, bool checkEndState = true);

[[nodiscard]] bool isMate(EvalT eval);
[[nodiscard]] bool isValid(EvalT eval);

[[nodiscard]] int getMateDistanceInPly(EvalT eval);
