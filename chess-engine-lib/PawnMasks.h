#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BitBoard.h"
#include "Side.h"

[[nodiscard]] BitBoard getPassedPawnOpponentMask(BoardPosition position, Side side);

[[nodiscard]] BitBoard getPassedPawnOwnMask(BoardPosition position, Side side);
