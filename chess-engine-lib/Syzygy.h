#pragma once

#include <optional>
#include <string_view>
#include <vector>

#include "EvalT.h"
#include "GameState.h"
#include "Move.h"

[[nodiscard]] char getSyzygyPathSeparator();

[[nodiscard]] bool syzygyPathIsValid(std::string_view syzygyDirs);

[[nodiscard]] int initSyzygy(const std::string& syzygyDirs);

void tearDownSyzygy();

[[nodiscard]] bool canProbeSyzgyRoot(const GameState& gameState);

[[nodiscard]] std::vector<Move> getSyzygyRootMoves(const GameState& gameState);

[[nodiscard]] bool canProbeSyzgyWdl(const GameState& gameState);

[[nodiscard]] bool canProbeSyzgyWdl(
        int plySinceCaptureOrPawn, GameState::CastlingRights castlingRights, int numPieces);

[[nodiscard]] std::optional<EvalT> probeSyzygyWdl(const GameState& gameState);
