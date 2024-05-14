#include "ClangDiagnosticIgnore.h"

#pragma once

#include "GameState.h"

#include <map>

std::size_t perft(const GameState& gameState, int depth, StackOfVectors<Move>& stack);

std::size_t perftUnmake(GameState& gameState, int depth, StackOfVectors<Move>& stack);

std::size_t perftSplit(
        const GameState& gameState,
        int depth,
        int splitDepth,
        StackOfVectors<Move>& stack,
        std::map<std::string, std::size_t>& splitMap,
        const std::string& movePrefix = "");

void perftPrint(GameState& gameState, int maxDepth, bool useUnmake = false);

void perftSplitPrint(const GameState& gameState, int depth, int splitDepth);
