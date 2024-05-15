#include "Engine.h"

#include "Search.h"

#include <random>

namespace {

StackOfVectors<Move> gMoveStack;

std::mt19937 gRandomGenerator;

}  // namespace

Move findMove(const GameState& gameState) {
    GameState copySate(gameState);
    const Move moveToPlay = searchForBestMove(copySate, 4, gMoveStack).bestMove;
    return moveToPlay;
}