#include "Engine.h"

#include <random>

namespace {

StackOfVectors<Move> gMoveStack;

std::mt19937 gRandomGenerator;

}  // namespace

Move findMove(const GameState& gameState) {
    const auto moves = gameState.generateMoves(gMoveStack);

    std::uniform_int_distribution randomDistribution(0, moves.size() - 1);
    const int moveIndex    = randomDistribution(gRandomGenerator);
    const Move& moveToPlay = moves[moveIndex];

    return moveToPlay;
}