#include "Engine.h"

#include "Eval.h"

#include <random>

namespace {

StackOfVectors<Move> gMoveStack;

std::mt19937 gRandomGenerator;

}  // namespace

Move findMove(const GameState& gameState) {
    const auto moves = gameState.generateMoves(gMoveStack);

    // Find the moves that create the lowest eval for the opponent.
    EvalT bestEval              = kInfiniteEval;
    StackVector<Move> bestMoves = gMoveStack.makeStackVector();

    for (const Move& move : moves) {
        GameState copyState(gameState);
        (void)copyState.makeMove(move);
        const EvalT eval = evaluate(copyState, gMoveStack);

        if (eval < bestEval) {
            bestEval = eval;
            bestMoves.clear();
            bestMoves.push_back(move);
        } else if (eval == bestEval) {
            bestMoves.push_back(move);
        }
    }

    bestMoves.lock();

    // If there's a tie, choose a random move between them.
    std::uniform_int_distribution randomDistribution(0, bestMoves.size() - 1);
    const int moveIndex    = randomDistribution(gRandomGenerator);
    const Move& moveToPlay = bestMoves[moveIndex];

    return moveToPlay;
}