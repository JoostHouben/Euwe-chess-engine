#include "Search.h"

namespace {

bool gRecordBestMove = false;
Move gBestMove;

[[nodiscard]] EvalT search(GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    if (depth == 0) {
        return evaluate(gameState, stack);
    }

    if (gameState.isForcedDraw()) {
        return 0;
    }

    auto moves = gameState.generateMoves(stack);
    if (moves.size() == 0) {
        return evaluateNoLegalMoves(gameState);
    }

    Move bestMove;
    EvalT bestEval = -kInfiniteEval;

    const bool recordBestMove = gRecordBestMove;
    gRecordBestMove           = false;

    for (Move move : moves) {
        auto unmakeInfo = gameState.makeMove(move);

        const EvalT score = -search(gameState, depth - 1, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (score > bestEval) {
            bestEval = score;
            bestMove = move;
        }
    }

    if (recordBestMove) {
        gBestMove = bestMove;
    }

    return bestEval;
}

}  // namespace

SearchResult searchForBestMove(GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    gRecordBestMove = true;

    const EvalT eval = search(gameState, depth, stack);

    return {.bestMove = gBestMove, .eval = eval};
}
