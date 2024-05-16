#include "Search.h"

#include <atomic>

namespace {

bool gRecordBestMove = false;
Move gBestMove;
std::atomic<bool> gStopSearch;

[[nodiscard]] EvalT search(
        GameState& gameState,
        const int depth,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack) {
    if (gStopSearch) {
        return 0;
    }

    if (depth == 0) {
        return evaluate(gameState, stack);
    }

    const bool recordBestMove = gRecordBestMove;
    gRecordBestMove           = false;

    if (!recordBestMove && gameState.isForcedDraw(/* repetitionsForDraw = */ 1)) {
        return 0;
    }

    auto moves = gameState.generateMoves(stack);
    if (moves.size() == 0) {
        return evaluateNoLegalMoves(gameState);
    }

    Move bestMove;

    for (Move move : moves) {
        auto unmakeInfo = gameState.makeMove(move);

        const EvalT score = -search(gameState, depth - 1, -beta, -alpha, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (gStopSearch) {
            break;
        }

        if (score > alpha) {
            alpha    = score;
            bestMove = move;

            if (alpha >= beta) {
                break;
            }
        }
    }

    if (recordBestMove) {
        gBestMove = bestMove;
    }

    return alpha;
}

}  // namespace

SearchResult searchForBestMove(GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    gRecordBestMove = true;
    gStopSearch     = false;

    const EvalT eval = search(gameState, depth, -kInfiniteEval, kInfiniteEval, stack);

    return {.bestMove = gBestMove, .eval = eval};
}

void stopSearch() {
    gStopSearch = true;
}
