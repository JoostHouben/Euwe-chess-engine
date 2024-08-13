#include "MoveSearcher.h"

#include "Eval.h"
#include "Math.h"
#include "MoveOrder.h"
#include "SEE.h"
#include "TTable.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <bit>
#include <limits>

#include <cstdint>

class MoveSearcher::Impl {
  public:
    Impl(const TimeManager& timeManager);

    void setFrontEnd(IFrontEnd* frontEnd);

    void newGame();

    [[nodiscard]] RootSearchResult searchForBestMove(
            GameState& gameState,
            int depth,
            StackOfVectors<Move>& stack,
            std::optional<EvalT> evalGuess = std::nullopt);

    void prepareForNewSearch(const GameState& gameState);

    void interruptSearch();

    [[nodiscard]] SearchStatistics getSearchStatistics() const;

    void resetSearchStatistics();

    void setTTableSize(int requestedSizeInMb);

  private:
    // == Types ==

    // Outcome of searching a single move; signals to main search whether to continue or stop.
    enum class SearchMoveOutcome {
        Continue,
        Cutoff,
        Interrupted,
    };

    // == Helper functions ==

    [[nodiscard]] std::array<Move, 2>& getKillerMoves(int ply);
    void storeKillerMove(const Move& move, int ply);

    [[nodiscard]] Move getCounterMove(const Move& move, Side side);
    void storeCounterMove(const Move& lastMove, const Move& counter, Side side);

    [[nodiscard]] static int getHistoryWeight(int depth);
    void updateHistoryForCutoff(const Move& move, int depth, Side side);
    void updateHistoryForUse(const Move& move, int depth, Side side);

    // Write updated information to the ttable.
    void updateTTable(
            EvalT bestScore,
            EvalT alphaOrig,
            EvalT beta,
            bool stoppedEarly,
            Move bestMove,
            int depth,
            HashT hash);

    // Extract the principal variation from the transposition table.
    [[nodiscard]] StackVector<Move> extractPv(
            GameState gameState, StackOfVectors<Move>& stack, int depth);

    void shiftKillerMoves(int halfMoveClock);
    void initializeHistoryFromPieceSquare();
    void scaleDownHistory();

    [[nodiscard]] bool shouldStopSearch() const;

    // == Search functions ==

    // Main search function: alpha-beta search with negamax and transposition table.
    //
    // If returned value s satisfies alpha < s < beta, the value is exact.
    // If s <= alpha, the value is an upper bound.
    // If beta <= s, the value is a lower bound.
    // If stopSearch_ is true, returns std::nullopt
    [[nodiscard]] EvalT search(
            GameState& gameState,
            int depth,
            int ply,
            EvalT alpha,
            EvalT beta,
            Move lastMove,
            int lastNullMovePly,
            StackOfVectors<Move>& stack);

    // Quiescence search. When in check search all moves, when not in check only search captures.
    // Continue until no more capture are available or we get a beta cutoff.
    // When not in check use a stand pat evaluation to set alpha and possibly get a beta cutoff.
    [[nodiscard]] EvalT quiesce(
            GameState& gameState,
            EvalT alpha,
            EvalT beta,
            StackOfVectors<Move>& stack,
            bool returnIfNotInCheck = false);

    // Subroutine for search.
    // Search a single move, updating alpha, bestScore and bestMove as necessary.
    [[nodiscard]] SearchMoveOutcome searchMove(
            GameState& gameState,
            const Move& move,
            int depth,
            int reduction,
            int ply,
            EvalT& alpha,
            EvalT beta,
            StackOfVectors<Move>& stack,
            EvalT& bestScore,
            Move& bestMove,
            const Move& lastMove,
            int lastNullMovePly,
            bool useScoutSearch);

    // Perform an aspiration window search.
    [[nodiscard]] RootSearchResult aspirationWindowSearch(
            GameState& gameState,
            const int depth,
            StackOfVectors<Move>& stack,
            const EvalT initialGuess);

    // == Constants ==

    static constexpr int kMaxDepth = 100;

    // == Data ==

    std::atomic<bool> stopSearch_ = false;
    mutable bool wasInterrupted_  = false;

    SearchTTable tTable_ = {};

    StackOfVectors<MoveEvalT> moveScoreStack_ = {};

    SearchStatistics searchStatistics_ = {};

    int moveClockForKillerMoves_                            = 0;
    std::array<std::array<Move, 2>, kMaxDepth> killerMoves_ = {};

    std::array<std::array<std::array<Move, kSquares>, kNumPieceTypes>, kNumSides> counterMoves_ =
            {};

    std::array<std::array<std::array<unsigned, kSquares>, kNumPieceTypes>, kNumSides>
            historyCutOff_ = {};
    std::array<std::array<std::array<unsigned, kSquares>, kNumPieceTypes>, kNumSides> historyUsed_ =
            {};

    const IFrontEnd* frontEnd_ = nullptr;

    const TimeManager& timeManager_;
};

namespace {

// Subroutine for search and quiescence search.
// Select best move based on pre-calculated scores using a simple linear search.
// Then do a 'destructive swap' with the first move in the list and return the best move.
// This basically ends up doing a selection sort when called repeatedly, except that we don't
// actually write the best moves to the front of the list.
[[nodiscard]] FORCE_INLINE Move
selectBestMove(StackVector<Move>& moves, StackVector<MoveEvalT>& moveScores, int firstMoveIdx) {
    int bestMoveIdx         = -1;
    MoveEvalT bestMoveScore = std::numeric_limits<MoveEvalT>::lowest();

    for (int moveIdx = firstMoveIdx; moveIdx < moveScores.size(); ++moveIdx) {
        if (moveScores[moveIdx] > bestMoveScore) {
            bestMoveScore = moveScores[moveIdx];
            bestMoveIdx   = moveIdx;
        }
    }

    const Move bestMove = moves[bestMoveIdx];

    // 'Destructive swap'
    moves[bestMoveIdx]      = moves[firstMoveIdx];
    moveScores[bestMoveIdx] = moveScores[firstMoveIdx];

    return bestMove;
}

[[nodiscard]] FORCE_INLINE bool nullMovePruningAllowed(
        const GameState& gameState,
        const bool isInCheck,
        const int depth,
        const int ply,
        const int lastNullMovePly) {
    const bool basicConditions = !isInCheck && ply > 0 && depth >= 3 && ply != lastNullMovePly + 2;
    if (!basicConditions) {
        return false;
    }

    // Allow null move only if the side to move has any (non-pawn) pieces.
    // Having pieces makes zugzwang less likely.
    const Side sideToMove = gameState.getSideToMove();
    const BitBoard piecesBitBoard =
            any(gameState.getPieceBitBoard(sideToMove, Piece::Knight),
                gameState.getPieceBitBoard(sideToMove, Piece::Bishop),
                gameState.getPieceBitBoard(sideToMove, Piece::Rook),
                gameState.getPieceBitBoard(sideToMove, Piece::Queen));

    return piecesBitBoard != BitBoard::Empty;
}

[[nodiscard]] FORCE_INLINE int getDepthReduction(
        const Move& move,
        const int moveIdx,
        const bool isPvNode,
        const int depth,
        const bool isInCheck) {
    static constexpr int kMovesSearchedFullDepth = 4;
    static constexpr int kMinDepthForReduction   = 3;

    const Piece promotionPiece = getPromotionPiece(move.flags);
    const bool moveIsTactical  = isCapture(move.flags) || promotionPiece == Piece::Queen;
    const bool tooShallow      = depth < kMinDepthForReduction;

    if (isInCheck || isPvNode || moveIdx < kMovesSearchedFullDepth || moveIsTactical
        || tooShallow) {
        return 0;
    }

    return 1;
}

}  // namespace

MoveSearcher::Impl::Impl(const TimeManager& timeManager) : timeManager_(timeManager) {
    moveScoreStack_.reserve(1'000);
    initializeHistoryFromPieceSquare();
}

void MoveSearcher::Impl::setFrontEnd(IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;
}

void MoveSearcher::Impl::newGame() {
    // Reset internal state for the sake of consistency

    stopSearch_     = false;
    wasInterrupted_ = false;

    tTable_.clear();

    resetSearchStatistics();

    moveClockForKillerMoves_ = 0;
    killerMoves_             = {};
    counterMoves_            = {};

    initializeHistoryFromPieceSquare();
}

FORCE_INLINE std::array<Move, 2>& MoveSearcher::Impl::getKillerMoves(const int ply) {
    MY_ASSERT(ply < kMaxDepth);
    return killerMoves_[ply];
}

FORCE_INLINE void MoveSearcher::Impl::storeKillerMove(const Move& move, const int ply) {
    if (isCapture(move.flags) || isPromotion(move.flags)) {
        // Only store 'quiet' moves as killer moves.
        return;
    }

    auto& plyKillerMoves = getKillerMoves(ply);

    if (move == plyKillerMoves[0]) {
        // Don't store the same move twice.
        return;
    }

    // Shift killer moves down and store the new move at the front.
    plyKillerMoves[1] = plyKillerMoves[0];
    plyKillerMoves[0] = move;
}

FORCE_INLINE Move MoveSearcher::Impl::getCounterMove(const Move& move, const Side side) {
    if (move.pieceToMove == Piece::Invalid) {
        return {};
    }
    return counterMoves_[(int)side][(int)move.pieceToMove][(int)move.to];
}

FORCE_INLINE void MoveSearcher::Impl::storeCounterMove(
        const Move& lastMove, const Move& counter, const Side side) {
    if (isCapture(counter.flags) || isPromotion(counter.flags)) {
        // Only store 'quiet' moves as counter moves.
        return;
    }
    if (lastMove.pieceToMove == Piece::Invalid) {
        return;
    }
    counterMoves_[(int)side][(int)lastMove.pieceToMove][(int)lastMove.to] = counter;
}

FORCE_INLINE int MoveSearcher::Impl::getHistoryWeight(const int depth) {
    return depth * depth;
}

FORCE_INLINE void MoveSearcher::Impl::updateHistoryForCutoff(
        const Move& move, const int depth, const Side side) {
    if (isCapture(move.flags) || isPromotion(move.flags)) {
        // Only update history for 'quiet' moves.
        return;
    }

    const int square = (int)move.to;
    const int piece  = (int)move.pieceToMove;

    historyCutOff_[(int)side][piece][square] += getHistoryWeight(depth);
}

FORCE_INLINE void MoveSearcher::Impl::updateHistoryForUse(
        const Move& move, const int depth, const Side side) {
    if (isCapture(move.flags) || isPromotion(move.flags)) {
        // Only update history for 'quiet' moves.
        return;
    }

    const int square = (int)move.to;
    const int piece  = (int)move.pieceToMove;

    historyUsed_[(int)side][piece][square] += getHistoryWeight(depth);
}

FORCE_INLINE void MoveSearcher::Impl::updateTTable(
        EvalT bestScore,
        EvalT alphaOrig,
        EvalT beta,
        bool stoppedEarly,
        Move bestMove,
        int depth,
        HashT hash) {
    ScoreType scoreType;
    if (stoppedEarly) {
        // Don't trust scores from partial search.
        scoreType = ScoreType::NotSet;
    } else if (bestScore <= alphaOrig) {
        // Best score is below original feasibility window, so it's an upper bound.
        scoreType = ScoreType::UpperBound;
    } else if (bestScore >= beta) {
        // A score above beta was obtained from a subcall that failed high, so that result is an
        // upper bound. This is true regardless of whether the score is above the original beta or
        // a tightened beta.
        scoreType = ScoreType::LowerBound;
    } else {
        // Score is in the feasibility window, so exact.
        scoreType = ScoreType::Exact;
    }

    const SearchTTable::EntryT entry = {
            .hash    = hash,
            .payload = {
                    .depth     = (std::uint8_t)depth,
                    .scoreType = scoreType,
                    .score     = bestScore,
                    .bestMove  = bestMove,
            }};

    auto isMoreValuable = [](const SearchTTPayload& newPayload, const SearchTTPayload& oldPayload) {
        return newPayload.depth >= oldPayload.depth;
    };

    tTable_.store(entry, isMoreValuable);
}

StackVector<Move> MoveSearcher::Impl::extractPv(
        GameState gameState, StackOfVectors<Move>& stack, const int depth) {
    // Note: taking copy of gameState
    // TODO: would make+unmake be faster here?

    StackVector<Move> pv = stack.makeStackVector();

    while (pv.size() < depth) {
        const auto ttHit = tTable_.probe(gameState.getBoardHash());

        if (!ttHit) {
            break;
        }

        pv.push_back(ttHit->payload.bestMove);
        (void)gameState.makeMove(ttHit->payload.bestMove);
    }

    pv.lock();
    return pv;
}

void MoveSearcher::Impl::shiftKillerMoves(const int halfMoveClock) {
    const int shiftAmount = halfMoveClock - moveClockForKillerMoves_;

    for (int ply = 0; ply < kMaxDepth - shiftAmount; ++ply) {
        killerMoves_[ply] = killerMoves_[ply + shiftAmount];
    }

    moveClockForKillerMoves_ = halfMoveClock;
}

void MoveSearcher::Impl::initializeHistoryFromPieceSquare() {
    static constexpr int kNumScaleBits        = 7;   // 128
    static constexpr int kPieceSquareBiasBits = 16;  // ~65k

    for (int side = 0; side < kNumSides; ++side) {
        for (int piece = 0; piece < kNumPieceTypes; ++piece) {
            for (int square = 0; square < kSquares; ++square) {
                int pieceSquareValue =
                        getPieceSquareValue((Piece)piece, (BoardPosition)square, (Side)side);
                pieceSquareValue += 50;  // Get rid of negative values

                historyCutOff_[side][piece][square] = pieceSquareValue
                                                   << (kPieceSquareBiasBits - kNumScaleBits);

                historyUsed_[side][piece][square] = 1 << kPieceSquareBiasBits;
            }
        }
    }
}

void MoveSearcher::Impl::scaleDownHistory() {
    static constexpr int kScaleDownBits = 4;   // 16
    static constexpr int kTargetBits    = 10;  // 1024
    static constexpr int kCountlTarget  = 32 - kTargetBits;

    for (int side = 0; side < kNumSides; ++side) {
        for (int piece = 0; piece < kNumPieceTypes; ++piece) {
            for (int square = 0; square < kSquares; ++square) {
                unsigned& historyUsed       = historyUsed_[side][piece][square];
                const int historyCountlZero = std::countl_zero(historyUsed);

                const int shiftAmount = clamp(historyCountlZero - kCountlTarget, 0, kScaleDownBits);

                historyUsed >>= shiftAmount;
                historyCutOff_[side][piece][square] >>= shiftAmount;
            }
        }
    }
}

bool MoveSearcher::Impl::shouldStopSearch() const {
    const std::uint64_t numNodes =
            searchStatistics_.normalNodesSearched + searchStatistics_.qNodesSearched;
    wasInterrupted_ =
            wasInterrupted_ || stopSearch_ || timeManager_.shouldInterruptSearch(numNodes);
    return wasInterrupted_;
}

EvalT MoveSearcher::Impl::search(
        GameState& gameState,
        int depth,
        const int ply,
        EvalT alpha,
        EvalT beta,
        Move lastMove,
        const int lastNullMovePly,
        StackOfVectors<Move>& stack) {
    if (shouldStopSearch()) {
        return -kInfiniteEval;
    }

    ++searchStatistics_.normalNodesSearched;
    searchStatistics_.selectiveDepth = max(searchStatistics_.selectiveDepth, ply);

    if (depth == 0) {
        return quiesce(gameState, alpha, beta, stack);
    }

    // alphaOrig determines whether the value returned is an upper bound
    const EvalT alphaOrig = alpha;

    if (ply > 0) {
        if (gameState.isRepetition(/* repetitionsForDraw = */ 1)) {
            // Exact value
            return 0;
        }

        if (gameState.isFiftyMoves()) {
            const auto moves = gameState.generateMoves(stack);
            if (moves.size() == 0) {
                // Exact value
                return evaluateNoLegalMoves(gameState);
            } else {
                // Exact value
                return 0;
            }
        }

        if (isInsufficientMaterial(gameState)) {
            // Exact value
            return 0;
        }
    }

    const BitBoard enemyControl = gameState.getEnemyControl();
    const bool isInCheck        = gameState.isInCheck(enemyControl);

    const bool isPvNode = beta - alpha > 1;

    if (isInCheck) {
        // Check extension
        depth += 1;
    }

    constexpr int kNullMoveReduction = 3;
    if (nullMovePruningAllowed(gameState, isInCheck, depth, ply, lastNullMovePly)) {
        const int nullMoveSearchDepth = max(1, depth - kNullMoveReduction - 1);

        const auto unmakeInfo = gameState.makeNullMove();

        const EvalT nullMoveScore =
                -search(gameState,
                        nullMoveSearchDepth,
                        ply + 1,
                        -beta,
                        -beta + 1,
                        /*lastMove =*/{},
                        /*lastNullMovePly =*/ply,
                        stack);

        gameState.unmakeNullMove(unmakeInfo);

        if (wasInterrupted_) {
            return -kInfiniteEval;
        }

        if (nullMoveScore >= beta) {
            // TODO: update ttable? We don't have a best move to store, but we can store a lower
            // bound on the score.

            // Null move failed high, don't bother searching other moves.
            // Return a conservative lower bound.
            return beta;
        }
    }

    EvalT bestScore = -kInfiniteEval;
    Move bestMove{};
    bool completedAnySearch = false;

    // Probe the transposition table and use the stored score and/or move if we get a hit.
    const auto ttHit = tTable_.probe(gameState.getBoardHash());
    if (ttHit) {
        const auto& ttInfo = ttHit->payload;

        searchStatistics_.tTableHits++;

        if (ttInfo.depth >= depth) {
            if (ttInfo.scoreType == ScoreType::Exact) {
                // Exact value
                return ttInfo.score;
            } else if (ttInfo.scoreType == ScoreType::LowerBound) {
                // Can safely raise the lower bound for our search window, because the true value
                // is guaranteed to be above this bound.
                alpha = max(alpha, ttInfo.score);
            } else if (ttInfo.scoreType == ScoreType::UpperBound) {
                // Can safely lower the upper bound for our search window, because the true value
                // is guaranteed to be below this bound.
                beta = min(beta, ttInfo.score);
            }
            // Else: score type not set (result from interrupted search).

            // Check if we can return based on tighter bounds from the transposition table.
            if (alpha >= beta) {
                // Based on information from the ttable, we now know that the true value is outside
                // of the feasibility window.
                // If alpha was raised by the tt entry this is a lower bound and we want to return
                // that raised alpha (fail-soft: that's the tightest lower bound we have).
                // If beta was lowered by the tt entry this is an upper bound and we want to return
                // that lowered beta (fail-soft: that's the tightest upper bound we have).
                // So either way we return the tt entry score.
                return ttInfo.score;
            }
        }

        // Try hash move first.
        // TODO: do we need a legality check here for hash collisions?
        const auto outcome = searchMove(
                gameState,
                ttInfo.bestMove,
                depth,
                /*reduction =*/0,
                ply,
                alpha,
                beta,
                stack,
                bestScore,
                bestMove,
                lastMove,
                lastNullMovePly,
                /*useScoutSearch =*/false);

        if (outcome == SearchMoveOutcome::Interrupted) {
            return bestScore;
        }

        completedAnySearch = true;

        if (outcome == SearchMoveOutcome::Cutoff) {
            // Fail high

            updateTTable(
                    bestScore,
                    alphaOrig,
                    beta,
                    wasInterrupted_,
                    bestMove,
                    depth,
                    gameState.getBoardHash());

            // Score was obtained from a subcall that failed high, so it was a lower bound for
            // that position. It is also a lower bound for the overall position because we're
            // maximizing.
            return bestScore;
        }
    }

    auto moves = gameState.generateMoves(stack, enemyControl);
    if (moves.size() == 0) {
        // Exact value
        return evaluateNoLegalMoves(gameState);
    }

    int moveIdx = 0;
    if (ttHit) {
        const auto& ttInfo    = ttHit->payload;
        const auto hashMoveIt = std::find(moves.begin(), moves.end(), ttInfo.bestMove);
        if (hashMoveIt != moves.end()) {
            *hashMoveIt = moves.front();
            ++moveIdx;
        }
    }

    auto moveScores = scoreMoves(
            moves,
            moveIdx,
            gameState,
            getKillerMoves(ply),
            getCounterMove(lastMove, gameState.getSideToMove()),
            historyCutOff_[(int)gameState.getSideToMove()],
            historyUsed_[(int)gameState.getSideToMove()],
            moveScoreStack_);

    for (; moveIdx < moves.size(); ++moveIdx) {
        const Move move = selectBestMove(moves, moveScores, moveIdx);

        const int reduction = getDepthReduction(move, moveIdx, isPvNode, depth, isInCheck);

        const auto outcome = searchMove(
                gameState,
                move,
                depth,
                reduction,
                ply,
                alpha,
                beta,
                stack,
                bestScore,
                bestMove,
                lastMove,
                lastNullMovePly,
                /*useScoutSearch =*/completedAnySearch);

        if (outcome != SearchMoveOutcome::Interrupted) {
            completedAnySearch = true;
        }

        if (outcome != SearchMoveOutcome::Continue) {
            break;
        }
    }

    if (completedAnySearch) {
        // If we fully evaluated any positions, update the ttable.
        MY_ASSERT(bestMove.pieceToMove != Piece::Invalid);
        updateTTable(
                bestScore,
                alphaOrig,
                beta,
                wasInterrupted_,
                bestMove,
                depth,
                gameState.getBoardHash());
    }

    // If bestScore <= alphaOrig, then all subcalls returned upper bounds and bestScore is the
    // maximum of these upper bounds, so an upper bound on the overall position. This is ok because
    // we're returning a value below the original alpha.
    //
    // Otherwise, alpha >= bestScore > alphaOrig.
    //
    // If alphaOrig < bestScore < alpha, then all subscalls returned upper bounds, but all of these
    // upper bounds were below the lower bound 'promised' by the transposition table. This situation
    // should never occur!! But in practice it may because of 'search instability'.
    //
    // Otherwise, bestScore = alpha.
    //
    // If alpha was never raised after a subcall, then alpha must have been raised from the ttable.
    // In this case all subcalls returned upper bounds, and at least one of those upper bounds was
    // equal to the lower bound 'promised' by the transposition table. In this case the value must
    // be exactly this lower/upper bound.
    //
    // If alpha was raised at least once, but we're not failing high, then bestScore was set from
    // an exact value, and all other subcalls return upper bounds below that. So the value is exact.
    //
    // If we're failing high relative to the ttable beta, but not relative to the original beta,
    // then the ttable beta was incorrect: we found a lower bound greater than the 'promised' upper
    // bound. This should never occur!! But in practice it may because of 'search instability'.
    //
    // If we're failing high relative to the original beta then we found a lower bound outside the
    // feasibility window so we can safely return a lower bound.
    return bestScore;
}

// Quiescence search. When in check search all moves, when not in check only search captures.
// Continue until no more capture are available or we get a beta cutoff.
// When not in check use a stand pat evaluation to set alpha and possibly get a beta cutoff.
EvalT MoveSearcher::Impl::quiesce(
        GameState& gameState,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack,
        const bool returnIfNotInCheck) {
    // TODO:
    //  - Can we use the TTable here?

    EvalT bestScore = -kInfiniteEval;

    if (shouldStopSearch()) {
        return bestScore;
    }

    ++searchStatistics_.qNodesSearched;

    // No need to check for repetitions and 50 move rule: those are impossible when only doing
    // captures.

    if (isInsufficientMaterial(gameState)) {
        // Exact value
        return 0;
    }

    const BitBoard enemyControl = gameState.getEnemyControl();
    const bool isInCheck        = gameState.isInCheck(enemyControl);

    if (returnIfNotInCheck && !isInCheck) {
        return kInfiniteEval;
    }

    EvalT standPat;
    if (!isInCheck) {
        // Stand pat
        standPat  = evaluate(gameState, stack, /*checkEndState =*/false);
        bestScore = standPat;
        if (bestScore >= beta) {
            return bestScore;
        }

        static constexpr int kStandPatDeltaPruningThreshold = 1'000;
        const EvalT deltaPruningScore = standPat + kStandPatDeltaPruningThreshold;
        if (deltaPruningScore < alpha) {
            // Stand pat is so far below alpha that we have no hope of raising it even if we find a
            // good capture. Return the stand pat evaluation plus a large margin.
            return deltaPruningScore;  // TODO: return alpha instead? (also in delta pruning below)
        }

        alpha = max(alpha, bestScore);
    }

    auto moves = gameState.generateMoves(stack, enemyControl, /*capturesOnly =*/!isInCheck);
    if (moves.size() == 0) {
        if (isInCheck) {
            // We ran full move generation, so no legal moves exist.
            return evaluateNoLegalMoves(gameState);
        }

        // No captures are available.

        // Check if we're in an end state by generating all moves.
        // Note that this ignores repetitions and 50 move rule.
        const auto allMoves = gameState.generateMoves(stack, enemyControl);
        if (allMoves.size() == 0) {
            return evaluateNoLegalMoves(gameState);
        }

        // If we're not in an end state return the stand pat evaluation.
        return bestScore;
    }

    auto moveScores = scoreMovesQuiesce(moves, /*firstMoveIdx =*/0, gameState, moveScoreStack_);

    for (int moveIdx = 0; moveIdx < moves.size(); ++moveIdx) {
        const Move move = selectBestMove(moves, moveScores, moveIdx);

        bool shouldOnlyConsiderCheck = false;
        if (!isInCheck) {
            // Delta pruning

            MY_ASSERT(isCapture(move.flags));

            constexpr EvalT kDeltaPruningThreshold = 200;

            // Let deltaPruningScore = standPat + SEE + kDeltaPruningThreshold
            // if deltaPruningScore < alpha, we can prune the move if it doesn't give check
            // So we need to check if SEE >= alpha - standPat - kDeltaPruningThreshold
            const int seeThreshold = alpha - standPat - kDeltaPruningThreshold;

            const int seeBound = staticExchangeEvaluationBound(gameState, move, seeThreshold);

            if (seeBound < seeThreshold) {
                // Delta pruning: this move looks like it has no hope of raising alpha. We should
                // only consider it if it gives check.
                shouldOnlyConsiderCheck = true;

                // If our optimistic estimate of the score of this move is above bestScore, raise
                // bestScore to match. This should mean that an upper bound returned from this
                // function if we prune moves is still reliable. Note that this is definitely below
                // alpha.
                const EvalT deltaPruningScore = standPat + seeBound + kDeltaPruningThreshold;
                bestScore                     = max(bestScore, deltaPruningScore);
            }
        }

        const auto unmakeInfo = gameState.makeMove(move);

        EvalT score = -quiesce(gameState, -beta, -alpha, stack, shouldOnlyConsiderCheck);

        gameState.unmakeMove(move, unmakeInfo);

        if (wasInterrupted_) {
            return bestScore;
        }

        if (isMate(score)) {
            score -= signum(score);
        }

        if (score >= beta) {
            return score;
        }

        alpha     = max(alpha, score);
        bestScore = max(bestScore, score);
    }

    return bestScore;
}

FORCE_INLINE MoveSearcher::Impl::SearchMoveOutcome MoveSearcher::Impl::searchMove(
        GameState& gameState,
        const Move& move,
        const int depth,
        const int reduction,
        const int ply,
        EvalT& alpha,
        const EvalT beta,
        StackOfVectors<Move>& stack,
        EvalT& bestScore,
        Move& bestMove,
        const Move& lastMove,
        const int lastNullMovePly,
        const bool useScoutSearch) {

    auto unmakeInfo = gameState.makeMove(move);

    const int reducedDepth = depth - reduction - 1;
    const int fullDepth    = depth - 1;
    if (reducedDepth > 0) {
        const bool likelyNullMoveAllowed = nullMovePruningAllowed(
                gameState, /*isInCheck =*/false, reducedDepth, ply + 1, lastNullMovePly);

        HashT hashToPrefetch = gameState.getBoardHash();
        if (likelyNullMoveAllowed) {
            updateHashForSideToMove(hashToPrefetch);
        }
        tTable_.prefetch(hashToPrefetch);
    }

    EvalT score;
    if (useScoutSearch) {
        // Zero window (scout) search
        score = -search(
                gameState, reducedDepth, ply + 1, -alpha - 1, -alpha, move, lastNullMovePly, stack);

        if (reduction > 0 && score > alpha && !wasInterrupted_) {
            // Search again without reduction
            score =
                    -search(gameState,
                            fullDepth,
                            ply + 1,
                            -alpha - 1,
                            -alpha,
                            move,
                            lastNullMovePly,
                            stack);
        }

        if (score > alpha && score < beta && !wasInterrupted_) {
            // If the score is within the window, do a full window search.
            score = -search(
                    gameState, fullDepth, ply + 1, -beta, -alpha, move, lastNullMovePly, stack);
        }
    } else {
        MY_ASSERT(reduction == 0);

        score = -search(
                gameState, reducedDepth, ply + 1, -beta, -alpha, move, lastNullMovePly, stack);
    }

    gameState.unmakeMove(move, unmakeInfo);

    if (wasInterrupted_) {
        return SearchMoveOutcome::Interrupted;
    }

    if (isMate(score)) {
        score -= signum(score);
    }

    updateHistoryForUse(move, depth, gameState.getSideToMove());
    if (score > bestScore) {
        bestScore = score;
        bestMove  = move;

        if (bestScore >= beta) {
            storeKillerMove(move, ply);
            storeCounterMove(lastMove, move, gameState.getSideToMove());
            updateHistoryForCutoff(move, depth, gameState.getSideToMove());

            // Fail high; score is a lower bound.
            return SearchMoveOutcome::Cutoff;
        }

        // If score is above alpha, it is either exact or a lower bound, so it is safe to raise
        // the lower bound of our feasibility window.
        alpha = max(alpha, bestScore);
    }

    return SearchMoveOutcome::Continue;
}

// Perform an aspiration window search.
RootSearchResult MoveSearcher::Impl::aspirationWindowSearch(
        GameState& gameState,
        const int depth,
        StackOfVectors<Move>& stack,
        const EvalT initialGuess) {
    static constexpr EvalT kInitialTolerance      = 25;
    static constexpr int kToleranceIncreaseFactor = 4;

    EvalT lowerTolerance = kInitialTolerance;
    EvalT upperTolerance = kInitialTolerance;

    EvalT lowerBound = initialGuess - lowerTolerance;
    EvalT upperBound = initialGuess + upperTolerance;

    bool everFailedLow = false;

    EvalT lastCompletedEval = -kInfiniteEval;

    do {
        const auto searchEval =
                search(gameState,
                       depth,
                       0,
                       lowerBound,
                       upperBound,
                       /*lastMove =*/{},
                       /*lastNullMovePly =*/INT_MIN,
                       stack);

        const bool noEval = searchEval < -kMateEval;
        if (!noEval) {
            lastCompletedEval = searchEval;

            const bool failedLow = searchEval <= lowerBound;
            everFailedLow |= failedLow;
        }

        if (wasInterrupted_) {
            if (everFailedLow) {
                // Don't trust the best move if we ever failed low and didn't complete the search.
                // TODO: we should probably ask for more search time here.

                if (frontEnd_) {
                    frontEnd_->reportDiscardedPv("partial aspiration search with failed low");
                }

                StackVector<Move> principalVariation = stack.makeStackVector();
                principalVariation.lock();
                return {.principalVariation = std::move(principalVariation),
                        .eval               = lastCompletedEval,
                        .wasInterrupted     = true};
            }

            // Return partial result.
            return {.principalVariation = extractPv(gameState, stack, depth),
                    .eval               = lastCompletedEval,
                    .wasInterrupted     = true};
        }

        // If we weren't interrupted we should have a valid eval.
        MY_ASSERT(!noEval);

        if (lowerBound < searchEval && searchEval < upperBound) {
            // Eval is within the aspiration window; return result.
            return {.principalVariation = extractPv(gameState, stack, depth),
                    .eval               = searchEval,
                    .wasInterrupted     = false};
        }

        const EvalT previousLowerBound = lowerBound;
        const EvalT previousUpperBound = upperBound;

        if (searchEval <= lowerBound) {
            // Failed low
            if (isMate(searchEval)) {
                // If we found a mate, fully open up the window to the mating side.
                // This will allow us to find the earliest mate.
                lowerBound = -kInfiniteEval;
            } else {
                // Exponentially grow the tolerance.
                lowerTolerance *= kToleranceIncreaseFactor;
                // Expand the lower bound based on the increased tolerance or the search result,
                // whichever is lower.
                lowerBound = min(searchEval - 1, initialGuess - lowerTolerance);
            }
        } else {
            // Failed high
            if (isMate(searchEval)) {
                // If we found a mate, fully open up the window to the mating side.
                // This will allow us to find the earliest mate.
                upperBound = kInfiniteEval;
            } else {
                // Exponentially grow the tolerance.
                upperTolerance *= kToleranceIncreaseFactor;
                // Expand the upper bound based on the increased tolerance or the search result,
                // whichever is higher.
                upperBound = max(searchEval + 1, initialGuess + upperTolerance);
            }
        }

        if (frontEnd_) {
            frontEnd_->reportAspirationWindowReSearch(
                    previousLowerBound, previousUpperBound, searchEval, lowerBound, upperBound);
        }
    } while (true);
}

// Entry point: perform search and return the principal variation and evaluation.
RootSearchResult MoveSearcher::Impl::searchForBestMove(
        GameState& gameState,
        const int depth,
        StackOfVectors<Move>& stack,
        std::optional<EvalT> evalGuess) {
    if (evalGuess) {
        return aspirationWindowSearch(gameState, depth, stack, *evalGuess);
    } else {
        const auto searchEval =
                search(gameState,
                       depth,
                       0,
                       -kInfiniteEval,
                       kInfiniteEval,
                       /*lastMove =*/{},
                       /*lastNullMovePly =*/INT_MIN,
                       stack);
        return {.principalVariation = extractPv(gameState, stack, depth),
                .eval               = searchEval,
                .wasInterrupted     = wasInterrupted_};
    }
}

void MoveSearcher::Impl::prepareForNewSearch(const GameState& gameState) {
    // Set state variables to prepare for search.
    stopSearch_     = false;
    wasInterrupted_ = false;

    shiftKillerMoves(gameState.getHalfMoveClock());
    scaleDownHistory();
}

void MoveSearcher::Impl::interruptSearch() {
    // Set stop flag to interrupt search.
    stopSearch_ = true;
}

SearchStatistics MoveSearcher::Impl::getSearchStatistics() const {

    //for (int side = 0; side < kNumSides; ++side) {
    //    for (int piece = 0; piece < kNumPieceTypes; ++piece) {
    //        for (int square = 0; square < kSquares; ++square) {
    //            const int historyCutOff = gHistoryCutOff[side][piece][square];
    //            const int historyUsed   = gHistoryUsed[side][piece][square];

    //            std::println("{};{};{}: {} / {}", side, piece, square, historyCutOff, historyUsed);
    //        }
    //    }
    //}

    SearchStatistics searchStatistics  = searchStatistics_;
    searchStatistics.ttableUtilization = tTable_.getUtilization();

    return searchStatistics;
}

void MoveSearcher::Impl::resetSearchStatistics() {
    searchStatistics_ = {};
}

void MoveSearcher::Impl::setTTableSize(const int requestedSizeInMb) {
    if (requestedSizeInMb == 0) {
        tTable_ = SearchTTable();
    } else {
        const std::size_t tTableSizeInBytes   = (std::size_t)requestedSizeInMb * 1024 * 1024;
        const std::size_t tTableSizeInEntries = tTableSizeInBytes / sizeof(SearchTTable::EntryT);

        tTable_ = SearchTTable(tTableSizeInEntries);
    }
}

// Implementation of interface: forward to implementation

MoveSearcher::MoveSearcher(const TimeManager& timeManager)
    : impl_(std::make_unique<MoveSearcher::Impl>(timeManager)) {}

MoveSearcher::~MoveSearcher() = default;

void MoveSearcher::setFrontEnd(IFrontEnd* frontEnd) {
    impl_->setFrontEnd(frontEnd);
}

void MoveSearcher::newGame() {
    impl_->newGame();
}

RootSearchResult MoveSearcher::searchForBestMove(
        GameState& gameState,
        int depth,
        StackOfVectors<Move>& stack,
        std::optional<EvalT> evalGuess) {
    return impl_->searchForBestMove(gameState, depth, stack, evalGuess);
}

void MoveSearcher::prepareForNewSearch(const GameState& gameState) {
    impl_->prepareForNewSearch(gameState);
}

void MoveSearcher::interruptSearch() {
    impl_->interruptSearch();
}

SearchStatistics MoveSearcher::getSearchStatistics() const {
    return impl_->getSearchStatistics();
}

void MoveSearcher::resetSearchStatistics() {
    impl_->resetSearchStatistics();
}

void MoveSearcher::setTTableSize(const int requestedSizeInMb) {
    impl_->setTTableSize(requestedSizeInMb);
}
