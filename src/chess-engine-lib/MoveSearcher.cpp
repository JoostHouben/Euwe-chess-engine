#include "MoveSearcher.h"

#include "Eval.h"
#include "Math.h"
#include "MoveOrdering.h"
#include "SEE.h"
#include "Syzygy.h"
#include "TTable.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <bit>
#include <limits>
#include <sstream>

#include <cstdint>
#include <cstring>

class MoveSearcher::Impl {
  public:
    Impl(const TimeManager& timeManager, const Evaluator& evaluator);

    void setFrontEnd(IFrontEnd* frontEnd);

    void setSyzygyEnabled(bool enabled);

    void newGame();

    [[nodiscard]] RootSearchResult searchForBestMove(
            GameState& gameState,
            int depth,
            StackOfVectors<Move>& stack,
            std::optional<EvalT> evalGuess = std::nullopt);

    void prepareForNewSearch(
            const GameState& gameState, const std::vector<Move>* movesToSearch, bool tbHitAtRoot);

    void interruptSearch();

    [[nodiscard]] SearchStatistics getSearchStatistics() const;

    void resetSearchStatistics();

    int getDefaultTTableSizeInMb() const;

    void setTTableSize(int requestedSizeInMb);

    [[nodiscard]] std::optional<RootNodeInfo> getRootNodeInfo(const GameState& gameState) const;

  private:
    // == Types ==

    // Outcome of searching a single move; signals to main search whether to continue or stop.
    enum class SearchMoveOutcome {
        Continue,
        Cutoff,
        Interrupted,
    };

    // == Helper functions ==

    // Write updated information to the ttable.
    void updateTTable(
            EvalT bestScore,
            EvalT alphaOrig,
            EvalT beta,
            bool stoppedEarly,
            Move bestMove,
            int depth,
            HashT hash,
            bool isPvNode);

    void storeEgtbValueInTTable(const EvalT value, int depth, HashT hash);

    void storeNullMoveScoreInTTable(const EvalT value, int depth, HashT hash);

    // Extract the principal variation from the transposition table.
    [[nodiscard]] std::vector<Move> extractPv(
            GameState gameState, StackOfVectors<Move>& stack, int depth);

    [[nodiscard]] bool shouldStopSearch() const;

    [[nodiscard]] bool shouldProbeSyzygy(const GameState& gameState, int ply, int depth) const;

    [[nodiscard]] bool captureWillProbeSyzygy(const GameState& gameState, int depth) const;

    [[nodiscard]] std::pair<EvalT, bool> getMoveFutilityValue(
            const EvalT eval,
            const EvalT alpha,
            const int reducedDepth,
            const int movesSearched,
            const Move& move,
            const bool moveIsLosing,
            const GameState& gameState,
            const std::optional<BitBoard> enemyPinBitBoard,
            std::optional<GameState::DirectCheckBitBoards>& directCheckBitBoards);

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
            GameState& gameState, EvalT alpha, EvalT beta, int ply, StackOfVectors<Move>& stack);

    // Subroutine for search.
    // Search a single move, updating alpha, bestScore and bestMove as necessary.
    [[nodiscard]] SearchMoveOutcome searchMove(
            GameState& gameState,
            const Move& move,
            MoveType moveType,
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

    // == Data ==

    mutable std::atomic<bool> stopSearch_ = false;
    mutable bool wasInterrupted_          = false;

    bool rootInTb_      = false;
    bool syzygyEnabled_ = false;

    std::uint8_t tTableTick_ = 0;

    int syzygyMinProbeDepth_ = 1;

    SearchTTable tTable_ = {};

    MoveScorer moveScorer_;

    SearchStatistics searchStatistics_ = {};

    const std::vector<Move>* rootMovesToSearch_ = nullptr;

    IFrontEnd* frontEnd_ = nullptr;

    const TimeManager& timeManager_;

    const Evaluator& evaluator_;
};

namespace {

[[nodiscard]] FORCE_INLINE bool nullMovePruningAllowed(
        const GameState& gameState,
        const bool isPvNode,
        const EvalT beta,
        const bool isInCheck,
        const int depth,
        const int ply,
        const int lastNullMovePly) {
    const bool basicConditions = !isPvNode && !isInCheck && !isMate(beta) && ply > 0 && depth >= 3
                              && ply != lastNullMovePly + 2;
    if (!basicConditions) {
        return false;
    }

    // Allow null move only if the side to move has any (non-pawn) pieces.
    // Having pieces makes zugzwang less likely.
    const Side sideToMove         = gameState.getSideToMove();
    const BitBoard piecesBitBoard = gameState.getPieceBitBoard(sideToMove, Piece::Knight)
                                  | gameState.getPieceBitBoard(sideToMove, Piece::Bishop)
                                  | gameState.getPieceBitBoard(sideToMove, Piece::Rook)
                                  | gameState.getPieceBitBoard(sideToMove, Piece::Queen);

    return piecesBitBoard != BitBoard::Empty;
}

[[nodiscard]] FORCE_INLINE int getDepthExtension(const bool isInCheck, const Move& lastMove) {
    // Check extension
    if (isInCheck) {
        return 1;
    }

    // 7th rank extension
    if (lastMove.pieceToMove == Piece::Pawn) {
        const int toRank = rankFromPosition(lastMove.to);
        if (toRank == 1 || toRank == 6) {
            return 1;
        }
    }

    return 0;
}

const auto lmrReductionTable = []() {
    std::array<std::array<int, 64>, 100> table = {};

    for (int depthIdx = 0; depthIdx < (int)table.size(); ++depthIdx) {
        const int depth = depthIdx + 1;
        for (int movesSearched = 0; movesSearched < (int)table[0].size(); ++movesSearched) {
            if (movesSearched == 0) {
                table[depthIdx][movesSearched] = 0;
            } else {
                table[depthIdx][movesSearched] =
                        (int)max(0., 0.5 + std::log(depth) * std::log(movesSearched) / 3);
            }
        }
    }

    return table;
}();

[[nodiscard]] FORCE_INLINE int getDepthReduction(
        const Move& move,
        const int movesSearched,
        const bool moveIsLosing,
        const bool isPvNode,
        const int depth,
        const int extension) {
    // Don't apply reductions if we're extending the current node.
    if (extension > 0) {
        return 0;
    }

    // Don't apply reductions in PV nodes.
    if (isPvNode) {
        return 0;
    }

    // Don't apply reductions too close to the horizon.
    static constexpr int kMinDepthForReduction = 3;
    if (depth < kMinDepthForReduction) {
        return 0;
    }

    const bool isTactical = isCaptureOrQueenPromo(move);

    if (!moveIsLosing) {
        // Don't apply reductions to tactical moves with positive SEE.
        if (isTactical) {
            return 0;
        }
    }

    // Late Move Reduction (LMR)
    if (isTactical) {
        return 1;
    }

    const int depthIdx         = min(depth - 1, (int)lmrReductionTable.size() - 1);
    const int movesSearchedIdx = min(movesSearched, (int)lmrReductionTable[0].size() - 1);
    const int lmrFromTable     = lmrReductionTable[depthIdx][movesSearchedIdx];

    return min(lmrFromTable, depth - 1);
}

FORCE_INLINE void updateMateDistance(EvalT& score) {
    if (isMate(score)) {
        score = mateDistancePlus1(score);
    }
}

// Compute the delta between two wrapping counters
FORCE_INLINE std::int8_t computeWrappingTickDelta(std::uint8_t tickA, std::uint8_t tickB) {
    const std::uint8_t wrappingTickDelta = tickA - tickB;
    std::int8_t signedTickDelta{};
    // 2's complement is guaranteed
    // if the absolute value of the true tick difference is <= 127, the resulting signedTickDelta
    // is correct
    std::memcpy(&signedTickDelta, &wrappingTickDelta, sizeof(wrappingTickDelta));
    return signedTickDelta;
}

FORCE_INLINE std::optional<Move> getTTableMove(
        const SearchTTPayload payload, const GameState& gameState) {
    if (payload.moveFrom == payload.moveTo) {
        return std::nullopt;
    }

    return Move{
            .pieceToMove = getPiece(gameState.getPieceOnSquare(payload.moveFrom)),
            .from        = payload.moveFrom,
            .to          = payload.moveTo,
            .flags       = payload.moveFlags,
    };
}

[[nodiscard]] FORCE_INLINE std::optional<EvalT> checkForcedEndState(
        const GameState& gameState, StackOfVectors<Move>& stack) {
    if (gameState.isRepetition(/*repetitionThreshold =*/2)) {
        return (EvalT)0;
    }

    if (gameState.isFiftyMoves()) {
        const auto moves = gameState.generateMoves(stack);
        if (moves.size() == 0) {
            return evaluateNoLegalMoves(gameState);
        } else {
            return (EvalT)0;
        }
    }

    if (isInsufficientMaterial(gameState)) {
        return (EvalT)0;
    }

    return std::nullopt;
}

[[nodiscard]] FORCE_INLINE bool isTTEntryMoreValuable(
        const SearchTTEntry& newEntry, const SearchTTEntry& oldEntry) {
    const auto& newPayload = newEntry.payload;
    const auto& oldPayload = oldEntry.payload;

    const int tickDelta        = computeWrappingTickDelta(newPayload.tick, oldPayload.tick);
    const int compensatedDepth = (int)newPayload.depth + tickDelta;

    if (compensatedDepth != (int)oldPayload.depth) {
        // If new entry is deeper (biased for recency), consider it more valuable.
        return compensatedDepth > (int)oldPayload.depth;
    }

    const bool newIsEgtb = newPayload.scoreType == ScoreType::EGTB;
    const bool oldIsEgtb = oldPayload.scoreType == ScoreType::EGTB;
    if (newIsEgtb != oldIsEgtb) {
        // Otherwise, if one of them is an EGTB value, consider that one more valuable.
        return newIsEgtb;
    }

    const bool newIsExact = newPayload.scoreType == ScoreType::Exact;
    const bool oldIsExact = oldPayload.scoreType == ScoreType::Exact;
    if (newIsExact != oldIsExact) {
        // Otherwise, if one of them is an exact value, consider that one more valuable.
        return newIsExact;
    }

    // Break ties in favor of the newer entry;
    return true;
}

[[nodiscard]] FORCE_INLINE EvalT
calculateFutilityMargin(const int reducedDepth, const int movesSearched, const bool isTactical) {
    static constexpr EvalT kFutilityMarginForLosingTactical = 100;
    static constexpr EvalT kFutilityMarginPerDepth          = 140;
    static constexpr EvalT kFutilityMarginPerMoveSearched   = 20;

    if (isTactical) {
        return (EvalT)(kFutilityMarginPerDepth * reducedDepth + kFutilityMarginForLosingTactical);
    }

    return (EvalT)max(
            kFutilityMarginPerDepth * reducedDepth - kFutilityMarginPerMoveSearched * movesSearched,
            0);
}

}  // namespace

MoveSearcher::Impl::Impl(const TimeManager& timeManager, const Evaluator& evaluator)
    : moveScorer_(evaluator), timeManager_(timeManager), evaluator_(evaluator) {
    setTTableSize(getDefaultTTableSizeInMb());
}

void MoveSearcher::Impl::setFrontEnd(IFrontEnd* frontEnd) {
    frontEnd_ = frontEnd;

    frontEnd_->addOption(
            FrontEndOption::createInteger("SyzygyProbeDepth", syzygyMinProbeDepth_, 1, 100));
}

void MoveSearcher::Impl::setSyzygyEnabled(const bool enabled) {
    if (syzygyEnabled_ == enabled) {
        return;
    }

    syzygyEnabled_           = enabled;
    searchStatistics_.tbHits = searchStatistics_.tbHits.value_or(0);

    if (tTable_.getNumInUse() > 0) {
        tTable_.clear();
    }
}

void MoveSearcher::Impl::newGame() {
    // Reset internal state for the sake of consistency

    stopSearch_     = false;
    wasInterrupted_ = false;

    rootInTb_ = false;

    moveScorer_.newGame();

    tTable_.clear();
    tTableTick_ = 0;

    resetSearchStatistics();
}

FORCE_INLINE void MoveSearcher::Impl::updateTTable(
        const EvalT bestScore,
        const EvalT alphaOrig,
        const EvalT beta,
        const bool stoppedEarly,
        const Move bestMove,
        const int depth,
        const HashT hash,
        const bool isPvNode) {
    ScoreType scoreType{};
    if (stoppedEarly) {
        if (bestScore > alphaOrig) {
            // Alpha was raised (but didn't cause a cut off), and the search was interrupted.
            // So this is a partially searched PV node.
            // The score is a lower bound since we raised alpha. Store the score and the best move so
            // far.
            scoreType = ScoreType::LowerBound;
        } else {
            // No move raised alpha but we didn't search all moves, so we don't know if the score is
            // a lower or upper bound.
            scoreType = ScoreType::NotSet;
        }
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
                    .score     = bestScore,
                    .depth     = (std::uint8_t)depth,
                    .tick      = tTableTick_,
                    .scoreType = scoreType,
                    .moveFrom  = bestMove.from,
                    .moveTo    = bestMove.to,
                    .moveFlags = bestMove.flags,
            }};

    if (isPvNode) {
        // In a PV node, ensure that the new info is always stored.
        tTable_.store(entry, [](const SearchTTEntry& newEntry, const SearchTTEntry& oldEntry) {
            if (newEntry.hash == oldEntry.hash) {
                // If we already have info for this position, override it.
                return true;
            }

            // For index collisions use the default policy. The new entry will be stored in the
            // 'recent' slot if less valuable than the existing info.
            return isTTEntryMoreValuable(newEntry, oldEntry);
        });
    } else {
        tTable_.store(entry, isTTEntryMoreValuable);
    }
}

FORCE_INLINE void MoveSearcher::Impl::storeEgtbValueInTTable(
        const EvalT value, const int depth, const HashT hash) {
    const SearchTTable::EntryT entry = {
            .hash    = hash,
            .payload = {
                    .score     = value,
                    .depth     = (std::uint8_t)depth,
                    .tick      = tTableTick_,
                    .scoreType = ScoreType::EGTB,
                    .moveFrom  = (BoardPosition)0,
                    .moveTo    = (BoardPosition)0,
                    .moveFlags = MoveFlags::None,
            }};

    tTable_.store(entry, isTTEntryMoreValuable);
}

FORCE_INLINE void MoveSearcher::Impl::storeNullMoveScoreInTTable(
        const EvalT value, const int depth, const HashT hash) {
    BoardPosition moveFrom = (BoardPosition)0;
    BoardPosition moveTo   = (BoardPosition)0;
    MoveFlags moveFlags    = MoveFlags::None;

    // Retain the existing hash move, if it exists.
    const auto ttHit = tTable_.probe(hash);
    if (ttHit) {
        moveFrom  = ttHit->payload.moveFrom;
        moveTo    = ttHit->payload.moveTo;
        moveFlags = ttHit->payload.moveFlags;
    }

    const SearchTTable::EntryT entry = {
            .hash    = hash,
            .payload = {
                    .score     = value,
                    .depth     = (std::uint8_t)depth,
                    .tick      = tTableTick_,
                    .scoreType = ScoreType::LowerBound,
                    .moveFrom  = moveFrom,
                    .moveTo    = moveTo,
                    .moveFlags = moveFlags,
            }};

    tTable_.store(entry, isTTEntryMoreValuable);
}

std::vector<Move> MoveSearcher::Impl::extractPv(
        GameState gameState, StackOfVectors<Move>& stack, const int depth) {
    const int maxPvLength = max(depth, searchStatistics_.selectiveDepth);

    std::vector<Move> pv;
    pv.reserve(maxPvLength);

    while ((int)pv.size() < maxPvLength) {
        const auto ttHit = tTable_.probe(gameState.getBoardHash());

        if (!ttHit) {
            break;
        }

        const auto move = getTTableMove(ttHit->payload, gameState);
        if (!move) {
            break;
        }

        pv.push_back(*move);
        (void)gameState.makeMove(*move);

        if (checkForcedEndState(gameState, stack).has_value()) {
            break;
        }
    }

    return pv;
}

FORCE_INLINE bool MoveSearcher::Impl::shouldStopSearch() const {
    const std::uint64_t numNodes =
            searchStatistics_.normalNodesSearched + searchStatistics_.qNodesSearched;
    wasInterrupted_ = wasInterrupted_ || stopSearch_.exchange(false)
                   || timeManager_.shouldInterruptSearch(numNodes);
    return wasInterrupted_;
}

FORCE_INLINE bool MoveSearcher::Impl::shouldProbeSyzygy(
        const GameState& gameState, const int ply, const int depth) const {
    return syzygyEnabled_ && !rootInTb_ && ply > 0 && depth >= syzygyMinProbeDepth_
        && canProbeSyzgyWdl(gameState);
}

FORCE_INLINE bool MoveSearcher::Impl::captureWillProbeSyzygy(
        const GameState& gameState, const int depth) const {
    // Note: using the current castling rights here is an approximation; the capture may change the
    // castling rights of the side to move. However, this is a very rare situation.

    return syzygyEnabled_ && !rootInTb_ && depth - 1 >= syzygyMinProbeDepth_ && depth - 1 > 0
        && canProbeSyzgyWdl(
                   /*plySinceCaptureOrPawn*/ 0,
                   gameState.getCastlingRights(),
                   gameState.getNumPieces() - 1);
}

FORCE_INLINE std::pair<EvalT, bool> MoveSearcher::Impl::getMoveFutilityValue(
        const EvalT eval,
        const EvalT alpha,
        const int reducedDepth,
        const int movesSearched,
        const Move& move,
        const bool isLosingTactical,
        const GameState& gameState,
        const std::optional<BitBoard> enemyPinBitBoard,
        std::optional<GameState::DirectCheckBitBoards>& directCheckBitBoards) {
    const bool isTactical = isCaptureOrQueenPromo(move);
    MY_ASSERT(IMPLIES(isLosingTactical, isTactical));

    if (isTactical && !isLosingTactical) {
        // Don't futility prune winning tactical moves.
        return {kMateEval, false};
    }
    if (isCapture(move) && captureWillProbeSyzygy(gameState, reducedDepth)) {
        // We will get a fast and accurate value from syzygy probe, so no point pruning this move.
        return {kMateEval, false};
    }

    const EvalT futilityMargin = calculateFutilityMargin(reducedDepth, movesSearched, isTactical);

    const int seeThreshold = alpha - (eval + futilityMargin);

    EvalT futilityValue{};
    if (seeThreshold >= MoveOrderer::kCaptureLosingThreshold && isLosingTactical) {
        // We know the move is losing, so no need to check whether SEE is above the losing
        // threshold.
        // Note that in this case, alpha >= eval + futilityMargin + kCaptureLosingThreshold.
        // So: futilityValue <= alpha.
        futilityValue = eval + futilityMargin + MoveOrderer::kCaptureLosingThreshold;
    } else if (seeThreshold >= 0 && !isCapture(move) && !isPromotion(move)) {
        // Since the move is not a capture or promotion, its SEE is at best 0. So this move cannot
        // exceed the SEE threshold, meaning futilityValue <= alpha.
        futilityValue = eval + futilityMargin;
    } else {
        // staticExchangeEvaluationBound checks if SEE >= seeThreshold, but we want to know if
        // SEE > seeThreshold. This is equivalent to checking if SEE >= seeThreshold + 1.
        const int seeBound = staticExchangeEvaluationBound(gameState, move, seeThreshold + 1);

        futilityValue = (EvalT)(eval + futilityMargin + seeBound);

        if (futilityValue > alpha) {
            // Early exit to avoid check calculations.
            return {futilityValue, false};
        }
    }

    MY_ASSERT(futilityValue <= alpha);

    if (!directCheckBitBoards) {
        directCheckBitBoards = gameState.getDirectCheckBitBoards();
    }

    if (gameState.givesCheck(move, *directCheckBitBoards, enemyPinBitBoard)) {
        // Don't futility prune moves that give check.
        return {kMateEval, false};
    }

    const bool voteToSkipQuiets = !isTactical && seeThreshold >= 0;

    return {futilityValue, voteToSkipQuiets};
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
    if (depth == 0) {
        return quiesce(gameState, alpha, beta, ply, stack);
    }
    const bool isPvNode = beta - alpha > 1;

    ++searchStatistics_.normalNodesSearched;

    if (isPvNode) {
        searchStatistics_.selectiveDepth = max(searchStatistics_.selectiveDepth, ply);
    }

    // alphaOrig determines whether the value returned is an upper bound
    const EvalT alphaOrig = alpha;

    if (shouldStopSearch()) {
        return -kInfiniteEval;
    }

    if (ply > 0) {
        if (const auto endStateValue = checkForcedEndState(gameState, stack)) {
            // Exact value
            return *endStateValue;
        }
    }

    const BoardControl boardControl = gameState.getBoardControl();
    const bool isInCheck            = gameState.isInCheck(boardControl);

    const int extension = getDepthExtension(isInCheck, lastMove);
    if (ply > 0) {
        depth += extension;
    }

    const bool boundsAreMate = isMate(alpha) || isMate(beta);

    constexpr int kMaxFutilityPruningDepth = 5;
    const bool futilityPruningEnabled      = depth <= kMaxFutilityPruningDepth && !boundsAreMate;

    constexpr int kMaxReverseFutilityPruningDepth = 5;
    const bool reverseFutilityPruningEnabled =
            !isPvNode && !isInCheck && depth <= kMaxReverseFutilityPruningDepth && !boundsAreMate;

    EvalT staticEval = -kInfiniteEval;
    if (futilityPruningEnabled || reverseFutilityPruningEnabled) {
        staticEval = evaluator_.evaluate(gameState, boardControl);
    }
    EvalT eval = staticEval;

    // If we just did eval, we can now get the pin bit board for free.
    const std::optional<BitBoard> enemyPinBitBoard =
            gameState.getCalculatedPinBitBoard(nextSide(gameState.getSideToMove()));

    std::optional<GameState::DirectCheckBitBoards> directCheckBitBoards = std::nullopt;

    if (reverseFutilityPruningEnabled) {
        static constexpr EvalT futilityMarginPerDepth = 140;
        const int futilityValue                       = staticEval - futilityMarginPerDepth * depth;

        if (futilityValue >= beta) {
            // Return a conservative lower bound (fail-hard).
            return beta;
        }
    }

    // Probe the transposition table and use the stored score and/or move if we get a hit.
    auto ttHit = tTable_.probe(gameState.getBoardHash());

    if (rootInTb_ && ttHit.has_value() && ttHit->payload.scoreType == ScoreType::EGTB) {
        // When the root is already in the endgame tablebase, we no longer want to use tablebase
        // values in the search.
        tTable_.erase(gameState.getBoardHash());
        ttHit = std::nullopt;
    }

    std::optional<Move> hashMove = std::nullopt;

    if (ttHit) {
        const auto& ttInfo = ttHit->payload;

        searchStatistics_.tTableHits++;

        // If TT hit is an EGTB value we can return it directly.
        // However, if we're at the root, force a further search to get at least one move in the PV.
        if (ttInfo.scoreType == ScoreType::EGTB && ply > 0) {
            // Exact value
            return ttInfo.score;
        }

        if (ttInfo.depth >= depth) {
            if (ttInfo.scoreType == ScoreType::Exact) {
                if (isPvNode) {
                    searchStatistics_.selectiveDepth =
                            max(searchStatistics_.selectiveDepth, ply + ttInfo.depth);
                }
                // Exact value
                return ttInfo.score;
            } else if (ttInfo.scoreType == ScoreType::LowerBound && ttInfo.score >= beta) {
                // Lower bound
                return ttInfo.score;
            } else if (ttInfo.scoreType == ScoreType::UpperBound && ttInfo.score < alpha) {
                // Upper bound
                return ttInfo.score;
            }
        }

        // Use TT value as a more accurate eval than static eval (for futility pruning)
        if (ttInfo.scoreType == ScoreType::Exact) {
            eval = ttInfo.score;
        } else if (ttInfo.scoreType == ScoreType::LowerBound) {
            eval = max(eval, ttInfo.score);
        } else if (ttInfo.scoreType == UpperBound) {
            eval = min(eval, ttInfo.score);
        }

        hashMove = getTTableMove(ttInfo, gameState);
    }

    if (shouldProbeSyzygy(gameState, ply, depth)) {
        const auto maybeTbScore = probeSyzygyWdl(gameState);
        timeManager_.forceNextCheck();

        if (maybeTbScore) {
            const EvalT tbScore = *maybeTbScore;

            MY_ASSERT(searchStatistics_.tbHits.has_value());
            *searchStatistics_.tbHits += 1;

            storeEgtbValueInTTable(tbScore, depth, gameState.getBoardHash());

            return tbScore;
        }
    }

    if (nullMovePruningAllowed(gameState, isPvNode, beta, isInCheck, depth, ply, lastNullMovePly)) {
        const int nullMoveReduction   = max(3, depth / 2);
        const int nullMoveSearchDepth = max(1, depth - nullMoveReduction - 1);

        const auto unmakeInfo = gameState.makeNullMove();

        EvalT nullMoveScore =
                -search(gameState,
                        nullMoveSearchDepth,
                        ply + 1,
                        -beta,
                        -beta + 1,
                        /*lastMove =*/{},
                        /*lastNullMovePly =*/ply,
                        stack);

        gameState.unmakeNullMove(unmakeInfo);

        updateMateDistance(nullMoveScore);

        if (wasInterrupted_) {
            return -kInfiniteEval;
        }

        if (nullMoveScore >= beta) {
            storeNullMoveScoreInTTable(beta, depth, gameState.getBoardHash());

            // Null move failed high, don't bother searching other moves.
            // Return a conservative lower bound (fail-hard).
            return beta;
        }
    }

    EvalT bestScore = -kInfiniteEval;
    Move bestMove{};
    int movesSearched = 0;

    if (hashMove) {
        // Try hash move first.
        // Do we need a legality check here for hash collisions?
        const auto outcome = searchMove(
                gameState,
                *hashMove,
                MoveType::HashMove,
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

        ++movesSearched;

        if (outcome == SearchMoveOutcome::Cutoff) {
            // Fail high

            updateTTable(
                    bestScore,
                    alphaOrig,
                    beta,
                    wasInterrupted_,
                    bestMove,
                    depth,
                    gameState.getBoardHash(),
                    isPvNode);

            // Score was obtained from a subcall that failed high, so it was a lower bound for
            // that position. It is also a lower bound for the overall position because we're
            // maximizing.
            return bestScore;
        }
    }

    auto moves = ply == 0 && rootMovesToSearch_ ? stack.makeStackVector(*rootMovesToSearch_)
                                                : gameState.generateMoves(stack, boardControl);
    if (moves.size() == 0) {
        // Exact value
        return evaluateNoLegalMoves(gameState);
    }

    auto moveOrderer = moveScorer_.getMoveOrderer(
            std::move(moves), hashMove, gameState, boardControl, lastMove, ply);

    int votesToSkipQuiets = 0;

    while (const auto maybeMove = moveOrderer.getNextBestMove(gameState)) {
        const Move move = *maybeMove;

        const int reduction = getDepthReduction(
                move, movesSearched, moveOrderer.lastMoveWasLosing(), isPvNode, depth, extension);

        // Futility pruning
        if (futilityPruningEnabled) {
            const auto [futilityValue, voteToSkip] = getMoveFutilityValue(
                    eval,
                    alpha,
                    depth - reduction,
                    movesSearched,
                    move,
                    moveOrderer.lastMoveWasLosing(),
                    gameState,
                    enemyPinBitBoard,
                    directCheckBitBoards);

            if (futilityValue <= alpha) {
                if (futilityValue > bestScore) {
                    bestScore = futilityValue;
                    bestMove  = move;
                }

                if (voteToSkip) {
                    ++votesToSkipQuiets;

                    static constexpr int kVotesToSkipQuietsThreshold = 5;
                    if (votesToSkipQuiets >= kVotesToSkipQuietsThreshold) {
                        moveOrderer.skipRemainingQuiets();
                    }
                }

                continue;
            }
        }

        const auto outcome = searchMove(
                gameState,
                move,
                moveOrderer.getLastMoveType(),
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
                /*useScoutSearch =*/isPvNode && (movesSearched > 0));

        if (outcome != SearchMoveOutcome::Interrupted) {
            ++movesSearched;
        }

        if (outcome != SearchMoveOutcome::Continue) {
            break;
        }
    }

    if (movesSearched > 0) {
        // If we fully evaluated any positions, update the ttable.
        MY_ASSERT(bestMove.pieceToMove != Piece::Invalid);
        updateTTable(
                bestScore,
                alphaOrig,
                beta,
                wasInterrupted_,
                bestMove,
                depth,
                gameState.getBoardHash(),
                isPvNode);
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
// Continue until no more captures are available or we get a beta cutoff.
// When not in check use a stand pat evaluation to set alpha and possibly get a beta cutoff.
EvalT MoveSearcher::Impl::quiesce(
        GameState& gameState, EvalT alpha, EvalT beta, const int ply, StackOfVectors<Move>& stack) {
    constexpr EvalT kDeltaPruningThreshold = 200;

    EvalT bestScore = -kInfiniteEval;
    Move bestMove{};

    if (shouldStopSearch()) {
        return bestScore;
    }

    ++searchStatistics_.qNodesSearched;

    const bool isPvNode = beta - alpha > 1;

    if (isPvNode) {
        searchStatistics_.selectiveDepth = max(searchStatistics_.selectiveDepth, ply);
    }

    if (const auto endStateValue = checkForcedEndState(gameState, stack)) {
        return *endStateValue;
    }

    const BoardControl boardControl = gameState.getBoardControl();
    const bool isInCheck            = gameState.isInCheck(boardControl);

    bool completedAnySearch = false;
    const EvalT alphaOrig   = alpha;

    EvalT standPat = -kInfiniteEval;
    if (!isInCheck) {
        // Stand pat
        standPat  = evaluator_.evaluate(gameState, boardControl);
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
    // If we just did eval, we can now get the pin bit board for free.
    const std::optional<BitBoard> enemyPinBitBoard =
            gameState.getCalculatedPinBitBoard(nextSide(gameState.getSideToMove()));

    std::optional<std::array<BitBoard, kNumPieceTypes - 1>> directCheckBitBoards = std::nullopt;

    std::optional<Move> hashMove = std::nullopt;

    // Probe the transposition table and use the stored score and/or move if we get a hit.
    auto ttHit = tTable_.probe(gameState.getBoardHash());

    if (rootInTb_ && ttHit.has_value() && ttHit->payload.scoreType == ScoreType::EGTB) {
        // When the root is already in the endgame tablebase, we no longer want to use tablebase
        // values in the search.
        tTable_.erase(gameState.getBoardHash());
        ttHit = std::nullopt;
    }

    if (ttHit) {
        const auto& ttInfo = ttHit->payload;

        searchStatistics_.tTableHits++;

        // No need to check depth: in qsearch, depth == 0.

        if (ttInfo.scoreType == ScoreType::Exact || ttInfo.scoreType == ScoreType::EGTB) {
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

        hashMove = getTTableMove(ttInfo, gameState);

        bool shouldTryHashMove = hashMove.has_value();
        if (hashMove && !isInCheck) {
            if (!isCapture(hashMove->flags)) {
                shouldTryHashMove = false;
            } else {
                // Let deltaPruningScore = standPat + SEE + kDeltaPruningThreshold
                // if deltaPruningScore < alpha, we can prune the move if it doesn't give check
                // So we need to check if SEE >= alpha - standPat - kDeltaPruningThreshold
                const int seeThreshold = alpha - standPat - kDeltaPruningThreshold;

                const int seeBound =
                        staticExchangeEvaluationBound(gameState, *hashMove, seeThreshold);

                if (seeBound < seeThreshold) {
                    // This move looks like it has no hope of raising alpha, so unless it's a check we
                    // can prune it. For some reason still calculating the upper bound for bestScore
                    // helps even for moves that give check...

                    // If our optimistic estimate of the score of this move is above bestScore, raise
                    // bestScore to match. This should mean that an upper bound returned from this
                    // function if we prune moves is still reliable. Note that this is definitely below
                    // alpha.
                    const EvalT deltaPruningScore =
                            (EvalT)(standPat + seeBound + kDeltaPruningThreshold);
                    bestScore = max(bestScore, deltaPruningScore);

                    if (!directCheckBitBoards) {
                        directCheckBitBoards = gameState.getDirectCheckBitBoards();
                    }

                    if (!gameState.givesCheck(*hashMove, *directCheckBitBoards, enemyPinBitBoard)) {
                        shouldTryHashMove = false;
                    }
                }
            }
        }

        if (shouldTryHashMove) {
            const auto unmakeInfo = gameState.makeMove(*hashMove);

            tTable_.prefetch(gameState.getBoardHash());
            evaluator_.prefetch(gameState);

            EvalT score = -quiesce(gameState, -beta, -alpha, ply + 1, stack);

            gameState.unmakeMove(*hashMove, unmakeInfo);

            if (wasInterrupted_) {
                return bestScore;
            }

            updateMateDistance(score);

            completedAnySearch = true;
            bestScore          = max(bestScore, score);
            bestMove           = *hashMove;

            if (score >= beta) {
                updateTTable(
                        bestScore,
                        alphaOrig,
                        beta,
                        false,
                        bestMove,
                        /*depth =*/0,
                        gameState.getBoardHash(),
                        isPvNode);

                return score;
            }

            alpha = max(alpha, score);
        }
    }

    auto moves = gameState.generateMoves(stack, boardControl, /*capturesOnly =*/!isInCheck);
    if (moves.size() == 0) {
        if (isInCheck) {
            // We ran full move generation, so no legal moves exist, and we're in check, so it's a
            // checkmate.
            return -kMateEval;
        }

        // No captures are available.

        // Check if we're in an end state by generating all moves.
        // Note that this ignores repetitions and 50 move rule.
        const auto allMoves = gameState.generateMoves(stack, boardControl);
        if (allMoves.size() == 0) {
            // No legal moves, not in check, so stalemate.
            return 0;
        }

        // If we're not in an end state return the stand pat evaluation.
        return bestScore;
    }

    // Ignore the hash move even if we didn't try it, since that would mean we pruned it.
    auto moveOrderer = moveScorer_.getMoveOrdererQuiescence(std::move(moves), hashMove, gameState);

    while (const auto maybeMove = moveOrderer.getNextBestMoveQuiescence()) {
        const Move move = *maybeMove;

        if (!isInCheck) {
            // Delta pruning

            MY_ASSERT(isCapture(move));

            // Let deltaPruningScore = standPat + SEE + kDeltaPruningThreshold
            // if deltaPruningScore < alpha, we can prune the move if it doesn't give check
            // So we need to check if SEE >= alpha - standPat - kDeltaPruningThreshold
            const int seeThreshold = alpha - standPat - kDeltaPruningThreshold;

            const int seeBound = staticExchangeEvaluationBound(gameState, move, seeThreshold);

            if (seeBound < seeThreshold) {
                // This move looks like it has no hope of raising alpha, so unless it's a check we
                // can prune it. For some reason still calculating the upper bound for bestScore
                // helps even for moves that give check...

                // If our optimistic estimate of the score of this move is above bestScore, raise
                // bestScore to match. This should mean that an upper bound returned from this
                // function if we prune moves is still reliable. Note that this is definitely below
                // alpha.
                const EvalT deltaPruningScore =
                        (EvalT)(standPat + seeBound + kDeltaPruningThreshold);
                bestScore = max(bestScore, deltaPruningScore);

                if (!directCheckBitBoards) {
                    directCheckBitBoards = gameState.getDirectCheckBitBoards();
                }

                if (!gameState.givesCheck(move, *directCheckBitBoards, enemyPinBitBoard)) {
                    continue;
                }
            }
        }

        const auto unmakeInfo = gameState.makeMove(move);

        tTable_.prefetch(gameState.getBoardHash());
        evaluator_.prefetch(gameState);

        EvalT score = -quiesce(gameState, -beta, -alpha, ply + 1, stack);

        gameState.unmakeMove(move, unmakeInfo);

        if (wasInterrupted_) {
            break;
        }

        updateMateDistance(score);

        if (!completedAnySearch) {
            completedAnySearch = true;
            bestMove           = move;
            // bestScore might be > score due to stand pat
        }

        alpha = max(alpha, score);
        if (score > bestScore) {
            bestScore = score;
            bestMove  = move;
        }

        if (alpha >= beta) {
            break;
        }
    }

    if (completedAnySearch) {
        updateTTable(
                bestScore,
                alphaOrig,
                beta,
                wasInterrupted_,
                bestMove,
                /*depth =*/0,
                gameState.getBoardHash(),
                isPvNode);
    }

    return bestScore;
}

FORCE_INLINE MoveSearcher::Impl::SearchMoveOutcome MoveSearcher::Impl::searchMove(
        GameState& gameState,
        const Move& move,
        const MoveType moveType,
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

    const int reducedDepth = max(depth - reduction - 1, 0);
    const int fullDepth    = depth - 1;

    tTable_.prefetch(gameState.getBoardHash());
    evaluator_.prefetch(gameState);

    EvalT score{};
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
        MY_ASSERT(beta == alpha + 1 || reduction == 0);

        score = -search(
                gameState, reducedDepth, ply + 1, -beta, -alpha, move, lastNullMovePly, stack);
    }

    gameState.unmakeMove(move, unmakeInfo);

    if (wasInterrupted_) {
        return SearchMoveOutcome::Interrupted;
    }

    updateMateDistance(score);

    if (score > bestScore) {
        bestScore = score;
        bestMove  = move;

        if (bestScore >= beta) {
            moveScorer_.reportCutoff(move, gameState, moveType, lastMove, ply, depth);

            // Fail high; score is a lower bound.
            return SearchMoveOutcome::Cutoff;
        }

        // If score is above alpha, it is either exact or a lower bound, so it is safe to raise
        // the lower bound of our feasibility window.
        alpha = max(alpha, bestScore);
    }
    moveScorer_.reportNonCutoff(move, gameState, moveType, depth);

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

    int lowerTolerance = kInitialTolerance;
    int upperTolerance = kInitialTolerance;

    auto toEval = [](int v) {
        return (EvalT)(clamp(v, (int)-kInfiniteEval, (int)kInfiniteEval));
    };

    EvalT lowerBound = toEval(initialGuess - lowerTolerance);
    EvalT upperBound = toEval(initialGuess + upperTolerance);

    bool everFailedLow  = false;
    bool everFailedHigh = false;

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

            everFailedLow |= searchEval <= lowerBound;
            everFailedHigh |= searchEval >= upperBound;
        }

        if (wasInterrupted_) {
            if (everFailedLow) {
                // Don't trust the best move if we ever failed low and didn't complete the search.
                // TODO: we should probably ask for more search time here.

                if (frontEnd_) {
                    frontEnd_->reportDiscardedPv("partial aspiration search with failed low");
                }

                return {.principalVariation = {},
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
            if (isMate(searchEval) && searchEval < 0) {
                // If we found a mate, fully open up the window to the mating side.
                // This will allow us to find the earliest mate.
                lowerBound = -kInfiniteEval;
            } else {
                // Exponentially grow the tolerance.
                const int oldTolerance = lowerTolerance;
                lowerTolerance *= kToleranceIncreaseFactor;
                // Expand the lower bound based on the increased tolerance or the search result,
                // whichever is lower.
                lowerBound = toEval(min(searchEval - oldTolerance, initialGuess - lowerTolerance));
            }

            if (!everFailedHigh) {
                upperBound = toEval(searchEval + 1);
            }
        } else {
            // Failed high
            if (isMate(searchEval) && searchEval > 0) {
                // If we found a mate, fully open up the window to the mating side.
                // This will allow us to find the earliest mate.
                upperBound = kInfiniteEval;
            } else {
                // Exponentially grow the tolerance.
                const int oldTolerance = upperTolerance;
                upperTolerance *= kToleranceIncreaseFactor;
                // Expand the upper bound based on the increased tolerance or the search result,
                // whichever is higher.
                upperBound = toEval(max(searchEval + oldTolerance, initialGuess + upperTolerance));
            }

            if (!everFailedLow) {
                lowerBound = toEval(searchEval - 1);
            }
        }

        if (frontEnd_) {
            frontEnd_->reportAspirationWindowReSearch(
                    depth,
                    previousLowerBound,
                    previousUpperBound,
                    searchEval,
                    lowerBound,
                    upperBound,
                    getSearchStatistics());
        }
    } while (true);
}

// Entry point: perform search and return the principal variation and evaluation.
RootSearchResult MoveSearcher::Impl::searchForBestMove(
        GameState& gameState,
        const int depth,
        StackOfVectors<Move>& stack,
        std::optional<EvalT> evalGuess) {
    MY_ASSERT(depth > 0);

    searchStatistics_.selectiveDepth = 0;

    moveScorer_.resetCutoffStatistics();

    const auto reportCutoffStatistics =
#ifdef TRACK_CUTOFF_STATISTICS
            [this]() {
                std::stringstream ss;
                moveScorer_.printCutoffStatistics(ss);
                frontEnd_->reportDebugString(ss.str());
            };
#else
            []() {
            };
#endif

    if (evalGuess) {
        auto searchResult = aspirationWindowSearch(gameState, depth, stack, *evalGuess);

        reportCutoffStatistics();

        return searchResult;
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

        reportCutoffStatistics();

        return {.principalVariation = extractPv(gameState, stack, depth),
                .eval               = searchEval,
                .wasInterrupted     = wasInterrupted_};
    }
}

void MoveSearcher::Impl::prepareForNewSearch(
        const GameState& gameState,
        const std::vector<Move>* const movesToSearch,
        const bool tbHitAtRoot) {
    // Set state variables to prepare for search.
    stopSearch_     = false;
    wasInterrupted_ = false;

    moveScorer_.prepareForNewSearch(gameState);

    ++tTableTick_;

    if (movesToSearch && !movesToSearch->empty()) {
        rootMovesToSearch_ = movesToSearch;

        // If the hash move at the root is excluded, erase the root node from the TTable.
        const auto rootTtHit = tTable_.probe(gameState.getBoardHash());

        if (rootTtHit) {
            const auto hashMove = getTTableMove(rootTtHit->payload, gameState);
            if (hashMove) {
                const bool hashMoveShouldBeSearched =
                        std::ranges::contains(*movesToSearch, *hashMove);

                if (!hashMoveShouldBeSearched) {
                    tTable_.erase(gameState.getBoardHash());
                }
            }
        }

    } else if (rootMovesToSearch_) {
        rootMovesToSearch_ = nullptr;

        // If we previously had a restriction on moves to search and now we don't, erase the root
        // node from the TTable to avoid polluting with results from the previous restricted search.
        tTable_.erase(gameState.getBoardHash());
    }

    rootInTb_ = tbHitAtRoot;
    if (tbHitAtRoot) {
        searchStatistics_.tbHits = searchStatistics_.tbHits.value_or(0) + 1;
    }
}

void MoveSearcher::Impl::interruptSearch() {
    // Set stop flag to interrupt search.
    stopSearch_ = true;
}

SearchStatistics MoveSearcher::Impl::getSearchStatistics() const {
    SearchStatistics searchStatistics = searchStatistics_;

    searchStatistics.ttableUtilization = tTable_.getUtilization();
    searchStatistics.timeElapsed       = timeManager_.getTimeElapsed();

    const std::uint64_t totalNodes =
            searchStatistics.normalNodesSearched + searchStatistics.qNodesSearched;
    using FloatSeconds = std::chrono::duration<float>;
    const float seconds =
            std::chrono::duration_cast<FloatSeconds>(searchStatistics.timeElapsed).count();
    searchStatistics.nodesPerSecond = static_cast<float>(totalNodes) / seconds;

    return searchStatistics;
}

void MoveSearcher::Impl::resetSearchStatistics() {
    searchStatistics_ = {};

    if (syzygyEnabled_) {
        searchStatistics_.tbHits = 0;
    }
}

int MoveSearcher::Impl::getDefaultTTableSizeInMb() const {
    return 16;
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

std::optional<RootNodeInfo> MoveSearcher::Impl::getRootNodeInfo(const GameState& gameState) const {
    const HashT hash = gameState.getBoardHash();
    const auto ttHit = tTable_.probe(hash);
    if (!ttHit) {
        return std::nullopt;
    }
    const auto ttInfo = ttHit->payload;
    if (ttInfo.scoreType != ScoreType::Exact) {
        return std::nullopt;
    }

    MY_ASSERT_DEBUG(getTTableMove(ttInfo, gameState).has_value());

    return RootNodeInfo{
            .eval  = ttInfo.score,
            .depth = ttInfo.depth,
    };
}

// Implementation of interface: forward to implementation

MoveSearcher::MoveSearcher(const TimeManager& timeManager, const Evaluator& evaluator)
    : impl_(std::make_unique<MoveSearcher::Impl>(timeManager, evaluator)) {}

MoveSearcher::~MoveSearcher() = default;

void MoveSearcher::setFrontEnd(IFrontEnd* const frontEnd) {
    impl_->setFrontEnd(frontEnd);
}

void MoveSearcher::setSyzygyEnabled(const bool enabled) {
    impl_->setSyzygyEnabled(enabled);
}

void MoveSearcher::newGame() {
    impl_->newGame();
}

RootSearchResult MoveSearcher::searchForBestMove(
        GameState& gameState,
        const int depth,
        StackOfVectors<Move>& stack,
        std::optional<EvalT> evalGuess) {
    return impl_->searchForBestMove(gameState, depth, stack, evalGuess);
}

void MoveSearcher::prepareForNewSearch(
        const GameState& gameState,
        const std::vector<Move>* const movesToSearch,
        const bool tbHitAtRoot) {
    impl_->prepareForNewSearch(gameState, movesToSearch, tbHitAtRoot);
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

int MoveSearcher::getDefaultTTableSizeInMb() const {
    return impl_->getDefaultTTableSizeInMb();
}

void MoveSearcher::setTTableSize(const int requestedSizeInMb) {
    impl_->setTTableSize(requestedSizeInMb);
}

std::optional<RootNodeInfo> MoveSearcher::getRootNodeInfo(const GameState& gameState) const {
    return impl_->getRootNodeInfo(gameState);
}
