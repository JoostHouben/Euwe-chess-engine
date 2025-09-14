#include "MoveOrderer.h"

#include "Macros.h"
#include "MoveScorer.h"
#include "MyAssert.h"
#include "SEE.h"

#include <print>

namespace {

FORCE_INLINE bool ignoreMove(
        const Move& moveToIgnore,
        StackVector<Move>& moves,
        int& moveIdx,
        const bool ignoredMoveShouldExist) {
    const auto startIt = moves.begin() + moveIdx;

    const auto hashMoveIt = std::find(startIt, moves.end(), moveToIgnore);

    MY_ASSERT_DEBUG(IMPLIES(ignoredMoveShouldExist, hashMoveIt != moves.end()));
    (void)ignoredMoveShouldExist;

    if (hashMoveIt != moves.end()) {
        std::swap(*hashMoveIt, *startIt);
        ++moveIdx;

        return true;
    } else {
        return false;
    }
}

}  // namespace

FORCE_INLINE MoveOrderer::MoveOrderer(
        StackVector<Move>&& preGeneratedMoves,
        StackVector<MoveEvalT>&& emptyMoveScores,
        const std::optional<Move>& moveToIgnore,
        const MoveScorer& moveScorer,
        const bool isQuiesce)
    : moves_(std::move(preGeneratedMoves)),
      moveScores_(std::move(emptyMoveScores)),
      moveScorer_(moveScorer),
      state_(isQuiesce ? State::QuiesceGenMoves : State::GenTacticals),
      currentMoveIdx_(0),
      firstLosingCaptureIdx_(moves_.size()),
      firstQuietIdx_(moves_.size()),
      lastMoveType_(MoveType::None),
      moveToIgnore_(moveToIgnore),
      isQuiesce_(isQuiesce),
      usingPregeneratedMoves_(!moves_.empty()),
      foundAnyLegalMoves_(!moves_.empty() || moveToIgnore.has_value()),
      skipQuietMoveGeneration_(false) {
    moveScores_.lock();
}

FORCE_INLINE std::optional<Move> MoveOrderer::getNextBestMove(
        const GameState& gameState,
        const BoardControl& boardControl,
        const Move& lastMove,
        const int ply) {
    MY_ASSERT(
            0 <= firstLosingCaptureIdx_ && firstLosingCaptureIdx_ <= firstQuietIdx_
            && firstQuietIdx_ <= moves_.size());
    MY_ASSERT(0 <= currentMoveIdx_ && currentMoveIdx_ <= moves_.size());

    switch (state_) {
        case State::GenTacticals: {
            genTacticals(gameState, boardControl, lastMove, ply);

            state_ = State::PickGoodTactical;
            [[fallthrough]];
        }

        case State::PickGoodTactical: {
            MY_ASSERT(currentMoveIdx_ <= firstLosingCaptureIdx_);

            if (const auto goodTactical = findGoodTactical(gameState); goodTactical.has_value()) {
                return goodTactical;
            }

            if (skipQuietMoveGeneration_) {
                state_          = State::PickLosingCaptures;
                currentMoveIdx_ = firstLosingCaptureIdx_;
                // NOLINTNEXTLINE(cppcoreguidelines-avoid-goto)
                goto skipQuietsLabel;
            }

            state_          = State::GenQuiets;
            currentMoveIdx_ = firstQuietIdx_;
            [[fallthrough]];
        }

        case State::GenQuiets: {
            genQuiets(gameState, boardControl, lastMove, ply);

            state_ = State::PickQuiets;
            [[fallthrough]];
        }

        case State::PickQuiets: {
            MY_ASSERT(firstQuietIdx_ <= currentMoveIdx_ && currentMoveIdx_ <= moves_.size());

            if (const auto quiet = findQuiet(); quiet.has_value()) {
                return quiet;
            }

            state_          = State::PickLosingCaptures;
            currentMoveIdx_ = firstLosingCaptureIdx_;
            [[fallthrough]];
        }

        case State::PickLosingCaptures:
        skipQuietsLabel: {
            MY_ASSERT(
                    firstLosingCaptureIdx_ <= currentMoveIdx_ && currentMoveIdx_ <= firstQuietIdx_);

            if (const auto losingCapture = findLosingCapture(); losingCapture.has_value()) {
                return losingCapture;
            }

            state_ = State::Done;
            [[fallthrough]];
        }

        case State::Done: {
            MY_ASSERT(currentMoveIdx_ == firstQuietIdx_);

            return std::nullopt;
        }

        case State::QuiesceGenMoves:
        case State::QuiescePickMove: {
            UNREACHABLE;
        }
    }

    UNREACHABLE;
}

FORCE_INLINE std::optional<Move> MoveOrderer::getNextBestMoveQuiescence(
        const GameState& gameState) {
    switch (state_) {
        case State::QuiesceGenMoves: {
            quiesceGenMoves(gameState);

            state_ = State::QuiescePickMove;
            [[fallthrough]];
        }

        case State::QuiescePickMove: {
            if (const auto quiesceMove = findQuiesce(); quiesceMove.has_value()) {
                return quiesceMove;
            }

            state_ = State::Done;
            [[fallthrough]];
        }

        case State::Done: {
            MY_ASSERT(currentMoveIdx_ == moves_.size());

            return std::nullopt;
        }

        case State::GenTacticals:
        case State::PickGoodTactical:
        case State::GenQuiets:
        case State::PickQuiets:
        case State::PickLosingCaptures: {
            UNREACHABLE;
        }
    }

    UNREACHABLE;
}

FORCE_INLINE bool MoveOrderer::lastMoveWasLosing() const {
    return getLastMoveType() == MoveType::LosingCapture;
}

FORCE_INLINE MoveType MoveOrderer::getLastMoveType() const {
    return lastMoveType_;
}

FORCE_INLINE bool MoveOrderer::anyLegalMoves(
        const GameState& gameState, const BoardControl& boardControl) {
    // Pre-generated move list in quiescence search may be incomplete; we need to rely on
    // logic in the quiescence search routine to determine whether there are legal moves.
    MY_ASSERT_DEBUG(!isQuiesce_);

    if (foundAnyLegalMoves_) {
        return true;
    }
    if (usingPregeneratedMoves_ || !skipQuietMoveGeneration_) {
        return foundAnyLegalMoves_;  // false
    }

    // We skipped quiet move generation, so we need to check if there are any quiet moves.
    MY_ASSERT_DEBUG(moves_.empty());
    moves_.unlock();
    gameState.generateMoves(
            moves_, boardControl, MoveCategories::Quiets | MoveCategories::UnderPromotions);
    moves_.lock();

    foundAnyLegalMoves_ |= !moves_.empty();
    return foundAnyLegalMoves_;
}

FORCE_INLINE void MoveOrderer::skipQuiets() {
    if (state_ == State::PickGoodTactical) {
        skipQuietMoveGeneration_ = true;
    } else if (state_ == State::PickQuiets) {
        state_          = State::PickLosingCaptures;
        currentMoveIdx_ = firstLosingCaptureIdx_;
    }
}

void MoveOrderer::genTacticals(
        const GameState& gameState,
        const BoardControl& boardControl,
        const Move& lastMove,
        const int ply) {
    MY_ASSERT_DEBUG(state_ == State::GenTacticals && !isQuiesce_);

    moveScores_.unlock();

    if (usingPregeneratedMoves_) {
        // Use pre-generated moves.
        // This should only happen in the root move.
        // If we add specialized root move ordering in the future, this code path can be
        // removed, including partitionTacticalMoves().

        if (moveToIgnore_
            && ignoreMove(
                    *moveToIgnore_,
                    moves_,
                    currentMoveIdx_,
                    /*ignoredMoveShouldExist*/ true)) {
            moveScores_.push_back(0);  // Placeholder for ignored move
        }

        moveScorer_.scoreMoves(
                moveScores_, moves_, currentMoveIdx_, gameState, boardControl, lastMove, ply);

        // Sets firstLosingCaptureIdx_ and firstQuietIdx_
        partitionTacticalMoves();
    } else {
        // Generate tactical moves.

        gameState.generateMoves(
                moves_, boardControl, MoveCategories::Captures | MoveCategories::QueenPromotions);
        moves_.lock();

        foundAnyLegalMoves_ |= !moves_.empty();

        firstQuietIdx_         = moves_.size();
        firstLosingCaptureIdx_ = firstQuietIdx_;

        if (moveToIgnore_
            && ignoreMove(
                    *moveToIgnore_,
                    moves_,
                    currentMoveIdx_,
                    /*ignoredMoveShouldExist*/ false)) {
            moveScores_.push_back(0);  // Placeholder for ignored move
        }

        // TODO: should we use regular move scoring here?
        // We're not in quiescence search here. But we only have captures and promotions, so perhaps
        // quiescence scoring is good enough (and cheaper).
        // Using regular move scoring would get us history and threat heuristics for promotions.
        moveScorer_.scoreMovesQuiesce(moveScores_, moves_, currentMoveIdx_, gameState);
    }

    moveScores_.lock();
    MY_ASSERT(moves_.size() == moveScores_.size());
}

FORCE_INLINE std::optional<Move> MoveOrderer::findGoodTactical(const GameState& gameState) {
    MY_ASSERT_DEBUG(state_ == State::PickGoodTactical && !isQuiesce_);

    while (currentMoveIdx_ < firstLosingCaptureIdx_) {
        const int bestMoveIdx = findHighestScoringMove(currentMoveIdx_, firstLosingCaptureIdx_);

        const Move bestMove = moves_[bestMoveIdx];

        if (isCapture(bestMove.flags)) {
            const bool isNonLosing = staticExchangeEvaluationMeetsBound(
                    gameState, bestMove, kCaptureLosingThreshold);

            if (!isNonLosing) {
                // This move is losing based on SEE. Move it to the losing moves list, and find the next
                // best move.
                --firstLosingCaptureIdx_;
                std::swap(moves_[bestMoveIdx], moves_[firstLosingCaptureIdx_]);
                std::swap(moveScores_[bestMoveIdx], moveScores_[firstLosingCaptureIdx_]);
                continue;
            }
        }

        // 'destructive swap'
        moves_[bestMoveIdx]      = moves_[currentMoveIdx_];
        moveScores_[bestMoveIdx] = moveScores_[currentMoveIdx_];

        ++currentMoveIdx_;

        lastMoveType_ = MoveType::GoodTactical;
        return bestMove;
    }

    return std::nullopt;
}

void MoveOrderer::genQuiets(
        const GameState& gameState,
        const BoardControl& boardControl,
        const Move& lastMove,
        const int ply) {
    MY_ASSERT_DEBUG(state_ == State::GenQuiets && !isQuiesce_);

    if (!usingPregeneratedMoves_) {
        MY_ASSERT(currentMoveIdx_ == firstQuietIdx_ && firstQuietIdx_ == moves_.size());

        moves_.unlock();
        gameState.generateMoves(
                moves_, boardControl, MoveCategories::Quiets | MoveCategories::UnderPromotions);
        moves_.lock();

        foundAnyLegalMoves_ |= !moves_.empty();

        moveScores_.unlock();

        if (moveToIgnore_
            && ignoreMove(
                    *moveToIgnore_,
                    moves_,
                    currentMoveIdx_,
                    /*ignoredMoveShouldExist*/ false)) {
            moveScores_.push_back(0);  // Placeholder for ignored move
        }

        moveScorer_.scoreMoves(
                moveScores_, moves_, currentMoveIdx_, gameState, boardControl, lastMove, ply);
        moveScores_.lock();
        MY_ASSERT(moves_.size() == moveScores_.size());
    }
}

FORCE_INLINE std::optional<Move> MoveOrderer::findQuiet() {
    MY_ASSERT_DEBUG(state_ == State::PickQuiets && !isQuiesce_);

    if (currentMoveIdx_ < moves_.size()) {
        const int bestMoveIdx = findHighestScoringMove(currentMoveIdx_, moves_.size());

        const Move bestMove = moves_[bestMoveIdx];
#ifdef TRACK_CUTOFF_STATISTICS
        const int bestScore = moveScores_[bestMoveIdx];
#endif

        // 'destructive swap'
        moves_[bestMoveIdx]      = moves_[currentMoveIdx_];
        moveScores_[bestMoveIdx] = moveScores_[currentMoveIdx_];

        ++currentMoveIdx_;

#ifdef TRACK_CUTOFF_STATISTICS
        // Re-enable later: KillerCounterMove, KillerMove, CounterMove
        lastMoveType_ = bestScore > 0 ? MoveType::GoodHistory : MoveType::BadHistory;
#else
        lastMoveType_ = MoveType::Quiet;
#endif

        return bestMove;
    }

    return std::nullopt;
}

FORCE_INLINE std::optional<Move> MoveOrderer::findLosingCapture() {
    MY_ASSERT_DEBUG(state_ == State::PickLosingCaptures && !isQuiesce_);

    if (currentMoveIdx_ < firstQuietIdx_) {
        // We've exhausted all the non-losing moves. Return the best losing move.
        // Here, 'best' is based on the original move scoring.
        // Losing moves are inserted in reverse order of score, so we need to start at the back.
        const int losingMoveIdx     = currentMoveIdx_ - firstLosingCaptureIdx_;
        const int lastLosingMoveIdx = firstQuietIdx_ - 1;
        const int listIdx           = lastLosingMoveIdx - losingMoveIdx;

        ++currentMoveIdx_;

        lastMoveType_ = MoveType::LosingCapture;

        MY_ASSERT(listIdx >= firstLosingCaptureIdx_ && listIdx < firstQuietIdx_);
        return moves_[listIdx];
    }

    return std::nullopt;
}

void MoveOrderer::quiesceGenMoves(const GameState& gameState) {
    MY_ASSERT_DEBUG(state_ == State::QuiesceGenMoves && isQuiesce_ && usingPregeneratedMoves_);

    moveScores_.unlock();
    if (moveToIgnore_
        && ignoreMove(
                *moveToIgnore_,
                moves_,
                currentMoveIdx_,
                /*ignoredMoveShouldExist*/ false)) {
        moveScores_.push_back(0);  // Placeholder for ignored move
    }

    moveScorer_.scoreMovesQuiesce(moveScores_, moves_, currentMoveIdx_, gameState);
    moveScores_.lock();
}

FORCE_INLINE std::optional<Move> MoveOrderer::findQuiesce() {
    if (currentMoveIdx_ == moves_.size()) {
        return std::nullopt;
    }
    MY_ASSERT(0 <= currentMoveIdx_ && currentMoveIdx_ < moves_.size());

    const int bestMoveIdx = findHighestScoringMove(currentMoveIdx_, moves_.size());

    const Move bestMove = moves_[bestMoveIdx];

    // 'destructive swap'
    moves_[bestMoveIdx]      = moves_[currentMoveIdx_];
    moveScores_[bestMoveIdx] = moveScores_[currentMoveIdx_];

    ++currentMoveIdx_;

    return bestMove;
}

FORCE_INLINE void MoveOrderer::partitionTacticalMoves() {
    MY_ASSERT(moves_.size() == moveScores_.size());

    const auto isTactical = [](const Move& move) {
        return isCaptureOrQueenPromo(move);
    };

    // Partition moves into tactical and quiet moves using Hoare's partitioning scheme.
    int i = currentMoveIdx_ - 1;
    int j = moves_.size();

    while (true) {
        do {
            ++i;
        } while (i < j && isTactical(moves_[i]));

        do {
            --j;
        } while (j >= i && !isTactical(moves_[j]));

        if (i >= j) {
            firstQuietIdx_ = i;
            break;
        }

        std::swap(moves_[i], moves_[j]);
        std::swap(moveScores_[i], moveScores_[j]);
    }

    MY_ASSERT(firstQuietIdx_ <= moves_.size());
#ifndef NDEBUG
    for (int moveIdx = currentMoveIdx_; moveIdx < firstQuietIdx_; ++moveIdx) {
        MY_ASSERT(isTactical(moves_[moveIdx]));
    }
    for (int moveIdx = firstQuietIdx_; moveIdx < moves_.size(); ++moveIdx) {
        MY_ASSERT(!isTactical(moves_[moveIdx]));
    }
#endif

    firstLosingCaptureIdx_ = firstQuietIdx_;
}

FORCE_INLINE int MoveOrderer::findHighestScoringMove(const int startIdx, const int endIdx) const {
    // Select best move based on pre-calculated scores using a simple linear search.
    // If the best move is then swapped to the front, repeated calls of this function end up doing
    // a selection sort.

    int bestMoveIdx         = startIdx;
    MoveEvalT bestMoveScore = moveScores_[startIdx];

    for (int moveIdx = startIdx + 1; moveIdx < endIdx; ++moveIdx) {
        if (moveScores_[moveIdx] > bestMoveScore) {
            bestMoveScore = moveScores_[moveIdx];
            bestMoveIdx   = moveIdx;
        }
    }

    return bestMoveIdx;
}
