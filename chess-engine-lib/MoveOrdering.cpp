#include "MoveOrdering.h"

#include "Eval.h"
#include "Macros.h"
#include "MyAssert.h"
#include "SEE.h"

#include <print>

namespace {

// Killer and counter bonuses can potentially both apply.
// Make sure that the combined bonus is less than the capture and promotion bonuses.
constexpr int kCaptureBonus     = 160'000;
constexpr int kPromotionBonus   = 160'000;
constexpr int kKillerMoveBonus  = 80'000;
constexpr int kCounterMoveBonus = 40'000;

constexpr int kMaxHistory = 4096;

constexpr std::array<int, kNumPieceTypes> kEscapeThreatBonus = {
        0,       // Pawn
        4'000,   // Knight
        4'000,   // Bishop
        8'000,   // Rook
        16'000,  // Queen
        0,       // King
};

constexpr std::array<int, kNumPieceTypes> kEnterThreatPenalty = {
        0,       // Pawn
        3'800,   // Knight
        3'800,   // Bishop
        7'600,   // Rook
        15'200,  // Queen
        0,       // King
};

namespace checks {
constexpr int kMaxControlBonus = 16'000;

constexpr int kMinKillerCounterMoveScore = kKillerMoveBonus + kCounterMoveBonus - kMaxHistory;
constexpr int kMaxKillerCounterMoveScore = kKillerMoveBonus + kCounterMoveBonus + kMaxHistory;

constexpr int kMinKillerMoveScore = kKillerMoveBonus - kMaxHistory;
constexpr int kMaxKillerMoveScore = kKillerMoveBonus + kMaxHistory;

constexpr int kMinCounterMoveScore = kCounterMoveBonus - kMaxHistory;
constexpr int kMaxCounterMoveScore = kCounterMoveBonus + kMaxHistory;

constexpr int kMaxRegularQuiet = kMaxHistory + kMaxControlBonus;

static_assert(kMaxKillerCounterMoveScore < kCaptureBonus);
static_assert(kMaxKillerCounterMoveScore < kPromotionBonus);

static_assert(kMaxKillerMoveScore < kMinKillerCounterMoveScore);

static_assert(kMaxCounterMoveScore < kMinKillerMoveScore);

static_assert(kMaxRegularQuiet < kMinCounterMoveScore);
}  // namespace checks

[[nodiscard]] FORCE_INLINE MoveEvalT
scoreQueenPromotion(const Move& /*move*/, const GameState& /*gameState*/) {
    MoveEvalT moveScore = kPromotionBonus;

    moveScore += getStaticPieceValue(Piece::Queen);
    moveScore -= getStaticPieceValue(Piece::Pawn);

    return moveScore;
}

}  // namespace

FORCE_INLINE MoveOrderer::MoveOrderer(
        StackVector<Move>&& moves, StackVector<MoveEvalT>&& moveScores, const int firstMoveIdx)
    : state_(State::Init),
      moves_(std::move(moves)),
      moveScores_(std::move(moveScores)),
      currentMoveIdx_(firstMoveIdx),
      firstLosingCaptureIdx_(moves_.size()),
      firstQuietIdx_(moves_.size()),
      lastMoveType_(MoveType::None) {}

FORCE_INLINE std::optional<Move> MoveOrderer::getNextBestMove(const GameState& gameState) {
    MY_ASSERT(
            0 <= firstLosingCaptureIdx_ && firstLosingCaptureIdx_ <= firstQuietIdx_
            && firstQuietIdx_ <= moves_.size());
    MY_ASSERT(0 <= currentMoveIdx_ && currentMoveIdx_ <= moves_.size());

    switch (state_) {
        case State::Init: {
            partitionTacticalMoves();

            state_ = State::GoodTactical;
            [[fallthrough]];
        }

        case State::GoodTactical: {
            MY_ASSERT(currentMoveIdx_ <= firstLosingCaptureIdx_);

            while (currentMoveIdx_ < firstLosingCaptureIdx_) {
                const int bestMoveIdx =
                        findHighestScoringMove(currentMoveIdx_, firstLosingCaptureIdx_);

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

            state_          = State::Quiets;
            currentMoveIdx_ = firstQuietIdx_;
            [[fallthrough]];
        }

        case State::Quiets: {
            MY_ASSERT(firstQuietIdx_ <= currentMoveIdx_ && currentMoveIdx_ <= moves_.size());

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
                lastMoveType_ = bestScore > kMinKillerCounterMoveScore ? MoveType::KillerCounterMove
                              : bestScore > kMinKillerMoveScore        ? MoveType::KillerMove
                              : bestScore > kMinCounterMoveScore       ? MoveType::CounterMove
                              : bestScore > 0                          ? MoveType::GoodHistory
                                                                       : MoveType::BadHistory;
#else
                lastMoveType_ = MoveType::Quiet;
#endif

                return bestMove;
            }

            state_          = State::LosingCaptures;
            currentMoveIdx_ = firstLosingCaptureIdx_;
            [[fallthrough]];
        }

        case State::LosingCaptures: {
            MY_ASSERT(
                    firstLosingCaptureIdx_ <= currentMoveIdx_ && currentMoveIdx_ <= firstQuietIdx_);

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

            state_ = State::Done;
            [[fallthrough]];
        }

        case State::Done: {
            MY_ASSERT(currentMoveIdx_ == firstQuietIdx_);

            return std::nullopt;
        }
    }

    UNREACHABLE;
}

FORCE_INLINE std::optional<Move> MoveOrderer::getNextBestMoveQuiescence() {
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

FORCE_INLINE bool MoveOrderer::lastMoveWasLosing() const {
    return getLastMoveType() == MoveType::LosingCapture;
}

FORCE_INLINE MoveType MoveOrderer::getLastMoveType() const {
    return lastMoveType_;
}

FORCE_INLINE void MoveOrderer::skipRemainingQuiets() {
    MY_ASSERT(state_ == State::Quiets);
    state_          = State::LosingCaptures;
    currentMoveIdx_ = firstLosingCaptureIdx_;
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

FORCE_INLINE void MoveOrderer::partitionTacticalMoves() {
    MY_ASSERT(state_ == State::Init);

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

MoveScorer::MoveScorer(const Evaluator& evaluator) : evaluator_(evaluator) {
    moveScoreStack_.reserve(1'000);
    newGame();
}

FORCE_INLINE void MoveScorer::reportNonCutoff(
        const Move& move, const GameState& gameState, const MoveType moveType, const int depth) {
    if (isCapture(move)) {
        updateCaptureHistoryForNonCutoff(move, gameState, depth);
    } else if (!isPromotion(move)) {
        updateMainHistoryForNonCutoff(move, depth, gameState.getSideToMove());
    }

#ifdef TRACK_CUTOFF_STATISTICS
    ++numSearchedByMoveType_[(int)moveType];
#else
    (void)moveType;
#endif
}

FORCE_INLINE void MoveScorer::reportCutoff(
        const Move& move,
        const GameState& gameState,
        const MoveType moveType,
        const Move& lastMove,
        const int ply,
        const int depth) {
    if (isCapture(move)) {
        updateCaptureHistoryForCutoff(move, gameState, depth);
    } else if (!isPromotion(move)) {
        storeKillerMove(move, ply);
        storeCounterMove(lastMove, move, gameState.getSideToMove());
        updateMainHistoryForCutoff(move, depth, gameState.getSideToMove());
    }

#ifdef TRACK_CUTOFF_STATISTICS
    ++numSearchedByMoveType_[(int)moveType];
    ++numCutoffsByMoveType_[(int)moveType];
#else
    (void)moveType;
#endif
}

FORCE_INLINE MoveOrderer MoveScorer::getMoveOrderer(
        StackVector<Move>&& moves,
        const std::optional<Move>& moveToIgnore,
        const GameState& gameState,
        const BoardControl& boardControl,
        const Move& lastMove,
        const int ply) const {
    int moveIdx = 0;
    if (moveToIgnore) {
        ignoreMove(*moveToIgnore, moves, moveIdx, /*ignoredMoveShouldExist*/ true);
    }

    auto moveScores = scoreMoves(moves, moveIdx, gameState, boardControl, lastMove, ply);

    return MoveOrderer(std::move(moves), std::move(moveScores), moveIdx);
}

FORCE_INLINE MoveOrderer MoveScorer::getMoveOrdererQuiescence(
        StackVector<Move>&& moves,
        const std::optional<Move>& moveToIgnore,
        const GameState& gameState) const {
    int moveIdx = 0;
    if (moveToIgnore) {
        ignoreMove(*moveToIgnore, moves, moveIdx, /*ignoredMoveShouldExist*/ false);
    }

    auto moveScores = scoreMovesQuiesce(moves, moveIdx, gameState);

    return MoveOrderer(std::move(moves), std::move(moveScores), moveIdx);
}

void MoveScorer::newGame() {
    moveClockForKillerMoves_ = 0;
    killerMoves_             = {};
    counterMoves_            = {};

    initializeHistoryFromPieceSquare();
    initializeCaptureHistory();
}

void MoveScorer::prepareForNewSearch(const GameState& gameState) {
    const int newHalfMoveClock = gameState.getHalfMoveClock();
    if (newHalfMoveClock < moveClockForKillerMoves_
        || newHalfMoveClock > moveClockForKillerMoves_ + 2) {
        newGame();
        moveClockForKillerMoves_ = newHalfMoveClock;
    } else if (newHalfMoveClock != moveClockForKillerMoves_) {
        shiftKillerMoves(newHalfMoveClock);
    }
}

void MoveScorer::resetCutoffStatistics() {
#ifdef TRACK_CUTOFF_STATISTICS
    numSearchedByMoveType_.fill(0);
    numCutoffsByMoveType_.fill(0);
#endif
}

void MoveScorer::printCutoffStatistics(std::ostream& out) const {
#ifdef TRACK_CUTOFF_STATISTICS
    int totalSearched                             = 0;
    int totalNumCutoffs                           = 0;
    std::array<double, kNumMoveTypes> cutoffRates = {};

    // skip MoveType::None
    for (int i = 1; i < kNumMoveTypes; ++i) {
        totalSearched += numSearchedByMoveType_[i];
        totalNumCutoffs += numCutoffsByMoveType_[i];
        cutoffRates[i] = numSearchedByMoveType_[i] == 0
                               ? 0.0
                               : (double)numCutoffsByMoveType_[i] / numSearchedByMoveType_[i];
    }

    const double cutoffRate = totalSearched == 0 ? 0.0 : (double)totalNumCutoffs / totalSearched;

    std::array<double, kNumMoveTypes> cutoffFraction = {};
    for (int i = 1; i < kNumMoveTypes; ++i) {
        cutoffFraction[i] =
                totalNumCutoffs == 0 ? 0.0 : (double)numCutoffsByMoveType_[i] / totalNumCutoffs;
    }

    const auto moveTypeToString = [](const MoveType moveType) {
        switch (moveType) {
            case MoveType::None:
                UNREACHABLE;
            case MoveType::HashMove:
                return "HashMove";
            case MoveType::GoodTactical:
                return "GoodTactical";
            case MoveType::LosingCapture:
                return "LosingCapture";
            case MoveType::KillerCounterMove:
                return "KillerCounterMove";
            case MoveType::KillerMove:
                return "KillerMove";
            case MoveType::CounterMove:
                return "CounterMove";
            case MoveType::GoodHistory:
                return "GoodHistory";
            case MoveType::BadHistory:
                return "BadHistory";
            case MoveType::NumMoveTypes:
                UNREACHABLE;
        }
        UNREACHABLE;
    };

    std::println(
            out,
            "Total cutoffs / searched: {} / {} ({:.1f}%)",
            totalNumCutoffs,
            totalSearched,
            cutoffRate * 100);
    std::println(out, "Cutoff / searched by move type:");
    for (int i = 1; i < kNumMoveTypes; ++i) {
        std::println(
                out,
                "\t{}: {} / {} ({:.1f}%)",
                moveTypeToString((MoveType)i),
                numCutoffsByMoveType_[i],
                numSearchedByMoveType_[i],
                cutoffRates[i] * 100);
    }

    std::println(out, "Cutoff fraction by move type:");
    for (int i = 1; i < kNumMoveTypes; ++i) {
        std::println(out, "\t{}: {:.1f}%", moveTypeToString((MoveType)i), cutoffFraction[i] * 100);
    }
#else
    (void)out;
#endif
}

FORCE_INLINE MoveScorer::KillerMoves& MoveScorer::getKillerMoves(const int ply) {
    MY_ASSERT(ply < kMaxDepth);
    return killerMoves_[ply];
}

FORCE_INLINE const MoveScorer::KillerMoves& MoveScorer::getKillerMoves(const int ply) const {
    MY_ASSERT(ply < kMaxDepth);
    return killerMoves_[ply];
}

FORCE_INLINE void MoveScorer::storeKillerMove(const Move& move, const int ply) {
    auto& plyKillerMoves = getKillerMoves(ply);

    if (move == plyKillerMoves[0]) {
        // Don't store the same move twice.
        return;
    }

    // Shift killer moves down and store the new move at the front.
    plyKillerMoves[1] = plyKillerMoves[0];
    plyKillerMoves[0] = move;
}

FORCE_INLINE Move MoveScorer::getCounterMove(const Move& move, const Side side) const {
    if (move.pieceToMove == Piece::Invalid) {
        return {};
    }
    return counterMoves_[(int)side][(int)move.pieceToMove][(int)move.to];
}

FORCE_INLINE void MoveScorer::storeCounterMove(
        const Move& lastMove, const Move& counter, const Side side) {
    if (lastMove.pieceToMove == Piece::Invalid) {
        return;
    }
    counterMoves_[(int)side][(int)lastMove.pieceToMove][(int)lastMove.to] = counter;
}

FORCE_INLINE MoveScorer::HistoryValueT MoveScorer::getHistoryWeight(const int depth) {
    return (HistoryValueT)(depth * depth);
}

FORCE_INLINE void MoveScorer::updateMainHistoryForCutoff(
        const Move& move, const int depth, const Side side) {
    updateMainHistory(move, side, getHistoryWeight(depth));
}

FORCE_INLINE void MoveScorer::updateMainHistoryForNonCutoff(
        const Move& move, const int depth, const Side side) {
    updateMainHistory(move, side, -getHistoryWeight(depth));
}

FORCE_INLINE void MoveScorer::updateMainHistory(
        const Move& move, const Side side, const HistoryValueT update) {

    const int square = (int)move.to;
    const int piece  = (int)move.pieceToMove;

    updateHistory(history_[(int)side][piece][square], update);
}

FORCE_INLINE void MoveScorer::updateCaptureHistoryForCutoff(
        const Move& move, const GameState& gameState, int depth) {
    updateCaptureHistory(move, gameState, getHistoryWeight(depth));
}

FORCE_INLINE void MoveScorer::updateCaptureHistoryForNonCutoff(
        const Move& move, const GameState& gameState, int depth) {
    updateCaptureHistory(move, gameState, -getHistoryWeight(depth));
}

FORCE_INLINE void MoveScorer::updateCaptureHistory(
        const Move& move, const GameState& gameState, const HistoryValueT update) {
    MY_ASSERT(isCapture(move));

    Piece capturedPiece;
    BoardPosition captureTarget = move.to;
    if (isEnPassant(move.flags)) {
        capturedPiece = Piece::Pawn;
        captureTarget = gameState.getEnPassantTarget();
    } else {
        capturedPiece = getPiece(gameState.getPieceOnSquare(move.to));
    }

    const int side  = (int)gameState.getSideToMove();
    const int piece = (int)move.pieceToMove;

    updateHistory(captureHistory_[side][piece][(int)capturedPiece][(int)captureTarget], update);
}

FORCE_INLINE void MoveScorer::updateHistory(HistoryValueT& history, const HistoryValueT update) {
    // History with 'gravity'.

    const HistoryValueT clampedUpdate = clamp<HistoryValueT>(update, -kMaxHistory, kMaxHistory);

    history += clampedUpdate - history * constexprAbs(clampedUpdate) / kMaxHistory;

    MY_ASSERT(constexprAbs(history) <= kMaxHistory);
}

void MoveScorer::shiftKillerMoves(const int halfMoveClock) {
    const int shiftAmount = halfMoveClock - moveClockForKillerMoves_;

    for (int ply = 0; ply < kMaxDepth - shiftAmount; ++ply) {
        killerMoves_[ply] = killerMoves_[(std::size_t)ply + shiftAmount];
    }

    moveClockForKillerMoves_ = halfMoveClock;
}

void MoveScorer::initializeHistoryFromPieceSquare() {
    for (int side = 0; side < kNumSides; ++side) {
        for (int piece = 0; piece < kNumPieceTypes; ++piece) {
            for (int square = 0; square < kSquares; ++square) {
                const int pieceSquareValue = evaluator_.getPieceSquareValue(
                        (Piece)piece, (BoardPosition)square, (Side)side);
                const HistoryValueT historyValue =
                        (HistoryValueT)clamp(pieceSquareValue, -kMaxHistory, kMaxHistory);

                history_[side][piece][square] = historyValue;
            }
        }
    }
}

void MoveScorer::initializeCaptureHistory() {
    for (int side = 0; side < kNumSides; ++side) {
        for (int capturingPiece = 0; capturingPiece < kNumPieceTypes; ++capturingPiece) {
            for (int capturedPiece = 0; capturedPiece < kNumPieceTypes - 1; ++capturedPiece) {
                for (int square = 0; square < kSquares; ++square) {
                    int historyValue = 0;

                    // Initialize with least valuable attacker (LVA) heuristic
                    historyValue -= getStaticPieceValue((Piece)capturingPiece) >> 5;

                    // Give bonuses for how good the target square is for both the capturing and
                    // captured piece.
                    historyValue += evaluator_.getPieceSquareValue(
                            (Piece)capturingPiece, (BoardPosition)square, (Side)side);
                    historyValue += evaluator_.getPieceSquareValue(
                            (Piece)capturedPiece, (BoardPosition)square, nextSide((Side)side));

                    historyValue = clamp(historyValue, -kMaxHistory, kMaxHistory);

                    captureHistory_[side][capturingPiece][capturedPiece][square] =
                            (HistoryValueT)historyValue;
                }
            }
        }
    }
}

FORCE_INLINE void MoveScorer::ignoreMove(
        const Move& moveToIgnore,
        StackVector<Move>& moves,
        int& moveIdx,
        const bool ignoredMoveShouldExist) const {
    const auto hashMoveIt = std::find(moves.begin(), moves.end(), moveToIgnore);

    MY_ASSERT_DEBUG(IMPLIES(ignoredMoveShouldExist, hashMoveIt != moves.end()));
    (void)ignoredMoveShouldExist;

    if (hashMoveIt != moves.end()) {
        std::swap(*hashMoveIt, moves.front());
        ++moveIdx;
    }
}

StackVector<MoveEvalT> MoveScorer::scoreMoves(
        const StackVector<Move>& moves,
        const int firstMoveIdx,
        const GameState& gameState,
        const BoardControl& boardControl,
        const Move& lastMove,
        const int ply) const {
    StackVector<MoveEvalT> scores = moveScoreStack_.makeStackVector();

    const auto& historyForSide = history_[(int)gameState.getSideToMove()];
    const auto& killerMoves    = getKillerMoves(ply);
    const Move counterMove     = getCounterMove(lastMove, gameState.getSideToMove());

    const int enemySideIdx = (int)nextSide(gameState.getSideToMove());

    std::array<BitBoard, kNumPieceTypes> controlToAvoid{};

    const BitBoard& pawnControl = boardControl.pieceTypeControl[enemySideIdx][(int)Piece::Pawn];
    controlToAvoid[(int)Piece::Knight] = pawnControl;
    controlToAvoid[(int)Piece::Bishop] = pawnControl;

    controlToAvoid[(int)Piece::Rook] =
            pawnControl | boardControl.pieceTypeControl[enemySideIdx][(int)Piece::Knight]
            | boardControl.pieceTypeControl[enemySideIdx][(int)Piece::Bishop];

    controlToAvoid[(int)Piece::Queen] =
            controlToAvoid[(int)Piece::Rook]
            | boardControl.pieceTypeControl[enemySideIdx][(int)Piece::Rook];

    for (int i = 0; i < firstMoveIdx; ++i) {
        scores.push_back(0);
    }

    for (int moveIdx = firstMoveIdx; moveIdx < moves.size(); ++moveIdx) {
        const Move& move = moves[moveIdx];

        MoveEvalT moveScore = 0;

        if (isCapture(move)) {
            moveScore += scoreCapture(move, gameState);
        } else {
            const int pieceIdx = (int)move.pieceToMove;

            moveScore += historyForSide[pieceIdx][(int)move.to];

            const bool originUnderThreat      = controlToAvoid[pieceIdx] & move.from;
            const bool destinationUnderThreat = controlToAvoid[pieceIdx] & move.to;

            if (originUnderThreat && !destinationUnderThreat) {
                moveScore += kEscapeThreatBonus[pieceIdx];
            } else if (!originUnderThreat && destinationUnderThreat) {
                moveScore -= kEnterThreatPenalty[pieceIdx];
            }
        }

        // If promoting to a queen is not a good move, promoting to a knight, bishop, or rook is
        // probably even worse. So only give an ordering bonus for promoting to a queen.
        if (isQueenPromotion(move)) {
            moveScore += scoreQueenPromotion(move, gameState);
        }

        if (!isCapture(move) && !isPromotion(move.flags)) {
            for (const Move& killerMove : killerMoves) {
                if (move == killerMove) {
                    moveScore += kKillerMoveBonus;
                }
            }

            if (move == counterMove) {
                moveScore += kCounterMoveBonus;
            }
        }

        scores.push_back(moveScore);
    }

    scores.lock();
    return scores;
}

StackVector<MoveEvalT> MoveScorer::scoreMovesQuiesce(
        const StackVector<Move>& moves, const int firstMoveIdx, const GameState& gameState) const {
    StackVector<MoveEvalT> scores = moveScoreStack_.makeStackVector();

    for (int i = 0; i < firstMoveIdx; ++i) {
        scores.push_back(0);
    }

    for (int moveIdx = firstMoveIdx; moveIdx < moves.size(); ++moveIdx) {
        const Move& move = moves[moveIdx];

        MoveEvalT moveScore = 0;

        if (isCapture(move)) {
            moveScore += scoreCapture(move, gameState);
        }

        // If promoting to a queen is not a good move, promoting to a knight, bishop, or rook is
        // probably even worse. So only give an ordering bonus for promoting to a queen.
        if (isQueenPromotion(move)) {
            moveScore += scoreQueenPromotion(move, gameState);
        }

        scores.push_back(moveScore);
    }

    scores.lock();
    return scores;
}

FORCE_INLINE MoveEvalT
MoveScorer::scoreCapture(const Move& move, const GameState& gameState) const {
    MoveEvalT moveScore = kCaptureBonus;

    Piece capturedPiece;
    BoardPosition captureTarget = move.to;
    if (isEnPassant(move.flags)) {
        capturedPiece = Piece::Pawn;
        captureTarget = gameState.getEnPassantTarget();
    } else {
        capturedPiece = getPiece(gameState.getPieceOnSquare(move.to));
    }

    // Most valuable victim (MVV).
    moveScore += getStaticPieceValue(capturedPiece);

    // Use capture history instead of least valuable attacker (LVA).
    moveScore += captureHistory_[(int)gameState.getSideToMove()][(int)move.pieceToMove]
                                [(int)capturedPiece][(int)captureTarget];

    return moveScore;
}
