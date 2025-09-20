#pragma once

#include "BoardConstants.h"
#include "Eval.h"
#include "GameState.h"
#include "Move.h"
#include "MoveOrderer.h"
#include "SearchConstants.h"
#include "StackOfVectors.h"

#include <array>
#include <cstdint>
#include <optional>
#include <ostream>

class MoveScorer {
  public:
    MoveScorer(const Evaluator& evaluator);

    void reportNonCutoff(
            const Move& move, const GameState& gameState, MoveType moveType, int depth);
    void reportCutoff(
            const Move& move,
            const GameState& gameState,
            MoveType moveType,
            const Move& lastMove,
            int ply,
            int depth);

    [[nodiscard]] MoveOrderer getMoveOrderer(
            StackVector<Move>&& preGeneratedMoves, const std::optional<Move>& hashMove) const;

    [[nodiscard]] MoveOrderer getMoveOrdererQuiescence(
            StackVector<Move>&& preGeneratedMoves, const std::optional<Move>& hashMove) const;

    void newGame();
    void prepareForNewSearch(const GameState& gameState);

    void resetCutoffStatistics();

    void printCutoffStatistics(std::ostream& out) const;

    // Scores tactical and quiet moves.
    // Applies history and threat heuristics to all non-captures, and applies killer and
    // counter-move heuristics
    void scoreMoves(
            StackVector<MoveEvalT>& scores,
            const StackVector<Move>& moves,
            const int firstMoveIdx,
            const GameState& gameState,
            const BoardControl& boardControl,
            const Move& lastMove,
            int ply) const;

    // Scores tactical moves (captures + queen promotions); gives the same score to all quiet moves.
    // Also does not apply history and threat heuristics to promotions.
    void scoreMovesQuiesce(
            StackVector<MoveEvalT>& scores,
            const StackVector<Move>& moves,
            const int firstMoveIdx,
            const GameState& gameState) const;

  private:
    static constexpr std::size_t kNumKillerMoves = 2;

    using KillerMoves         = std::array<Move, kNumKillerMoves>;
    using KillerMovesPerDepth = std::array<KillerMoves, kMaxSearchDepth>;

    using MovePerSquare       = std::array<Move, kSquares>;
    using CounterMovePerPiece = std::array<MovePerSquare, kNumPieceTypes>;
    using CounterMovePerSide  = std::array<CounterMovePerPiece, kNumSides>;

    using HistoryValueT         = std::int16_t;
    using HistoryPerSquare      = std::array<HistoryValueT, kSquares>;
    using HistoryPieceTo        = std::array<HistoryPerSquare, kNumPieceTypes>;
    using HistoryPieceToPerSide = std::array<HistoryPieceTo, kNumSides>;

    using HistoryCapturedPiece      = std::array<HistoryPerSquare, kNumPieceTypes - 1>;
    using HistoryPieceCapturedPiece = std::array<HistoryCapturedPiece, kNumPieceTypes>;
    using CaptureHistoryPerSide     = std::array<HistoryPieceCapturedPiece, kNumSides>;

    [[nodiscard]] KillerMoves& getKillerMoves(int ply);
    [[nodiscard]] const KillerMoves& getKillerMoves(int ply) const;
    void storeKillerMove(const Move& move, int ply);

    [[nodiscard]] Move getCounterMove(const Move& move, Side side) const;
    void storeCounterMove(const Move& lastMove, const Move& counter, Side side);

    [[nodiscard]] static HistoryValueT getHistoryWeight(int depth);

    void updateMainHistoryForCutoff(const Move& move, int depth, Side side);
    void updateMainHistoryForNonCutoff(const Move& move, int depth, Side side);
    void updateMainHistory(const Move& move, Side side, HistoryValueT update);

    void updateCaptureHistoryForCutoff(const Move& move, const GameState& gameState, int depth);
    void updateCaptureHistoryForNonCutoff(const Move& move, const GameState& gameState, int depth);
    void updateCaptureHistory(const Move& move, const GameState& gameState, HistoryValueT update);

    void updateHistory(HistoryValueT& history, HistoryValueT update);

    void shiftKillerMoves(int halfMoveClock);
    void initializeHistoryFromPieceSquare();
    void initializeCaptureHistory();

    [[nodiscard]] MoveEvalT scoreCapture(const Move& move, const GameState& gameState) const;

    mutable StackOfVectors<MoveEvalT> moveScoreStack_ = {};

    int moveClockForKillerMoves_     = 0;
    KillerMovesPerDepth killerMoves_ = {};

    CounterMovePerSide counterMoves_ = {};

    HistoryPieceToPerSide history_        = {};
    CaptureHistoryPerSide captureHistory_ = {};

    const Evaluator& evaluator_;

#ifdef TRACK_CUTOFF_STATISTICS
    std::array<int, kNumMoveTypes> numSearchedByMoveType_ = {};
    std::array<int, kNumMoveTypes> numCutoffsByMoveType_  = {};
#endif
};
