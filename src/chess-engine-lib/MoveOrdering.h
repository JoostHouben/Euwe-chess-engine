#pragma once

#include "BoardConstants.h"
#include "GameState.h"
#include "Move.h"
#include "StackOfVectors.h"

#include <array>
#include <optional>
#include <ostream>
#include <utility>

using MoveEvalT = int;

class Evaluator;

//#define TRACK_CUTOFF_STATISTICS

enum class MoveType {
    None,
    HashMove,
    GoodTactical,
    LosingCapture,
#ifdef TRACK_CUTOFF_STATISTICS
    KillerCounterMove,
    KillerMove,
    CounterMove,
    GoodHistory,
    BadHistory,
#else
    Quiet,
#endif
    NumMoveTypes,
};

static constexpr std::size_t kNumMoveTypes = (std::size_t)MoveType::NumMoveTypes;

class MoveScorer;

class MoveOrderer {
  public:
    MoveOrderer(
            StackVector<Move>&& preGeneratedMoves,
            StackVector<MoveEvalT>&& emptyMoveScores,
            const std::optional<Move>& moveToIgnore,
            const MoveScorer& moveScorer,
            bool isQuiesce);

    MoveOrderer(const MoveOrderer&)            = delete;
    MoveOrderer& operator=(const MoveOrderer&) = delete;

    MoveOrderer(MoveOrderer&&)            = delete;
    MoveOrderer& operator=(MoveOrderer&&) = delete;

    ~MoveOrderer() = default;

    [[nodiscard]] std::optional<Move> getNextBestMove(
            const GameState& gameState,
            const BoardControl& boardControl,
            const Move& lastMove,
            int ply);
    [[nodiscard]] std::optional<Move> getNextBestMoveQuiescence(const GameState& gameState);

    [[nodiscard]] bool lastMoveWasLosing() const;
    [[nodiscard]] MoveType getLastMoveType() const;

    [[nodiscard]] bool anyLegalMoves(const GameState& gameState, const BoardControl& boardControl);

    void skipQuiets();

    static constexpr int kCaptureLosingThreshold = -20;

  private:
    enum class State {
        InitTacticals,
        GoodTactical,
        InitQuiets,
        Quiets,
        LosingCaptures,

        InitQuiesce,
        Quiesce,

        Done,
    };

    void initTacticals(
            const GameState& gameState,
            const BoardControl& boardControl,
            const Move& lastMove,
            int ply);

    std::optional<Move> findGoodTactical(const GameState& gameState);

    void initQuiets(
            const GameState& gameState,
            const BoardControl& boardControl,
            const Move& lastMove,
            int ply);

    std::optional<Move> findQuiet();

    std::optional<Move> findLosingCapture();

    void initQuiesce(const GameState& gameState);

    std::optional<Move> findQuiesce();

    void partitionTacticalMoves();

    [[nodiscard]] int findHighestScoringMove(int startIdx, int endIdx) const;

    // Data members ordered for alignment

    StackVector<Move> moves_;
    StackVector<MoveEvalT> moveScores_;

    const MoveScorer& moveScorer_;

    State state_;

    int currentMoveIdx_;
    int firstLosingCaptureIdx_;
    int firstQuietIdx_;

    MoveType lastMoveType_;

    std::optional<Move> moveToIgnore_;

    bool isQuiesce_;
    bool usingPregeneratedMoves_;
    bool foundAnyLegalMoves_;
    bool skipQuietMoveGeneration_;
};

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
            StackVector<Move>&& preGeneratedMoves, const std::optional<Move>& moveToIgnore) const;

    [[nodiscard]] MoveOrderer getMoveOrdererQuiescence(
            StackVector<Move>&& preGeneratedMoves, const std::optional<Move>& moveToIgnore) const;

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
    static constexpr int kMaxDepth               = 100;

    using KillerMoves         = std::array<Move, kNumKillerMoves>;
    using KillerMovesPerDepth = std::array<KillerMoves, kMaxDepth>;

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
