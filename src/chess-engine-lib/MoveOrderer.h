#pragma once

#include "GameState.h"
#include "Move.h"

#include <optional>

using MoveEvalT = int;

class Evaluator;

//#define TRACK_CUTOFF_STATISTICS

enum class MoveType {
    None,
    HashMove,
    GoodTactical,
    LosingCapture,
#ifdef TRACK_CUTOFF_STATISTICS
    //KillerCounterMove,
    //KillerMove,
    //CounterMove,
    GoodHistory,
    BadHistory,
#else
    Quiet,
#endif
    Quiesce,
    NumMoveTypes,
};

static constexpr std::size_t kNumMoveTypes = (std::size_t)MoveType::NumMoveTypes;

class MoveScorer;

class MoveOrderer {
  public:
    MoveOrderer(
            StackVector<Move>&& preGeneratedMoves,
            StackVector<MoveEvalT>&& emptyMoveScores,
            const std::optional<Move>& hashMove,
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
    [[nodiscard]] std::optional<Move> getNextBestMoveQuiescence(
            const GameState& gameState, const BoardControl& boardControl, bool isInCheck);

    [[nodiscard]] MoveType getLastMoveType() const;

    [[nodiscard]] bool anyLegalMoves(const GameState& gameState, const BoardControl& boardControl);
    [[nodiscard]] bool anyLegalMovesQuiescence(
            const GameState& gameState, const BoardControl& boardControl, bool isInCheck);

    void skipQuiets();

    static constexpr int kCaptureLosingThreshold = -20;

  private:
    enum class State {
        HashMove,
        GenTacticals,
        PickGoodTactical,
        GenQuiets,
        PickQuiets,
        PickLosingCaptures,

        QuiesceHashMove,
        QuiesceGenMoves,
        QuiescePickMove,

        Done,
    };

    void genTacticals(
            const GameState& gameState,
            const BoardControl& boardControl,
            const Move& lastMove,
            int ply);

    std::optional<Move> findGoodTactical(const GameState& gameState);

    void genQuiets(
            const GameState& gameState,
            const BoardControl& boardControl,
            const Move& lastMove,
            int ply);

    std::optional<Move> findQuiet();

    std::optional<Move> findLosingCapture();

    void quiesceGenMoves(
            const GameState& gameState, const BoardControl& boardControl, bool isInCheck);

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

    std::optional<Move> hashMove_;

    bool usingPregeneratedMoves_;
    bool foundAnyLegalMoves_;
    bool skipQuietMoveGeneration_;
};
