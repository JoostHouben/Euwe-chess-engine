#pragma once

#include "EvalParams.h"
#include "EvalT.h"
#include "GameState.h"

#include <memory>
#include <valarray>

using VectorT = std::valarray<double>;

struct EvalWithGradient {
    EvalCalcT eval;
    VectorT gradient;
};

using PstMapping = std::array<std::int8_t, kSquares>;

struct PawnKingEvalInfo {
    EvalCalcT earlyEval = 0.;
    EvalCalcT lateEval  = 0.;

    EvalCalcT phaseMaterial = 0.;

    // Conditionally unstoppable means: unstoppable if enemy has no pieces.
    bool whiteHasConditionallyUnstoppablePawn = false;
    bool blackHasConditionallyUnstoppablePawn = false;
};

class PawnKingEvalHashTable {
  public:
    explicit PawnKingEvalHashTable(bool nonEmpty);

    [[nodiscard]] std::optional<PawnKingEvalInfo> probe(HashT hash) const;

    void prefetch(HashT hash) const;

    void store(HashT hash, const PawnKingEvalInfo& info);

    [[nodiscard]] bool empty() const { return data_ == nullptr; }

  private:
    struct Entry {
        HashT hash = 0;
        PawnKingEvalInfo info{};
    };

    // NOLINTNEXTLINE(cppcoreguidelines-avoid-c-arrays)
    std::unique_ptr<Entry[]> data_;
};

class Evaluator {
  public:
    struct EvalCalcParams : EvalParams {
        EvalCalcParams(const EvalParams& evalParams);

        EvalCalcT maxPhaseMaterial_{};
    };

    explicit Evaluator(bool usePawnKingEvalHashTable = false);
    explicit Evaluator(const EvalParams& params, bool usePawnKingEvalHashTable = false);

    [[nodiscard]] int getPieceSquareValue(Piece piece, BoardPosition position, Side side) const;

    [[nodiscard]] EvalCalcT evaluateRaw(const GameState& gameState) const;

    [[nodiscard]] EvalWithGradient evaluateWithGradient(const GameState& gameState) const;

    [[nodiscard]] EvalT evaluate(const GameState& gameState) const;
    [[nodiscard]] EvalT evaluate(
            const GameState& gameState, const BoardControl& boardControl) const;

    void prefetch(const GameState& gameState) const;

  private:
    EvalCalcParams params_;

    mutable PawnKingEvalHashTable pawnKingEvalHashTable_;
};

[[nodiscard]] int getStaticPieceValue(Piece piece);

[[nodiscard]] bool isInsufficientMaterial(const GameState& gameState);

[[nodiscard]] EvalT evaluateNoLegalMoves(const GameState& gameState);

void getPstMapping(
        const GameState& gameState,
        const PstMapping*& whiteMapping,
        const PstMapping*& whiteKingMapping,
        const PstMapping*& blackMapping,
        const PstMapping*& blackKingMapping);
