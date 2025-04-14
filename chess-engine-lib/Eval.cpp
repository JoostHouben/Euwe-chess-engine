#include "Eval.h"

#include "BitBoard.h"
#include "Intrinsics.h"
#include "Macros.h"
#include "Math.h"
#include "PawnMasks.h"
#include "PieceControl.h"

#include <array>
#include <optional>
#include <type_traits>
#include <utility>

namespace {
constexpr std::size_t kPawnKingHashTableSizeBytes = 4ULL * 1024 * 1024;
constexpr std::size_t kPawnKingHashTableEntries =
        kPawnKingHashTableSizeBytes / sizeof(PawnKingEvalInfo);

[[nodiscard]] constexpr bool isPowerOfTwo(const std::size_t x) {
    return x != 0 && (x & (x - 1)) == 0;
}

static_assert(isPowerOfTwo(kPawnKingHashTableEntries));

constexpr std::size_t kPawnKingHashTableMask = kPawnKingHashTableEntries - 1;

using PstMappingBySide = std::array<PstMapping, kNumSides>;

struct PstMappings {
    PstMappingBySide defaultIdx;
    PstMappingBySide whiteSideFlipped;
    PstMappingBySide blackSideFlipped;
    PstMappingBySide bothSidesFlipped;
};

constexpr PstMappings kPstMappings = []() {
    PstMappings mappings{};
    for (int squareIdx = 0; squareIdx < kSquares; ++squareIdx) {
        const BoardPosition whitePosition = (BoardPosition)squareIdx;
        const BoardPosition blackPosition = getVerticalReflection(whitePosition);

        const bool onWhiteSide = rankFromPosition(whitePosition) < 4;

        mappings.defaultIdx[0][squareIdx] = (int)whitePosition;
        mappings.defaultIdx[1][squareIdx] = (int)blackPosition;

        // white side flipped
        {
            const BoardPosition flippedWhitePosition =
                    onWhiteSide ? getHorizontalReflection(whitePosition) : whitePosition;
            const BoardPosition flippedBlackPosition =
                    onWhiteSide ? getHorizontalReflection(blackPosition) : blackPosition;

            mappings.whiteSideFlipped[0][squareIdx] = (int)flippedWhitePosition;
            mappings.whiteSideFlipped[1][squareIdx] = (int)flippedBlackPosition;
        }

        // black side flipped
        {
            const BoardPosition flippedWhitePosition =
                    onWhiteSide ? whitePosition : getHorizontalReflection(whitePosition);
            const BoardPosition flippedBlackPosition =
                    onWhiteSide ? blackPosition : getHorizontalReflection(blackPosition);

            mappings.blackSideFlipped[0][squareIdx] = (int)flippedWhitePosition;
            mappings.blackSideFlipped[1][squareIdx] = (int)flippedBlackPosition;
        }

        // both sides flipped
        {
            const BoardPosition flippedWhitePosition = getHorizontalReflection(whitePosition);
            const BoardPosition flippedBlackPosition = getHorizontalReflection(blackPosition);

            mappings.bothSidesFlipped[0][squareIdx] = (int)flippedWhitePosition;
            mappings.bothSidesFlipped[1][squareIdx] = (int)flippedBlackPosition;
        }
    }

    return mappings;
}();

VectorT zeros() {
    return std::valarray<double>((EvalCalcT)0, kNumEvalParams);
}

template <bool CalcJacobians>
using ParamGradient = std::conditional_t<CalcJacobians, VectorT, std::monostate>;

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE ParamGradient<CalcJacobians> zeroGradient() {
    if constexpr (CalcJacobians) {
        return zeros();
    } else {
        return std::monostate{};
    }
}

template <bool CalcJacobians>
struct TermWithGradient;

template <>
struct TermWithGradient<false> {
    EvalCalcT value = 0;
};

template <>
struct TermWithGradient<true> {
    EvalCalcT value          = 0;
    ParamGradient<true> grad = zeroGradient<true>();
};

template <bool CalcJacobians>
struct TaperedEvaluation {
    TermWithGradient<CalcJacobians> early{};
    TermWithGradient<CalcJacobians> late{};
};

template <bool CalcJacobians>
struct PiecePositionEvaluation {
    TaperedEvaluation<CalcJacobians> eval{};

    TermWithGradient<CalcJacobians> phaseMaterial{};

    int numKingAttackers = 0;

    const PstMapping* pstIndex{};
    const PstMapping* pstIndexKing{};
};

template <bool CalcJacobians>
FORCE_INLINE void updateTaperedTerm(
        const Evaluator::EvalCalcParams& params,
        const TaperedTerm& term,
        TaperedEvaluation<CalcJacobians>& eval,
        const EvalCalcT weight) {
    eval.early.value += term.early * weight;
    eval.late.value += term.late * weight;

    if constexpr (CalcJacobians) {
        eval.early.grad[params.getParamIndex(term.early)] += weight;
        eval.late.grad[params.getParamIndex(term.late)] += weight;
    }
}

template <bool CalcJacobians>
FORCE_INLINE void updateTaperedTerm(
        const Evaluator::EvalCalcParams& params,
        const TaperedTerm& term,
        TaperedEvaluation<CalcJacobians>& eval,
        const int weight) {
    updateTaperedTerm(params, term, eval, (EvalCalcT)weight);
}

template <bool CalcJacobians, bool IsKing = false>
FORCE_INLINE void updatePiecePositionEvaluation(
        const Evaluator::EvalCalcParams& params,
        const int pieceIdx,
        const BoardPosition position,
        PiecePositionEvaluation<CalcJacobians>& result) {
    result.phaseMaterial.value += params.phaseMaterialValues[pieceIdx];
    if constexpr (CalcJacobians) {
        result.phaseMaterial.grad[params.getParamIndex(params.phaseMaterialValues[pieceIdx])] += 1;
    }

    int pstIndex{};
    if constexpr (IsKing) {
        pstIndex = (int)((*result.pstIndexKing)[(int)position]);
    } else {
        pstIndex = (int)((*result.pstIndex)[(int)position]);
    }

    updateTaperedTerm(params, params.pieceSquareTables[pieceIdx][pstIndex], result.eval, 1);
}

template <bool CalcJacobians>
FORCE_INLINE void updateMobilityEvaluation(
        const Evaluator::EvalCalcParams& params,
        const Piece piece,
        const BoardControl& boardControl,
        int& pieceControlIdx,
        const BitBoard ownOccupancy,
        const BitBoard enemyKingArea,
        PiecePositionEvaluation<CalcJacobians>& result) {
    const BitBoard& control = boardControl.pieceControl[pieceControlIdx++];
    const bool isAttacker   = (control & enemyKingArea) != BitBoard::Empty;

    result.numKingAttackers += isAttacker;
    updateTaperedTerm(params, params.kingAttackWeight[(int)piece], result.eval, isAttacker);

    const int mobility = popCount(control & ~ownOccupancy);
    updateTaperedTerm(params, params.mobilityBonus[(int)piece], result.eval, mobility);
}

template <bool CalcJacobians>
FORCE_INLINE void updateForVirtualKingMobility(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const Side side,
        const BoardPosition kingPosition,
        TaperedEvaluation<CalcJacobians>& eval) {

    const BitBoard ownOccupancy = gameState.getSideOccupancy(side);

    // Consider all of our own pieces as blockers, but for the enemy pieces we only consider pawns.
    // This is to account for the fact that the other enemy pieces are likely mobile and so should
    // not be relied upon to protect the king from sliding attacks.
    const BitBoard blockers =
            ownOccupancy | gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);

    // We're only interested in squares from which an enemy slider could attack the king, so we
    // exclude the occupied squares themselves.
    const BitBoard virtualKingControl =
            getPieceControlledSquares(Piece::Queen, kingPosition, blockers) & ~blockers;

    const int virtualKingMobility = popCount(virtualKingControl);

    updateTaperedTerm(params, params.kingVirtualMobilityPenalty, eval, -virtualKingMobility);
}

static constexpr auto kTropisms = []() {
    std::array<std::array<std::uint8_t, kSquares>, kSquares> tropisms{};
    for (int from = 0; from < kSquares; ++from) {
        const auto [fromFile, fromRank] = fileRankFromPosition((BoardPosition)from);
        for (int to = 0; to < kSquares; ++to) {
            const auto [toFile, toRank] = fileRankFromPosition((BoardPosition)to);
            const int distance = constexprAbs(fromFile - toFile) + constexprAbs(fromRank - toRank);
            tropisms[from][to] = (std::uint8_t)(14 - distance);
        }
    }
    return tropisms;
}();

static constexpr auto kChebyshevDistances = []() {
    std::array<std::array<std::uint8_t, kSquares>, kSquares> distances{};
    for (int from = 0; from < kSquares; ++from) {
        const auto [fromFile, fromRank] = fileRankFromPosition((BoardPosition)from);
        for (int to = 0; to < kSquares; ++to) {
            const auto [toFile, toRank] = fileRankFromPosition((BoardPosition)to);
            const int distance =
                    max(constexprAbs(fromFile - toFile), constexprAbs(fromRank - toRank));
            distances[from][to] = (std::uint8_t)distance;
        }
    }
    return distances;
}();

[[nodiscard]] FORCE_INLINE int getChebyshevDistance(
        const BoardPosition aPos, const BoardPosition bPos) {
    return (int)kChebyshevDistances[(int)aPos][(int)bPos];
}

template <bool CalcJacobians>
FORCE_INLINE void updateForKingTropism(
        const Evaluator::EvalCalcParams& params,
        const std::array<std::uint8_t, kSquares>& ownKingTropisms,
        const std::array<std::uint8_t, kSquares>& enemyKingTropisms,
        const int pieceIdx,
        const BoardPosition position,
        TaperedEvaluation<CalcJacobians>& eval) {
    const EvalCalcT ownTropism   = ownKingTropisms[(int)position];
    const EvalCalcT enemyTropism = enemyKingTropisms[(int)position];

    updateTaperedTerm(params, params.ownKingTropism[pieceIdx], eval, ownTropism);

    updateTaperedTerm(params, params.enemyKingTropism[pieceIdx], eval, enemyTropism);
}

template <bool CalcJacobians>
FORCE_INLINE void updateForKingTropism(
        const Evaluator::EvalCalcParams& params,
        const BoardPosition ownKingPosition,
        const BoardPosition enemyKingPosition,
        const int pieceIdx,
        const BoardPosition position,
        TaperedEvaluation<CalcJacobians>& eval) {
    updateForKingTropism(
            params,
            kTropisms[(int)ownKingPosition],
            kTropisms[(int)enemyKingPosition],
            pieceIdx,
            position,
            eval);
}

template <bool CalcJacobians>
FORCE_INLINE void updateForKingTropism(
        const Evaluator::EvalCalcParams& params,
        const BoardPosition ownKingPosition,
        const BoardPosition enemyKingPosition,
        const Piece piece,
        const BoardPosition position,
        TaperedEvaluation<CalcJacobians>& eval) {
    updateForKingTropism(params, ownKingPosition, enemyKingPosition, (int)piece, position, eval);
}

template <bool CalcJacobians>
FORCE_INLINE void updateForPins(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const Side side,
        const BoardPosition ownKingPosition,
        TaperedEvaluation<CalcJacobians>& eval) {
    const BitBoard pinBitBoard = gameState.getPinBitBoard(side, ownKingPosition);
    if (pinBitBoard == BitBoard::Empty) {
        return;
    }

    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const BitBoard& pieceBitBoard = gameState.getPieceBitBoard(side, (Piece)pieceIdx);
        const int numPinnedPieces     = popCount(pieceBitBoard & pinBitBoard);

        updateTaperedTerm(params, params.piecePinnedAdjustment[pieceIdx], eval, numPinnedPieces);
    }
}

template <bool CalcJacobians>
FORCE_INLINE void updateForKingOpenFiles(
        const Evaluator::EvalCalcParams& params,
        const Side side,
        const BoardPosition kingPosition,
        const BitBoard ownPawns,
        TaperedEvaluation<CalcJacobians>& eval) {
    const BitBoard forwardMask = getPawnForwardMask(kingPosition, side);
    if ((ownPawns & forwardMask) == BitBoard::Empty) {
        updateTaperedTerm(params, params.kingOpenFileAdjustment, eval, 1);
    }

    const BitBoard neighboringPawns = ownPawns & getPawnNeighborFileMask(kingPosition);
    float flankWeight               = 0.f;

    const BoardPosition flank1Position =
            (BoardPosition)(((std::uint8_t)kingPosition - (std::uint8_t)1) & 63);
    const BitBoard flank1Mask = getPawnForwardMask(flank1Position, side);
    flankWeight += (float)((neighboringPawns & flank1Mask) == BitBoard::Empty);

    const BoardPosition flank2Position =
            (BoardPosition)(((std::uint8_t)kingPosition + (std::uint8_t)1) & 63);
    const BitBoard flank2Mask = getPawnForwardMask(flank2Position, side);
    flankWeight += (float)((neighboringPawns & flank2Mask) == BitBoard::Empty);

    updateTaperedTerm(params, params.kingFlankOpenFileAdjustment, eval, flankWeight);
}

template <bool CalcJacobians>
void evaluatePiecePositionsForSide(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const BoardControl& boardControl,
        const Side side,
        const BoardPosition ownKingPosition,
        const BoardPosition enemyKingPosition,
        const BitBoard enemyKingArea,
        PiecePositionEvaluation<CalcJacobians>& result) {
    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(nextSide(side), Piece::Pawn);
    const BitBoard anyPawn    = ownPawns | enemyPawns;

    const BitBoard ownOccupancy = gameState.getSideOccupancy(side);

    const int numOwnPawns = popCount(ownPawns);

    const auto& ownKingTropisms   = kTropisms[(int)ownKingPosition];
    const auto& enemyKingTropisms = kTropisms[(int)enemyKingPosition];

    int pieceControlIdx = boardControl.getPieceControlStartIdx(side);

    // Knights
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Knight);

        const int numKnights = popCount(pieceBitBoard);
        if (numKnights >= 2) {
            updateTaperedTerm(params, params.knightPairBonus, result.eval, 1);
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);

            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Knight, position, result);

            updateForKingTropism(
                    params,
                    ownKingTropisms,
                    enemyKingTropisms,
                    (int)Piece::Knight,
                    position,
                    result.eval);

            updateTaperedTerm(params, params.knightPawnAdjustment[numOwnPawns], result.eval, 1);

            updateMobilityEvaluation<CalcJacobians>(
                    params,
                    Piece::Knight,
                    boardControl,
                    pieceControlIdx,
                    ownOccupancy,
                    enemyKingArea,
                    result);
        }
    }

    // Bishops
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Bishop);

        const std::array<int, 2> ownPawnsPerSquareColor = {
                popCount(ownPawns & kDarkSquareBitBoard),
                popCount(ownPawns & kLightSquareBitBoard),
        };
        const std::array<int, 2> enemyPawnsPerSquareColor = {
                popCount(enemyPawns & kDarkSquareBitBoard),
                popCount(enemyPawns & kLightSquareBitBoard),
        };

        std::array<bool, 2> hasBishopOfColor = {false, false};

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);

            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Bishop, position, result);

            const int squareColor = getSquareColor(position);

            hasBishopOfColor[squareColor] = true;

            updateTaperedTerm(
                    params,
                    params.bishopPawnSameColorAdjustment[ownPawnsPerSquareColor[squareColor]],
                    result.eval,
                    1);

            updateTaperedTerm(
                    params,
                    params.bishopEnemyPawnSameColorAdjustment
                            [enemyPawnsPerSquareColor[squareColor]],
                    result.eval,
                    1);

            updateForKingTropism(
                    params,
                    ownKingTropisms,
                    enemyKingTropisms,
                    (int)Piece::Bishop,
                    position,
                    result.eval);

            updateMobilityEvaluation<CalcJacobians>(
                    params,
                    Piece::Bishop,
                    boardControl,
                    pieceControlIdx,
                    ownOccupancy,
                    enemyKingArea,
                    result);
        }

        if (hasBishopOfColor[0] && hasBishopOfColor[1]) {
            updateTaperedTerm(params, params.bishopPairBonus, result.eval, 1);
        }
    }

    // Rooks
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Rook);

        const int numRooks = popCount(pieceBitBoard);
        if (numRooks >= 2) {
            updateTaperedTerm(params, params.rookPairBonus, result.eval, 1);
        }

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);

            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Rook, position, result);

            const BitBoard fileBitBoard = getFileBitBoard(position);
            const bool blockedByOwnPawn = (ownPawns & fileBitBoard) != BitBoard::Empty;
            const bool blockedByAnyPawn = (anyPawn & fileBitBoard) != BitBoard::Empty;

            if (!blockedByAnyPawn) {
                updateTaperedTerm(params, params.rookOpenFileBonus, result.eval, 1);
            } else if (!blockedByOwnPawn) {
                updateTaperedTerm(params, params.rookSemiOpenFileBonus, result.eval, 1);
            }

            updateForKingTropism(
                    params,
                    ownKingTropisms,
                    enemyKingTropisms,
                    (int)Piece::Rook,
                    position,
                    result.eval);

            updateTaperedTerm(params, params.rookPawnAdjustment[numOwnPawns], result.eval, 1);

            updateMobilityEvaluation<CalcJacobians>(
                    params,
                    Piece::Rook,
                    boardControl,
                    pieceControlIdx,
                    ownOccupancy,
                    enemyKingArea,
                    result);
        }
    }

    // Queens
    {
        BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, Piece::Queen);

        while (pieceBitBoard != BitBoard::Empty) {
            const BoardPosition position = popFirstSetPosition(pieceBitBoard);

            updatePiecePositionEvaluation<CalcJacobians>(
                    params, (int)Piece::Queen, position, result);

            updateForKingTropism(
                    params,
                    ownKingTropisms,
                    enemyKingTropisms,
                    (int)Piece::Queen,
                    position,
                    result.eval);

            updateTaperedTerm(params, params.queenPawnAdjustment[numOwnPawns], result.eval, 1);

            updateMobilityEvaluation<CalcJacobians>(
                    params,
                    Piece::Queen,
                    boardControl,
                    pieceControlIdx,
                    ownOccupancy,
                    enemyKingArea,
                    result);
        }
    }

    // King
    {
        // no mobility bonus for king

        updateForVirtualKingMobility<CalcJacobians>(
                params, gameState, side, ownKingPosition, result.eval);

        // Note king attack data between kings was calculated during initialization.

        // King safety.
        const int controlNearEnemyKing =
                popCount(boardControl.sideControl[(int)side] & enemyKingArea);

        updateTaperedTerm(
                params, params.controlNearEnemyKing[controlNearEnemyKing], result.eval, 1);

        const int numKingAttackersIdx =
                min(result.numKingAttackers, (int)params.numKingAttackersAdjustment.size() - 1);
        updateTaperedTerm(
                params, params.numKingAttackersAdjustment[numKingAttackersIdx], result.eval, 1);

        updateForPins(params, gameState, side, ownKingPosition, result.eval);
    }
}

template <bool CalcJacobians>
FORCE_INLINE void evaluateAttackDefend(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const BoardControl& boardControl,
        const Side side,
        TaperedEvaluation<CalcJacobians>& eval) {
    const BitBoard& ownControl   = boardControl.sideControl[(int)side];
    const BitBoard& enemyControl = boardControl.sideControl[(int)nextSide(side)];

    const std::array<BitBoard, 3> attackDefendBitBoards = {
            ownControl & ~enemyControl,  // defended, not attacked
            ~ownControl & enemyControl,  // not defended, attacked (hanging)
            ownControl & enemyControl,   // defended, attacked
    };

    // Skip the king: defending the king is useless, and the king should never be under attack
    // (i.e., in check) when running eval.
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        const BitBoard pieceBitBoard = gameState.getPieceBitBoard(side, (Piece)pieceIdx);

        for (int attackDefendIdx = 0; attackDefendIdx < 3; ++attackDefendIdx) {
            const BitBoard relevantPieces = pieceBitBoard & attackDefendBitBoards[attackDefendIdx];
            const int numRelevantPieces   = popCount(relevantPieces);

            updateTaperedTerm(
                    params,
                    params.attackDefendAdjustment[pieceIdx][attackDefendIdx],
                    eval,
                    numRelevantPieces);
        }
    }
}

template <bool CalcJacobians>
FORCE_INLINE void modifyForFactor(
        const Evaluator::EvalCalcParams& params,
        const EvalCalcT& factor,
        TermWithGradient<CalcJacobians>& whiteEval) {
    if constexpr (CalcJacobians) {
        whiteEval.grad *= factor;

        auto& dEvalDFactor = whiteEval.grad[params.getParamIndex(factor)];
        MY_ASSERT(dEvalDFactor == 0);
        dEvalDFactor = whiteEval.value;
    }

    whiteEval.value *= factor;
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE bool correctForOppositeColoredBishops(
        const GameState& gameState,
        const Evaluator::EvalCalcParams& params,
        TermWithGradient<CalcJacobians>& whiteEval) {
    const BitBoard anyOtherPiece = gameState.getPieceBitBoard(Side::White, Piece::Knight)
                                 | gameState.getPieceBitBoard(Side::White, Piece::Rook)
                                 | gameState.getPieceBitBoard(Side::White, Piece::Queen)
                                 | gameState.getPieceBitBoard(Side::Black, Piece::Knight)
                                 | gameState.getPieceBitBoard(Side::Black, Piece::Rook)
                                 | gameState.getPieceBitBoard(Side::Black, Piece::Queen);

    if (anyOtherPiece != BitBoard::Empty) {
        return false;
    }

    const BitBoard whiteDarkBishops =
            gameState.getPieceBitBoard(Side::White, Piece::Bishop) & kDarkSquareBitBoard;
    const BitBoard whiteLightBishops =
            gameState.getPieceBitBoard(Side::White, Piece::Bishop) & kLightSquareBitBoard;
    const BitBoard blackDarkBishops =
            gameState.getPieceBitBoard(Side::Black, Piece::Bishop) & kDarkSquareBitBoard;
    const BitBoard blackLightBishops =
            gameState.getPieceBitBoard(Side::Black, Piece::Bishop) & kLightSquareBitBoard;

    const bool bishopsAreOppositeColor =
            ((whiteDarkBishops != BitBoard::Empty) ^ (blackDarkBishops != BitBoard::Empty))
            && ((whiteLightBishops != BitBoard::Empty) ^ (blackLightBishops != BitBoard::Empty));

    if (!bishopsAreOppositeColor) {
        return false;
    }

    const int whitePawns = popCount(gameState.getPieceBitBoard(Side::White, Piece::Pawn));
    const int blackPawns = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Pawn));

    const int pawnDelta = std::abs(whitePawns - blackPawns);
    const int factorIdx = min(pawnDelta, (int)params.oppositeColoredBishopFactor.size() - 1);

    modifyForFactor<CalcJacobians>(
            params, params.oppositeColoredBishopFactor[factorIdx], whiteEval);

    return true;
}

template <bool CalcJacobians>
void correctForDrawish(
        const GameState& gameState,
        const Evaluator::EvalCalcParams& params,
        TermWithGradient<CalcJacobians>& whiteEval) {
    if (correctForOppositeColoredBishops<CalcJacobians>(gameState, params, whiteEval)) {
        return;
    }

    const bool whiteIsStronger = whiteEval.value >= 0;

    const int strongerSidePawns =
            whiteIsStronger ? popCount(gameState.getPieceBitBoard(Side::White, Piece::Pawn))
                            : popCount(gameState.getPieceBitBoard(Side::Black, Piece::Pawn));

    if (strongerSidePawns > 0) {
        return;
    }

    int strongSideKnights = popCount(gameState.getPieceBitBoard(Side::White, Piece::Knight));
    int strongSideBishops = popCount(gameState.getPieceBitBoard(Side::White, Piece::Bishop));
    int strongSideRooks   = popCount(gameState.getPieceBitBoard(Side::White, Piece::Rook));
    int strongSideQueens  = popCount(gameState.getPieceBitBoard(Side::White, Piece::Queen));

    int weakSideKnights = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Knight));
    int weakSideBishops = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Bishop));
    int weakSideRooks   = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Rook));
    int weakSideQueens  = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Queen));

    if (!whiteIsStronger) {
        std::swap(strongSideKnights, weakSideKnights);
        std::swap(strongSideBishops, weakSideBishops);
        std::swap(strongSideRooks, weakSideRooks);
        std::swap(strongSideQueens, weakSideQueens);
    }

    const int strongSideMinorPieces = strongSideKnights + strongSideBishops;
    const int strongSideMajorPieces = strongSideRooks + strongSideQueens;

    const int weakSideMinorPieces = weakSideKnights + weakSideBishops;
    const int weakSideMajorPieces = weakSideRooks + weakSideQueens;

    // With only a single minor piece you can't reliably deliver mate.
    if (strongSideMajorPieces == 0 && strongSideMinorPieces == 1) {
        modifyForFactor<CalcJacobians>(params, params.singleMinorFactor, whiteEval);
        return;
    }

    // Only two knights; this is insufficient material once the weaker side has lost their material.
    if (strongSideMajorPieces == 0 && strongSideKnights == 2 && strongSideBishops == 0) {
        modifyForFactor<CalcJacobians>(params, params.twoKnightsFactor, whiteEval);
        return;
    }

    // Rook vs a minor piece is drawish.
    if (strongSideRooks == 1 && strongSideQueens == 0 && strongSideMinorPieces == 0
        && (weakSideMinorPieces == 1 && weakSideMajorPieces == 0)) {
        modifyForFactor<CalcJacobians>(params, params.rookVsMinorFactor, whiteEval);
        return;
    }

    // Rook and minor vs rook is drawish.
    if (strongSideRooks == 1 && strongSideQueens == 0 && strongSideMinorPieces == 1
        && weakSideRooks == 1 && weakSideQueens == 0 && weakSideMinorPieces == 0) {
        modifyForFactor<CalcJacobians>(params, params.rookAndMinorVsRookFactor, whiteEval);
        return;
    }
}

[[nodiscard]] FORCE_INLINE BoardPosition
getPromotionSquare(const BoardPosition pawnPosition, const Side pawnSide) {
    return (BoardPosition)((((int)pawnSide - 1) & 56) + ((int)pawnPosition & 7));
}

template <bool CalcJacobians>
void evaluatePawnKingForSide(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const BoardControl& boardControl,
        const Side side,
        const BoardPosition ownKingPosition,
        const BoardPosition enemyKingPosition,
        const BitBoard ownKingArea,
        const BitBoard enemyKingArea,
        PiecePositionEvaluation<CalcJacobians>& result,
        bool& hasConditionallyUnstoppablePawn) {
    const Side enemySide = nextSide(side);

    const BitBoard ownPawns   = gameState.getPieceBitBoard(side, Piece::Pawn);
    const BitBoard enemyPawns = gameState.getPieceBitBoard(enemySide, Piece::Pawn);

    BitBoard pawnBitBoard = ownPawns;

    hasConditionallyUnstoppablePawn = false;
    std::array<EvalCalcT, kFiles + 2> filePassedPawnWeight{};

    while (pawnBitBoard != BitBoard::Empty) {
        const BoardPosition position = popFirstSetPosition(pawnBitBoard);
        const int file               = fileFromPosition(position);

        const BitBoard passedPawnOpponentMask = getPassedPawnOpponentMask(position, side);
        const BitBoard forwardMask            = getPawnForwardMask(position, side);
        const BitBoard neighborMask           = getPawnNeighborFileMask(position);

        const BitBoard opponentBlockers = enemyPawns & passedPawnOpponentMask;
        const BitBoard ownBlockers      = ownPawns & forwardMask;
        const BitBoard ownNeighbors     = ownPawns & neighborMask;

        const bool isDoubledPawn = ownBlockers != BitBoard::Empty;
        const bool isPassedPawn  = !isDoubledPawn && opponentBlockers == BitBoard::Empty;
        const bool isIsolated    = ownNeighbors == BitBoard::Empty;

        int tropismIdx = (int)Piece::Pawn;
        int pstIdx     = (int)Piece::Pawn;

        if (isDoubledPawn) {
            updateTaperedTerm(params, params.doubledPawnPenalty, result.eval, -1);

            tropismIdx = EvalParams::kDoubledPawnTropismIdx;
        } else if (isPassedPawn) {
            pstIdx     = EvalParams::kPassedPawnPstIdx;
            tropismIdx = EvalParams::kPassedPawnTropismIdx;

            const BoardPosition promotionSquare = getPromotionSquare(position, side);
            const int promotionDistance = min(5, getChebyshevDistance(promotionSquare, position));

            const bool isEnemyTurn = side != gameState.getSideToMove();
            const int kingDistance =
                    getChebyshevDistance(promotionSquare, enemyKingPosition) - isEnemyTurn;

            const bool outsideKingSquare = promotionDistance < kingDistance;
            updateTaperedTerm(
                    params, params.passedPawnOutsideKingSquare, result.eval, outsideKingSquare);

            const bool kingCoversPromotion            = (forwardMask & ownKingArea) == forwardMask;
            const bool pawnIsConditionallyUnstoppable = outsideKingSquare || kingCoversPromotion;
            hasConditionallyUnstoppablePawn |= pawnIsConditionallyUnstoppable;

            const std::size_t passedPawnWeightIdx     = file + 1;
            filePassedPawnWeight[passedPawnWeightIdx] = 0.5;

            updateTaperedTerm(
                    params,
                    params.connectedPassedPawnBonus,
                    result.eval,
                    filePassedPawnWeight[passedPawnWeightIdx - 1]
                            + filePassedPawnWeight[passedPawnWeightIdx + 1]);
        } else if (isIsolated) {
            tropismIdx = EvalParams::kIsolatedPawnTropismIdx;
        }

        if (isIsolated) {
            updateTaperedTerm(params, params.isolatedPawnPenalty, result.eval, -1);
        }

        updatePiecePositionEvaluation<CalcJacobians>(params, pstIdx, position, result);

        updateForKingTropism(
                params, ownKingPosition, enemyKingPosition, tropismIdx, position, result.eval);
    }

    const BitBoard& pawnControl  = boardControl.pieceTypeControl[(int)side][(int)Piece::Pawn];
    const BitBoard kingAttack    = enemyKingArea & pawnControl;
    const int numAttackedSquares = popCount(kingAttack);
    result.numKingAttackers += numAttackedSquares;

    updateTaperedTerm(
            params, params.kingAttackWeight[(int)Piece::Pawn], result.eval, numAttackedSquares);

    updatePiecePositionEvaluation<CalcJacobians, /*IsKing*/ true>(
            params, (int)Piece::King, ownKingPosition, result);

    updateForKingOpenFiles(params, side, ownKingPosition, ownPawns, result.eval);
}

template <bool CalcJacobians>
FORCE_INLINE void evaluateUnstoppablePawn(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const Side side,
        const bool hasConditionallyUnstoppablePawn,
        PiecePositionEvaluation<CalcJacobians>& result) {
    if (!hasConditionallyUnstoppablePawn) {
        return;
    }

    const Side enemySide = nextSide(side);

    const bool enemyHasPieces = (gameState.getPieceBitBoard(enemySide, Piece::Knight)
                                 | gameState.getPieceBitBoard(enemySide, Piece::Bishop)
                                 | gameState.getPieceBitBoard(enemySide, Piece::Rook)
                                 | gameState.getPieceBitBoard(enemySide, Piece::Queen))
                             != BitBoard::Empty;

    if (enemyHasPieces) {
        return;
    }

    result.eval.early.value += params.hasUnstoppablePawn;
    result.eval.late.value += params.hasUnstoppablePawn;
    if constexpr (CalcJacobians) {
        const auto paramIdx = params.getParamIndex(params.hasUnstoppablePawn);
        result.eval.early.grad[paramIdx] += 1;
        result.eval.late.grad[paramIdx] += 1;
    }
}

template <bool CalcJacobians>
FORCE_INLINE void updateControlForKing(
        const BitBoard whiteKingArea,
        const BitBoard blackKingArea,
        PiecePositionEvaluation<CalcJacobians>& whiteResult,
        PiecePositionEvaluation<CalcJacobians>& blackResult) {
    const BitBoard kingOverlap      = whiteKingArea & blackKingArea;
    const bool kingsAttackEachOther = kingOverlap != BitBoard::Empty;
    whiteResult.numKingAttackers += kingsAttackEachOther;
    blackResult.numKingAttackers += kingsAttackEachOther;
}

template <bool CalcJacobians>
FORCE_INLINE void evaluatePawnKing(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const BoardControl& boardControl,
        const BoardPosition whiteKingPosition,
        const BoardPosition blackKingPosition,
        const BitBoard whiteKingArea,
        const BitBoard blackKingArea,
        PawnKingEvalHashTable& pawnKingEvalHashTable,
        PiecePositionEvaluation<CalcJacobians>& whiteResult,
        PiecePositionEvaluation<CalcJacobians>& blackResult) {
    bool whiteHasConditionallyUnstoppablePawn{};
    bool blackHasConditionallyUnstoppablePawn{};

    evaluatePawnKingForSide(
            params,
            gameState,
            boardControl,
            Side::White,
            whiteKingPosition,
            blackKingPosition,
            whiteKingArea,
            blackKingArea,
            whiteResult,
            whiteHasConditionallyUnstoppablePawn);

    evaluatePawnKingForSide(
            params,
            gameState,
            boardControl,
            Side::Black,
            blackKingPosition,
            whiteKingPosition,
            blackKingArea,
            whiteKingArea,
            blackResult,
            blackHasConditionallyUnstoppablePawn);

    if (!pawnKingEvalHashTable.empty()) {
        const PawnKingEvalInfo pawnKingEvalInfo{
                .earlyEval = whiteResult.eval.early.value - blackResult.eval.early.value,
                .lateEval  = whiteResult.eval.late.value - blackResult.eval.late.value,

                .phaseMaterial = whiteResult.phaseMaterial.value + blackResult.phaseMaterial.value,

                .whiteHasConditionallyUnstoppablePawn = whiteHasConditionallyUnstoppablePawn,
                .blackHasConditionallyUnstoppablePawn = blackHasConditionallyUnstoppablePawn,
        };

        pawnKingEvalHashTable.store(gameState.getPawnKingHash(), pawnKingEvalInfo);
    }

    updateControlForKing(whiteKingArea, blackKingArea, whiteResult, blackResult);

    evaluateUnstoppablePawn(
            params, gameState, Side::White, whiteHasConditionallyUnstoppablePawn, whiteResult);

    evaluateUnstoppablePawn(
            params, gameState, Side::Black, blackHasConditionallyUnstoppablePawn, blackResult);
}

FORCE_INLINE void useStoredPawnKingEval(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const BoardControl& boardControl,
        const BitBoard whiteKingArea,
        const BitBoard blackKingArea,
        const PawnKingEvalInfo& pawnKingEvalInfo,
        PiecePositionEvaluation<false>& whiteResult,
        PiecePositionEvaluation<false>& blackResult) {
    // Apply the retrieved eval and phase material values to the white result, and leave the black result unchanged.
    // Eventually we'll just sum the white and black results together anyway.
    whiteResult.eval.early.value += pawnKingEvalInfo.earlyEval;
    whiteResult.eval.late.value += pawnKingEvalInfo.lateEval;

    whiteResult.phaseMaterial.value += pawnKingEvalInfo.phaseMaterial;

    /*
    const BitBoard& pawnControl  = boardControl.pieceTypeControl[(int)side][(int)Piece::Pawn];
    const BitBoard kingAttack    = enemyKingArea & pawnControl;
    const int numAttackedSquares = popCount(kingAttack);
    result.numKingAttackers += numAttackedSquares;
    */
    const auto updateFromControlForSide = [](const BitBoard pawnControl,
                                             const BitBoard enemyKingArea,
                                             PiecePositionEvaluation<false>& result) {
        const BitBoard kingAttack    = enemyKingArea & pawnControl;
        const int numAttackedSquares = popCount(kingAttack);
        result.numKingAttackers += numAttackedSquares;
    };

    updateFromControlForSide(
            boardControl.pieceTypeControl[(int)Side::White][(int)Piece::Pawn],
            blackKingArea,
            whiteResult);
    updateFromControlForSide(
            boardControl.pieceTypeControl[(int)Side::Black][(int)Piece::Pawn],
            whiteKingArea,
            blackResult);

    updateControlForKing(whiteKingArea, blackKingArea, whiteResult, blackResult);

    evaluateUnstoppablePawn(
            params,
            gameState,
            Side::White,
            pawnKingEvalInfo.whiteHasConditionallyUnstoppablePawn,
            whiteResult);

    evaluateUnstoppablePawn(
            params,
            gameState,
            Side::Black,
            pawnKingEvalInfo.blackHasConditionallyUnstoppablePawn,
            blackResult);
}

template <bool CalcJacobians>
FORCE_INLINE void evaluatePawnKingOrRetrieve(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const BoardControl& boardControl,
        const BoardPosition whiteKingPosition,
        const BoardPosition blackKingPosition,
        const BitBoard whiteKingArea,
        const BitBoard blackKingArea,
        PawnKingEvalHashTable& pawnKingEvalHashTable,
        PiecePositionEvaluation<CalcJacobians>& whiteResult,
        PiecePositionEvaluation<CalcJacobians>& blackResult) {
    if constexpr (!CalcJacobians) {
        if (!pawnKingEvalHashTable.empty()) {
            const auto retrievedInfo = pawnKingEvalHashTable.probe(gameState.getPawnKingHash());

            if (retrievedInfo) {
                useStoredPawnKingEval(
                        params,
                        gameState,
                        boardControl,
                        whiteKingArea,
                        blackKingArea,
                        *retrievedInfo,
                        whiteResult,
                        blackResult);

                return;
            }
        }
    }

    evaluatePawnKing(
            params,
            gameState,
            boardControl,
            whiteKingPosition,
            blackKingPosition,
            whiteKingArea,
            blackKingArea,
            pawnKingEvalHashTable,
            whiteResult,
            blackResult);
}

[[nodiscard]] FORCE_INLINE ParamGradient<true> getMaxPhaseMaterialGradient(
        const EvalParams& params) {
    /*
    maxPhaseMaterial = 2 * 8 * evalParams.phaseMaterialValues[(int)Piece::Pawn]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Knight]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Bishop]
                     + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Rook]
                     + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::Queen]
                     + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::King];
    */
    ParamGradient<true> gradient = zeroGradient<true>();
    gradient[params.getParamIndex(params.phaseMaterialValues[(int)Piece::Pawn])] += 2 * 8;
    gradient[params.getParamIndex(params.phaseMaterialValues[(int)Piece::Knight])] += 2 * 2;
    gradient[params.getParamIndex(params.phaseMaterialValues[(int)Piece::Bishop])] += 2 * 2;
    gradient[params.getParamIndex(params.phaseMaterialValues[(int)Piece::Rook])] += 2 * 2;
    gradient[params.getParamIndex(params.phaseMaterialValues[(int)Piece::Queen])] += 2 * 1;
    gradient[params.getParamIndex(params.phaseMaterialValues[(int)Piece::King])] += 2 * 1;
    return gradient;
}

[[nodiscard]] FORCE_INLINE EvalCalcT calcTaperedValue(
        const EvalCalcT earlyValue,
        const EvalCalcT lateValue,
        const EvalCalcT earlyFactor,
        const EvalCalcT lateFactor) {
    return earlyValue * earlyFactor + lateValue * lateFactor;
}

[[nodiscard]] ParamGradient<true> calcTaperedGradient(
        const VectorT& earlyGradient,
        const VectorT& lateGradient,
        const VectorT& earlyFactorGradient,
        const EvalCalcT earlyValue,
        const EvalCalcT lateValue,
        const EvalCalcT earlyFactor,
        const EvalCalcT lateFactor) {
    return earlyGradient * earlyFactor + earlyFactorGradient * earlyValue
         + lateGradient * lateFactor - earlyFactorGradient * lateValue;
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE std::tuple<float, float, ParamGradient<CalcJacobians>> calcTaperParams(
        const Evaluator::EvalCalcParams& params,
        const PiecePositionEvaluation<CalcJacobians>& whitePiecePositionEval,
        const PiecePositionEvaluation<CalcJacobians>& blackPiecePositionEval) {
    const EvalCalcT phaseMaterial =
            whitePiecePositionEval.phaseMaterial.value + blackPiecePositionEval.phaseMaterial.value;
    const float earlyFactor = (float)phaseMaterial / (float)params.maxPhaseMaterial_;
    const float lateFactor  = 1.f - earlyFactor;

    ParamGradient<CalcJacobians> earlyFactorGradient;
    if constexpr (CalcJacobians) {
        const ParamGradient<CalcJacobians> phaseMaterialGradient =
                whitePiecePositionEval.phaseMaterial.grad
                + blackPiecePositionEval.phaseMaterial.grad;

        const ParamGradient<CalcJacobians> maxPhaseMaterialGradient =
                getMaxPhaseMaterialGradient(params);

        earlyFactorGradient = (phaseMaterialGradient * params.maxPhaseMaterial_
                               - phaseMaterial * maxPhaseMaterialGradient)
                            / (params.maxPhaseMaterial_ * params.maxPhaseMaterial_);
    }

    return {earlyFactor, lateFactor, earlyFactorGradient};
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE TermWithGradient<CalcJacobians> calcTaperedEval(
        const PiecePositionEvaluation<CalcJacobians>& whitePiecePositionEval,
        const PiecePositionEvaluation<CalcJacobians>& blackPiecePositionEval,
        const float earlyFactor,
        const float lateFactor,
        const ParamGradient<CalcJacobians>& earlyFactorGradient) {
    const EvalCalcT earlyEval =
            whitePiecePositionEval.eval.early.value - blackPiecePositionEval.eval.early.value;
    const EvalCalcT lateEval =
            whitePiecePositionEval.eval.late.value - blackPiecePositionEval.eval.late.value;

    TermWithGradient<CalcJacobians> result;

    result.value = calcTaperedValue(earlyEval, lateEval, earlyFactor, lateFactor);

    if constexpr (CalcJacobians) {
        const ParamGradient<CalcJacobians> earlyEvalGradient =
                whitePiecePositionEval.eval.early.grad - blackPiecePositionEval.eval.early.grad;

        const ParamGradient<CalcJacobians> lateEvalGradient =
                whitePiecePositionEval.eval.late.grad - blackPiecePositionEval.eval.late.grad;

        result.grad = calcTaperedGradient(
                earlyEvalGradient,
                lateEvalGradient,
                earlyFactorGradient,
                earlyEval,
                lateEval,
                earlyFactor,
                lateFactor);
    }

    return result;
}

FORCE_INLINE void getPstMapping(
        const BoardPosition whiteKingPosition,
        const BoardPosition blackKingPosition,
        const PstMapping*& whiteMapping,
        const PstMapping*& whiteKingMapping,
        const PstMapping*& blackMapping,
        const PstMapping*& blackKingMapping) {
    const bool whiteKingIsOnQueenSide = fileFromPosition(whiteKingPosition) < 4;
    const bool blackKingIsOnQueenSide = fileFromPosition(blackKingPosition) < 4;

    if (!whiteKingIsOnQueenSide && !blackKingIsOnQueenSide) {
        // Both on king side: default
        whiteMapping     = &kPstMappings.defaultIdx[0];
        whiteKingMapping = &kPstMappings.defaultIdx[0];

        blackMapping     = &kPstMappings.defaultIdx[1];
        blackKingMapping = &kPstMappings.defaultIdx[1];
    } else if (whiteKingIsOnQueenSide && !blackKingIsOnQueenSide) {
        // White is on queen side: use flipped white mapping for pieces, and fully flipped mapping
        // for black king
        whiteMapping     = &kPstMappings.whiteSideFlipped[0];
        whiteKingMapping = &kPstMappings.defaultIdx[0];

        blackMapping     = &kPstMappings.whiteSideFlipped[1];
        blackKingMapping = &kPstMappings.bothSidesFlipped[1];
    } else if (!whiteKingIsOnQueenSide && blackKingIsOnQueenSide) {
        // Black is on queen side: use flipped black mapping for pieces, and fully flipped mapping
        // for white king
        whiteMapping     = &kPstMappings.blackSideFlipped[0];
        whiteKingMapping = &kPstMappings.bothSidesFlipped[0];

        blackMapping     = &kPstMappings.blackSideFlipped[1];
        blackKingMapping = &kPstMappings.defaultIdx[1];
    } else {  // whiteKingIsOnQueenSide && blackKingIsOnQueenSide
        // Both on queen side: use fully flipped mapping for both sides
        whiteMapping     = &kPstMappings.bothSidesFlipped[0];
        whiteKingMapping = &kPstMappings.bothSidesFlipped[0];

        blackMapping     = &kPstMappings.bothSidesFlipped[1];
        blackKingMapping = &kPstMappings.bothSidesFlipped[1];
    }
}

template <bool CalcJacobians>
[[nodiscard]] FORCE_INLINE TermWithGradient<CalcJacobians> evaluateForWhite(
        const Evaluator::EvalCalcParams& params,
        const GameState& gameState,
        const BoardControl& boardControl,
        PawnKingEvalHashTable& pawnKingEvalHashTable) {

    const BoardPosition whiteKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(Side::White, Piece::King));
    const BoardPosition blackKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(Side::Black, Piece::King));

    PiecePositionEvaluation<CalcJacobians> whitePiecePositionEval{};
    PiecePositionEvaluation<CalcJacobians> blackPiecePositionEval{};

    const BitBoard whiteKingArea =
            getPieceControlledSquares(Piece::King, whiteKingPosition, BitBoard::Empty);
    const BitBoard blackKingArea =
            getPieceControlledSquares(Piece::King, blackKingPosition, BitBoard::Empty);

    // No need to add king attack weight for the king itself: that would be symmetric.

    getPstMapping(
            whiteKingPosition,
            blackKingPosition,
            whitePiecePositionEval.pstIndex,
            whitePiecePositionEval.pstIndexKing,
            blackPiecePositionEval.pstIndex,
            blackPiecePositionEval.pstIndexKing);

    evaluatePawnKingOrRetrieve(
            params,
            gameState,
            boardControl,
            whiteKingPosition,
            blackKingPosition,
            whiteKingArea,
            blackKingArea,
            pawnKingEvalHashTable,
            whitePiecePositionEval,
            blackPiecePositionEval);

    evaluatePiecePositionsForSide(
            params,
            gameState,
            boardControl,
            Side::White,
            whiteKingPosition,
            blackKingPosition,
            blackKingArea,
            whitePiecePositionEval);
    evaluatePiecePositionsForSide(
            params,
            gameState,
            boardControl,
            Side::Black,
            blackKingPosition,
            whiteKingPosition,
            whiteKingArea,
            blackPiecePositionEval);

    evaluateAttackDefend(params, gameState, boardControl, Side::White, whitePiecePositionEval.eval);

    evaluateAttackDefend(params, gameState, boardControl, Side::Black, blackPiecePositionEval.eval);

    const EvalCalcT tempoFactor = gameState.getSideToMove() == Side::White ? 1.f : -1.f;
    updateTaperedTerm(params, params.tempoBonus, whitePiecePositionEval.eval, tempoFactor);

    const auto [earlyFactor, lateFactor, earlyFactorGradient] =
            calcTaperParams(params, whitePiecePositionEval, blackPiecePositionEval);

    auto taperedEval = calcTaperedEval(
            whitePiecePositionEval,
            blackPiecePositionEval,
            earlyFactor,
            lateFactor,
            earlyFactorGradient);

    correctForDrawish<CalcJacobians>(gameState, params, taperedEval);

    return taperedEval;
}

[[nodiscard]] FORCE_INLINE std::pair<bool, bool> insufficientMaterialForSides(
        const GameState& gameState) {
    bool whiteInsufficientMaterial = false;
    bool blackInsufficientMaterial = false;

    const bool noWhitePawns =
            gameState.getPieceBitBoard(Side::White, Piece::Pawn) == BitBoard::Empty;
    const bool noWhiteRooks =
            gameState.getPieceBitBoard(Side::White, Piece::Rook) == BitBoard::Empty;
    const bool noWhiteQueens =
            gameState.getPieceBitBoard(Side::White, Piece::Queen) == BitBoard::Empty;

    const bool whiteOnlyHasMinorPieces = noWhitePawns && noWhiteRooks && noWhiteQueens;

    const int numWhiteKnights = popCount(gameState.getPieceBitBoard(Side::White, Piece::Knight));
    const int numWhiteBishops = popCount(gameState.getPieceBitBoard(Side::White, Piece::Bishop));

    const bool whiteOnlyHasAKing =
            whiteOnlyHasMinorPieces && numWhiteKnights == 0 && numWhiteBishops == 0;

    const bool noBlackPawns =
            gameState.getPieceBitBoard(Side::Black, Piece::Pawn) == BitBoard::Empty;
    const bool noBlackRooks =
            gameState.getPieceBitBoard(Side::Black, Piece::Rook) == BitBoard::Empty;
    const bool noBlackQueens =
            gameState.getPieceBitBoard(Side::Black, Piece::Queen) == BitBoard::Empty;

    const bool blackOnlyHasMinorPieces = noBlackPawns && noBlackRooks && noBlackQueens;

    const int numBlackKnights = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Knight));
    const int numBlackBishops = popCount(gameState.getPieceBitBoard(Side::Black, Piece::Bishop));

    const bool blackOnlyHasAKing =
            blackOnlyHasMinorPieces && numBlackKnights == 0 && numBlackBishops == 0;

    if (whiteOnlyHasMinorPieces) {
        if (numWhiteKnights == 0 && numWhiteBishops <= 1) {
            whiteInsufficientMaterial = true;
        } else if (numWhiteBishops == 0 && numWhiteKnights <= 1) {
            whiteInsufficientMaterial = true;
        }
    }

    if (blackOnlyHasMinorPieces) {
        if (numBlackKnights == 0 && numBlackBishops <= 1) {
            blackInsufficientMaterial = true;
        } else if (numBlackBishops == 0 && numBlackKnights <= 1) {
            blackInsufficientMaterial = true;
        }
    }

    if (whiteOnlyHasAKing && blackOnlyHasMinorPieces && numBlackBishops == 0
        && numBlackKnights == 2) {
        blackInsufficientMaterial = true;
    }

    if (blackOnlyHasAKing && whiteOnlyHasMinorPieces && numWhiteBishops == 0
        && numWhiteKnights == 2) {
        whiteInsufficientMaterial = true;
    }

    return {whiteInsufficientMaterial, blackInsufficientMaterial};
}

}  // namespace

PawnKingEvalHashTable::PawnKingEvalHashTable(const bool nonEmpty) {
    if (nonEmpty) {
        // NOLINTNEXTLINE(cppcoreguidelines-avoid-c-arrays)
        data_ = std::make_unique<Entry[]>(kPawnKingHashTableEntries);
    }
}

FORCE_INLINE std::optional<PawnKingEvalInfo> PawnKingEvalHashTable::probe(const HashT hash) const {
    MY_ASSERT(!empty());

    const std::size_t index = hash & kPawnKingHashTableMask;

    const Entry& entry = data_[index];
    if (entry.hash == hash) {
        return entry.info;
    }

    return std::nullopt;
}

FORCE_INLINE void PawnKingEvalHashTable::prefetch(const HashT hash) const {
    MY_ASSERT(!empty());

    const std::size_t index = hash & kPawnKingHashTableMask;

    ::prefetch(&data_[index]);
}

FORCE_INLINE void PawnKingEvalHashTable::store(HashT hash, const PawnKingEvalInfo& info) {
    MY_ASSERT(!empty());

    const std::size_t index = hash & kPawnKingHashTableMask;

    Entry& entry = data_[index];
    entry.hash   = hash;
    entry.info   = info;
}

Evaluator::EvalCalcParams::EvalCalcParams(const EvalParams& evalParams)
    : EvalParams(evalParams),
      maxPhaseMaterial_(
              2 * 8 * evalParams.phaseMaterialValues[(int)Piece::Pawn]
              + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Knight]
              + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Bishop]
              + 2 * 2 * evalParams.phaseMaterialValues[(int)Piece::Rook]
              + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::Queen]
              + 2 * 1 * evalParams.phaseMaterialValues[(int)Piece::King]) {}

Evaluator::Evaluator(bool usePawnKingEvalHashTable)
    : Evaluator(EvalParams::getDefaultParams(), usePawnKingEvalHashTable) {}

Evaluator::Evaluator(const EvalParams& params, bool usePawnKingEvalHashTable)
    : params_(params), pawnKingEvalHashTable_(/*nonEmpty*/ usePawnKingEvalHashTable) {}

FORCE_INLINE int Evaluator::getPieceSquareValue(
        const Piece piece, BoardPosition position, const Side side) const {
    // Subtract out the piece values from the PSTs. That way the returned value is a good
    // representation of how good this square is for the given piece, which is useful for heuristics
    // in the search.
    // We also choose the early values. The values returned by this function are used for history
    // heuristic initialization, so that makes sense. They're also used for move ordering for which
    // the choice is perhaps a bit arbitrary.

    const int squareIndex = kPstMappings.defaultIdx[(int)side][(int)position];

    return (int)(params_.pieceSquareTables[(int)piece][squareIndex].early
                 - params_.pieceValues[(int)piece].early);
}

EvalCalcT Evaluator::evaluateRaw(const GameState& gameState) const {
    const auto rawEvalWhite = evaluateForWhite<false>(
            params_, gameState, gameState.getBoardControl(), pawnKingEvalHashTable_);

    return gameState.getSideToMove() == Side::White ? rawEvalWhite.value : -rawEvalWhite.value;
}

EvalWithGradient Evaluator::evaluateWithGradient(const GameState& gameState) const {
    const auto rawEvalWhite = evaluateForWhite<true>(
            params_, gameState, gameState.getBoardControl(), pawnKingEvalHashTable_);

    const EvalCalcT colorFactor = gameState.getSideToMove() == Side::White ? 1.f : -1.f;

    return {.eval = colorFactor * rawEvalWhite.value, .gradient = colorFactor * rawEvalWhite.grad};
}

FORCE_INLINE EvalT Evaluator::evaluate(const GameState& gameState) const {
    return evaluate(gameState, gameState.getBoardControl());
}

EvalT Evaluator::evaluate(const GameState& gameState, const BoardControl& boardControl) const {
    const auto rawEvalWhite =
            evaluateForWhite<false>(params_, gameState, boardControl, pawnKingEvalHashTable_);

    const EvalT clampedEvalWhite =
            (EvalT)clamp((int)rawEvalWhite.value, -kMateEval + 1'000, kMateEval - 1'000);

    return gameState.getSideToMove() == Side::White ? clampedEvalWhite : -clampedEvalWhite;
}

FORCE_INLINE void Evaluator::prefetch(const GameState& gameState) const {
    if (pawnKingEvalHashTable_.empty()) {
        return;
    }

    pawnKingEvalHashTable_.prefetch(gameState.getPawnKingHash());
}

FORCE_INLINE int getStaticPieceValue(const Piece piece) {
    static constexpr std::array kStaticPieceValues = {
            100,      // Pawn
            305,      // Knight
            308,      // Bishop
            563,      // Rook
            950,      // Queen
            20'000,   // King
            INT_MIN,  // (not a possible value)
            0,        // Invalid (aka none)
    };

    return kStaticPieceValues[(int)piece];
}

FORCE_INLINE bool isInsufficientMaterial(const GameState& gameState) {
    const auto [whiteInsufficientMaterial, blackInsufficientMaterial] =
            insufficientMaterialForSides(gameState);

    return whiteInsufficientMaterial && blackInsufficientMaterial;
}

FORCE_INLINE EvalT evaluateNoLegalMoves(const GameState& gameState) {
    if (gameState.isInCheck()) {
        // We're in check and there are no legal moves so we're in checkmate.
        return -kMateEval;
    }

    // We're not in check and there are no legal moves so this is a stalemate.
    return 0;
}

FORCE_INLINE void getPstMapping(
        const GameState& gameState,
        const PstMapping*& whiteMapping,
        const PstMapping*& whiteKingMapping,
        const PstMapping*& blackMapping,
        const PstMapping*& blackKingMapping) {
    const BoardPosition whiteKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(Side::White, Piece::King));
    const BoardPosition blackKingPosition =
            getFirstSetPosition(gameState.getPieceBitBoard(Side::Black, Piece::King));

    getPstMapping(
            whiteKingPosition,
            blackKingPosition,
            whiteMapping,
            whiteKingMapping,
            blackMapping,
            blackKingMapping);
}
