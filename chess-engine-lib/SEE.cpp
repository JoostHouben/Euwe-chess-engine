#include "SEE.h"

#include "Eval.h"
#include "Macros.h"
#include "Math.h"
#include "PieceControl.h"

#include <utility>

namespace {

[[nodiscard]] FORCE_INLINE BitBoard getSlidingPiecesControllingSquare(
        const BoardPosition targetSquare,
        const BitBoard anyPiece,
        const PieceBitBoards& pieceBitBoards) {

    const BitBoard rookControlFromTarget = getRookAttack(targetSquare, anyPiece);
    const BitBoard rookMovePieces =
            pieceBitBoards[0][(int)Piece::Rook] | pieceBitBoards[0][(int)Piece::Queen]
            | pieceBitBoards[1][(int)Piece::Rook] | pieceBitBoards[1][(int)Piece::Queen];

    const BitBoard bishopControlFromTarget = getBishopAttack(targetSquare, anyPiece);
    const BitBoard bishopMovePieces =
            pieceBitBoards[0][(int)Piece::Bishop] | pieceBitBoards[0][(int)Piece::Queen]
            | pieceBitBoards[1][(int)Piece::Bishop] | pieceBitBoards[1][(int)Piece::Queen];

    return (rookControlFromTarget & rookMovePieces) | (bishopControlFromTarget & bishopMovePieces);
}

[[nodiscard]] FORCE_INLINE BitBoard getAllPiecesControllingSquare(
        const BoardPosition targetSquare,
        const PieceBitBoards& pieceBitBoards,
        const BitBoard anyPiece) {
    BitBoard controllingPieces =
            getSlidingPiecesControllingSquare(targetSquare, anyPiece, pieceBitBoards);

    const BitBoard knightControlFromTarget =
            getPieceControlledSquares(Piece::Knight, targetSquare, anyPiece);
    const BitBoard knightPieces =
            pieceBitBoards[0][(int)Piece::Knight] | pieceBitBoards[1][(int)Piece::Knight];

    controllingPieces = controllingPieces | (knightControlFromTarget & knightPieces);

    const BitBoard kingControlFromTarget =
            getPieceControlledSquares(Piece::King, targetSquare, anyPiece);
    const BitBoard kingPieces =
            pieceBitBoards[0][(int)Piece::King] | pieceBitBoards[1][(int)Piece::King];

    controllingPieces = controllingPieces | (kingControlFromTarget & kingPieces);

    const BitBoard targetSquareBitBoard = BitBoard::Empty | targetSquare;
    const BitBoard whitePawnControlFromTarget =
            getPawnControlledSquares(targetSquareBitBoard, Side::White);

    controllingPieces =
            controllingPieces
            | (whitePawnControlFromTarget & pieceBitBoards[(int)Side::Black][(int)Piece::Pawn]);

    const BitBoard blackPawnControlFromTarget =
            getPawnControlledSquares(targetSquareBitBoard, Side::Black);

    controllingPieces =
            controllingPieces
            | (blackPawnControlFromTarget & pieceBitBoards[(int)Side::White][(int)Piece::Pawn]);

    return controllingPieces;
}

[[nodiscard]] FORCE_INLINE std::pair<Piece, BitBoard> getLeastValuableAttacker(
        const BitBoard controllingPieces,
        const PieceBitBoards& pieceBitBoards,
        const int minimumAttackerIdx,
        const Side side) {
    const auto& sidePieceBitBoards = pieceBitBoards[(int)side];

    for (int pieceIdx = minimumAttackerIdx; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        const BitBoard pieceAttackers = controllingPieces & sidePieceBitBoards[pieceIdx];
        if (pieceAttackers != BitBoard::Empty) {
            return {(Piece)pieceIdx, getFirstSetBitBoard(pieceAttackers)};
        }
    }

    return {Piece::Invalid, BitBoard::Empty};
}

[[nodiscard]] FORCE_INLINE BitBoard getPiecesThatCanTakeAlongXRay(const GameState& gameState) {
    // Don't need to consider kings, because an exchagne sequence always ends once the king has been
    // captured
    return gameState.getPieceBitBoard(Side::White, Piece::Pawn)
         | gameState.getPieceBitBoard(Side::White, Piece::Bishop)
         | gameState.getPieceBitBoard(Side::White, Piece::Rook)
         | gameState.getPieceBitBoard(Side::White, Piece::Queen)
         | gameState.getPieceBitBoard(Side::Black, Piece::Pawn)
         | gameState.getPieceBitBoard(Side::Black, Piece::Bishop)
         | gameState.getPieceBitBoard(Side::Black, Piece::Rook)
         | gameState.getPieceBitBoard(Side::Black, Piece::Queen);
}

[[nodiscard]] FORCE_INLINE std::pair<BitBoard, Piece> getAnyPieceAndTargetPiece(
        const GameState& gameState, const Move& move) {
    const BoardPosition targetSquare = move.to;

    BitBoard anyPiece = gameState.getOccupancy().ownPiece | gameState.getOccupancy().enemyPiece;

    Piece targetPiece = getPiece(gameState.getPieceOnSquare(move.to));

    if (isEnPassant(move.flags)) {
        const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
        const auto [toFile, toRank]     = fileRankFromPosition(move.to);
        BoardPosition captureTarget     = positionFromFileRank(toFile, fromRank);
        anyPiece &= ~captureTarget;
        anyPiece |= targetSquare;

        targetPiece = Piece::Pawn;
    }

    return {anyPiece, targetPiece};
}

FORCE_INLINE void updateForXRay(
        const BitBoard piecesThatCanTakeAlongXray,
        const BitBoard vacatedSquare,
        const BoardPosition targetSquare,
        const BitBoard anyPiece,
        const PieceBitBoards& pieceBitBoards,
        BitBoard& controllingPieces,
        std::array<int, kNumSides>& minimumAttackerIdx) {
    if ((piecesThatCanTakeAlongXray & vacatedSquare) != BitBoard::Empty) {
        // Re-calculate sliding piece attacks to account for x-rays
        const BitBoard controllingSlidingPieces =
                getSlidingPiecesControllingSquare(targetSquare, anyPiece, pieceBitBoards);

        // We need to intersect with anyPiece so that we ignore sliding pieces that already
        // entered the exchange.
        controllingPieces = controllingPieces | (controllingSlidingPieces & anyPiece);

        minimumAttackerIdx[0] = min(minimumAttackerIdx[0], (int)Piece::Bishop);
        minimumAttackerIdx[1] = min(minimumAttackerIdx[1], (int)Piece::Bishop);
    }
}

template <bool ReturnBound, bool ReturnMeets>
[[nodiscard]] FORCE_INLINE auto staticExchangeEvaluationImpl(
        const GameState& gameState, const Move& move, const int threshold) {
    // TODO: consider promotions? Pins?

    static_assert(!(ReturnBound && ReturnMeets));

    const BoardPosition targetSquare = move.to;

    auto [anyPiece, targetPiece] = getAnyPieceAndTargetPiece(gameState, move);

    std::array<int, kNumTotalPieces> gain{};
    int exchangeIdx     = 0;
    gain[exchangeIdx++] = getPieceValue(targetPiece);

    if constexpr (ReturnBound) {
        if (gain[0] < threshold) {
            return gain[0];
        }
    } else if constexpr (ReturnMeets) {
        ASSUME(gain[0] > 0);  // if we pass threshold = 0, compiler can remove this branch
        if (gain[0] < threshold) {
            return false;
        }
    }

    const PieceBitBoards& pieceBitBoards = gameState.getPieceBitBoards();

    const BitBoard piecesThatCanTakeAlongXray = getPiecesThatCanTakeAlongXRay(gameState);

    std::array<int, kNumSides> minimumAttackerIdx = {0, 0};

    targetPiece            = move.pieceToMove;
    BitBoard vacatedSquare = BitBoard::Empty | move.from;

    BitBoard controllingPieces =
            getAllPiecesControllingSquare(targetSquare, pieceBitBoards, anyPiece);

    const Side sideToMove = gameState.getSideToMove();
    for (;; ++exchangeIdx) {
        // Gain from the perspective of current side to move if the target piece is taken
        gain[exchangeIdx] = getPieceValue(targetPiece) - gain[exchangeIdx - 1ULL];

        const Side side = (Side)((exchangeIdx + (int)sideToMove) & 1);

        if constexpr (ReturnMeets) {
            if (side == sideToMove) {
                if (gain[exchangeIdx] < threshold) {
                    // Even if we take the target piece, we're still below the threshold.
                    // If the exchange gets to this point it will never rise above the threshold again,
                    // so we don't need to consider this capture or what happens after this point.
                    break;
                }
            } else {
                if (-gain[exchangeIdx] >= threshold) {
                    // Even if we take the target piece, the gain for the opponent is still above the
                    // threshold. So if the exchange gets to this point it will never drop below the
                    // threshold again, so we don't need to consider this capture what happens after
                    // this point.
                    break;
                }
            }
        }

        // Deferred update of controllingPieces and anyPiece
        controllingPieces &= ~vacatedSquare;
        anyPiece &= ~vacatedSquare;

        updateForXRay(
                piecesThatCanTakeAlongXray,
                vacatedSquare,
                targetSquare,
                anyPiece,
                pieceBitBoards,
                controllingPieces,
                minimumAttackerIdx);

        auto [attacker, attackerSquare] = getLeastValuableAttacker(
                controllingPieces, pieceBitBoards, minimumAttackerIdx[(int)side], side);

        if (attacker == Piece::Invalid) {
            // No more attackers left.
            break;
        }

        if constexpr (ReturnBound) {
            if (side == sideToMove) {
                if (gain[exchangeIdx] < threshold) {
                    // Even after taking the target piece, we're still below the threshold.
                    // If the exchange gets to this point it will never rise above the threshold again,
                    // so we don't need to consider what happens after this point.
                    ++exchangeIdx;  // We do need to consider the last capture to get an accurate bound.
                    break;
                }
            } else {
                if (-gain[exchangeIdx] >= threshold) {
                    // Even after taking the target piece, the gain for the opponent is still above the
                    // threshold. So if the exchange gets to this point it will never drop below the
                    // threshold again, so we don't need to consider what happens after this point.
                    ++exchangeIdx;  // We do need to consider the last capture to get an accurate bound.
                    break;
                }
            }
        }

        minimumAttackerIdx[(int)side] = (int)attacker;

        // If the attacker takes, it becomes the new target and its square is vacated.
        // Defer updating of controllingPieces and anyPiece.
        targetPiece   = attacker;
        vacatedSquare = attackerSquare;
    }

    // After captures 0...i-1, if we stop the exchange the gain from the perspective of the player
    // to move is -gain[i-1]. If we continue the exchange our gain becomes gain[i]. So the value of
    // this position from our perspective is max(-gain[i-1], gain[i]), since we can choose to end
    // the exchange.
    // So from the previous player's perspective the gain after captures 0...i-1 becomes the
    // negation of that: -max(-gain[i-1], gain[i])
    for (int i = exchangeIdx - 1; i > 0; --i) {
        gain[i - 1ULL] = -max(-gain[i - 1ULL], gain[i]);
    }

    if constexpr (ReturnMeets) {
        return gain[0] >= threshold;
    } else {
        return gain[0];
    }
}

}  // namespace

int staticExchangeEvaluation(const GameState& gameState, const Move& move) {
    return staticExchangeEvaluationImpl</*ReturnBound =*/false, /*ReturnMeets =*/false>(
            gameState, move, /*unused*/ 0);
}

int staticExchangeEvaluationBound(
        const GameState& gameState, const Move& move, const int threshold) {
    return staticExchangeEvaluationImpl</*ReturnBound =*/true, /*ReturnMeets =*/false>(
            gameState, move, threshold);
}

bool staticExchangeEvaluationNonLosing(const GameState& gameState, const Move& move) {
    return staticExchangeEvaluationImpl</*ReturnBound =*/false, /*ReturnMeets =*/true>(
            gameState, move, /*threshold*/ 0);
}
