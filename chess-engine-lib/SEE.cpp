#include "SEE.h"

#include "Eval.h"
#include "Macros.h"
#include "Math.h"
#include "PieceControl.h"

#include <utility>

namespace {

FORCE_INLINE BitBoard getSlidingPiecesControllingSquare(
        const BoardPosition targetSquare,
        const BitBoard anyPiece,
        const PieceBitBoards& pieceBitBoards) {

    const BitBoard rookControlFromTarget = getRookAttack(targetSquare, anyPiece);
    const BitBoard rookMovePieces =
            any(pieceBitBoards[0][(int)Piece::Rook],
                pieceBitBoards[0][(int)Piece::Queen],
                pieceBitBoards[1][(int)Piece::Rook],
                pieceBitBoards[1][(int)Piece::Queen]);

    const BitBoard bishopControlFromTarget = getBishopAttack(targetSquare, anyPiece);
    const BitBoard bishopMovePieces =
            any(pieceBitBoards[0][(int)Piece::Bishop],
                pieceBitBoards[0][(int)Piece::Queen],
                pieceBitBoards[1][(int)Piece::Bishop],
                pieceBitBoards[1][(int)Piece::Queen]);

    return any(
            intersection(rookControlFromTarget, rookMovePieces),
            intersection(bishopControlFromTarget, bishopMovePieces));
}

FORCE_INLINE BitBoard getAllPiecesControllingSquare(
        const BoardPosition targetSquare,
        const PieceBitBoards& pieceBitBoards,
        const BitBoard anyPiece) {
    BitBoard controllingPieces =
            getSlidingPiecesControllingSquare(targetSquare, anyPiece, pieceBitBoards);

    const BitBoard knightControlFromTarget =
            getPieceControlledSquares(Piece::Knight, targetSquare, anyPiece);
    const BitBoard knightPieces =
            any(pieceBitBoards[0][(int)Piece::Knight], pieceBitBoards[1][(int)Piece::Knight]);

    controllingPieces = any(controllingPieces, intersection(knightControlFromTarget, knightPieces));

    const BitBoard kingControlFromTarget =
            getPieceControlledSquares(Piece::King, targetSquare, anyPiece);
    const BitBoard kingPieces =
            any(pieceBitBoards[0][(int)Piece::King], pieceBitBoards[1][(int)Piece::King]);

    controllingPieces = any(controllingPieces, intersection(kingControlFromTarget, kingPieces));

    BitBoard targetSquareBitBoard = BitBoard::Empty;
    set(targetSquareBitBoard, targetSquare);
    const BitBoard whitePawnControlFromTarget =
            getPawnControlledSquares(targetSquareBitBoard, Side::White);

    controllingPieces =
            any(controllingPieces,
                intersection(
                        whitePawnControlFromTarget,
                        pieceBitBoards[(int)Side::Black][(int)Piece::Pawn]));

    const BitBoard blackPawnControlFromTarget =
            getPawnControlledSquares(targetSquareBitBoard, Side::Black);

    controllingPieces =
            any(controllingPieces,
                intersection(
                        blackPawnControlFromTarget,
                        pieceBitBoards[(int)Side::White][(int)Piece::Pawn]));

    return controllingPieces;
}

FORCE_INLINE std::pair<Piece, BitBoard> getLeastValuableAttacker(
        const BitBoard controllingPieces,
        const PieceBitBoards& pieceBitBoards,
        const int minimumAttackerIdx,
        const Side side) {
    const auto& sidePieceBitBoards = pieceBitBoards[(int)side];

    for (int pieceIdx = minimumAttackerIdx; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        const BitBoard pieceAttackers =
                intersection(controllingPieces, sidePieceBitBoards[pieceIdx]);
        if (pieceAttackers != BitBoard::Empty) {
            return {(Piece)pieceIdx, getFirstSetBitBoard(pieceAttackers)};
        }
    }

    return {Piece::Invalid, BitBoard::Empty};
}

}  // namespace

int staticExchangeEvaluationBound(const GameState& gameState, const Move& move, int threshold) {
    // TODO: consider promotions? Pins?

    const BoardPosition targetSquare = move.to;

    BitBoard anyPiece = any(gameState.getOccupancy().ownPiece, gameState.getOccupancy().enemyPiece);

    Piece targetPiece = getPiece(gameState.getPieceOnSquare(move.to));

    if (isEnPassant(move.flags)) {
        const auto [fromFile, fromRank] = fileRankFromPosition(move.from);
        const auto [toFile, toRank]     = fileRankFromPosition(move.to);
        BoardPosition captureTarget     = positionFromFileRank(toFile, fromRank);
        clear(anyPiece, captureTarget);
        set(anyPiece, targetSquare);

        targetPiece = Piece::Pawn;
    }

    std::array<int, kNumTotalPieces> gain;
    int exchangeIdx     = 0;
    gain[exchangeIdx++] = getPieceValue(targetPiece);

    if (gain[0] < threshold) {
        return gain[0];
    }

    const PieceBitBoards& pieceBitBoards = gameState.getPieceBitBoards();

    // Don't need to consider kings, because an exchagne sequence always ends once the king has been
    // captured
    const BitBoard piecesThatCanTakeAlongXray =
            any(gameState.getPieceBitBoard(Side::White, Piece::Pawn),
                gameState.getPieceBitBoard(Side::White, Piece::Bishop),
                gameState.getPieceBitBoard(Side::White, Piece::Rook),
                gameState.getPieceBitBoard(Side::White, Piece::Queen),
                gameState.getPieceBitBoard(Side::Black, Piece::Pawn),
                gameState.getPieceBitBoard(Side::Black, Piece::Bishop),
                gameState.getPieceBitBoard(Side::Black, Piece::Rook),
                gameState.getPieceBitBoard(Side::Black, Piece::Queen));

    std::array<int, kNumSides> minimumAttackerIdx = {0, 0};

    targetPiece            = move.pieceToMove;
    BitBoard vacatedSquare = BitBoard::Empty;
    set(vacatedSquare, move.from);

    BitBoard controllingPieces =
            getAllPiecesControllingSquare(targetSquare, pieceBitBoards, anyPiece);

    if (isEnPassant(move.flags)) {
        controllingPieces = intersection(controllingPieces, anyPiece);
    }

    const Side sideToMove = gameState.getSideToMove();
    for (;; ++exchangeIdx) {
        // Gain from the perspective of current side to move if the target piece is taken
        gain[exchangeIdx] = getPieceValue(targetPiece) - gain[exchangeIdx - 1];

        const Side side = (Side)((exchangeIdx + (int)sideToMove) & 1);

        // Deferred update of controllingPieces and anyPiece
        controllingPieces = subtract(controllingPieces, vacatedSquare);
        anyPiece          = subtract(anyPiece, vacatedSquare);

        if (intersection(piecesThatCanTakeAlongXray, vacatedSquare) != BitBoard::Empty) {
            // Re-calculate sliding piece attacks to account for x-rays
            const BitBoard controllingSlidingPieces =
                    getSlidingPiecesControllingSquare(targetSquare, anyPiece, pieceBitBoards);

            // We need to intersect with anyPiece so that we ignore sliding pieces that already
            // entered the exchange.
            controllingPieces =
                    any(controllingPieces, intersection(controllingSlidingPieces, anyPiece));

            minimumAttackerIdx[0] = min(minimumAttackerIdx[0], (int)Piece::Bishop);
            minimumAttackerIdx[1] = min(minimumAttackerIdx[1], (int)Piece::Bishop);
        }

        auto [attacker, attackerSquare] = getLeastValuableAttacker(
                controllingPieces, pieceBitBoards, minimumAttackerIdx[(int)side], side);

        if (attacker == Piece::Invalid) {
            // No more attackers left.
            break;
        }

        if (side == sideToMove) {
            if (gain[exchangeIdx] < threshold) {
                // Even after taking the target piece, we're still below the threshold.
                // If the exchange gets to this point it will never rise above the threshold again,
                // so we don't need to consider what happens after this point.
                ++exchangeIdx;
                break;
            }
        } else {
            if (-gain[exchangeIdx] >= threshold) {
                // Even after taking the target piece, the gain for the opponent is still above the
                // threshold. So if the exchange gets to this point it will never drop below the
                // threshold again, so we don't need to consider what happens after this point.
                ++exchangeIdx;
                break;
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
        gain[i - 1] = -max(-gain[i - 1], gain[i]);
    }

    return gain[0];
}