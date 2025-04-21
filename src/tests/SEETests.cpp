#include "chess-engine-lib/SEE.h"

#include "chess-engine-lib/Eval.h"

#include "MyGTest.h"

#include <ostream>

namespace SEETests {

struct SEETestConfig {
    std ::string name;
    std::string fen;
    Move move;
    int expectedScore;

    friend std::ostream& operator<<(std::ostream& os, const SEETestConfig& config) {
        os << config.name;
        return os;
    }
};

std::string getTestName(const ::testing::TestParamInfo<SEETestConfig>& info) {
    return info.param.name;
}

class SEETests : public ::testing::TestWithParam<SEETestConfig> {};

void testSee(const GameState& gameState, const Move& move, const int expectedScore) {
    EXPECT_EQ(expectedScore, staticExchangeEvaluation(gameState, move));

    for (int threshold = -1000; threshold <= expectedScore; ++threshold) {
        // seeBound should be a lower bound l such that s >= l >= t.
        const int seeBound = staticExchangeEvaluationBound(gameState, move, threshold);
        EXPECT_GE(expectedScore, seeBound);
        EXPECT_GE(seeBound, threshold);

        const bool meetsBound = staticExchangeEvaluationMeetsBound(gameState, move, threshold);
        EXPECT_EQ(expectedScore >= threshold, meetsBound);
    }

    for (int threshold = expectedScore + 1; threshold <= 1000; ++threshold) {
        // seeBound should be an upper bound u such that s <= u < t.
        const int seeBound = staticExchangeEvaluationBound(gameState, move, threshold);
        EXPECT_LE(expectedScore, seeBound);
        EXPECT_LT(seeBound, threshold);

        const bool meetsBound = staticExchangeEvaluationMeetsBound(gameState, move, threshold);
        EXPECT_EQ(expectedScore >= threshold, meetsBound);
    }
}

TEST_P(SEETests, TestStaticExchangeEvaluation) {
    const SEETestConfig config = GetParam();

    const GameState gameState = GameState::fromFen(config.fen);

    testSee(gameState, config.move, config.expectedScore);
}

TEST_P(SEETests, TestStaticExchangeEvaluationWithoutVictim) {
    const SEETestConfig config = GetParam();

    if (config.move.pieceToMove == Piece::Pawn || !isCapture(config.move)) {
        // Removing the victim to create a normal move only works for non-pawn captures.
        return;
    }

    GameState gameState = GameState::fromFen(config.fen);

    const Piece victim             = getPiece(gameState.getPieceOnSquare(config.move.to));
    int expectedScoreWithoutVictim = config.expectedScore - getStaticPieceValue(victim);
    gameState.removePiece(config.move.to);

    const MoveFlags flagsNonCapture =
            (MoveFlags)((std::uint8_t)config.move.flags & ~(std::uint8_t)MoveFlags::IsCapture);
    const Move moveNonCapture{
            .pieceToMove = config.move.pieceToMove,
            .from        = config.move.from,
            .to          = config.move.to,
            .flags       = flagsNonCapture};

    testSee(gameState, moveNonCapture, expectedScoreWithoutVictim);
}

const auto testCases = ::testing::Values(
        SEETestConfig{
                .name = "knightWinsPawnAfterRecapture",
                .fen  = "k7/8/5n2/3p4/8/2N2B2/8/K7 w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Knight,
                             .from        = BoardPosition::C3,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "discoveredRookAttacks",
                .fen  = "2K5/8/8/3pRrRr/8/8/8/2k5 w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::E5,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Rook)},
        SEETestConfig{
                .name = "bishopCantTakeLikeARook",
                .fen  = "2K5/8/8/3pRbRr/8/8/8/2k5 w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::E5,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "discoveredBishopAttacks",
                .fen  = "2K5/8/8/3p4/4B3/5b2/6B1/2k4b w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Bishop,
                             .from        = BoardPosition::E4,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Bishop)},
        SEETestConfig{
                .name = "discoveredQueenAttacksStraight",
                .fen  = "2K5/8/8/3pQqQq/8/8/8/2k5 w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Queen,
                             .from        = BoardPosition::E5,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Queen)},
        SEETestConfig{
                .name = "discoveredQueenAttacksDiagonal",
                .fen  = "2K5/8/8/3p4/4Q3/5q2/6Q1/2k4q w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Queen,
                             .from        = BoardPosition::E4,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Queen)},
        // RxP, BxR, RxB, but then black doesn't play ... QxR, RxQ
        // Gain for black is pawn + bishop - rook
        SEETestConfig{
                .name = "queenDoesntTakeDefendedRook",
                .fen  = "2K5/8/2b1q3/3p4/8/3R4/3R4/2kR4 w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::D3,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)
                               + getStaticPieceValue(Piece::Bishop)
                               - getStaticPieceValue(Piece::Rook)},
        // Same as above, but sides are flipped
        SEETestConfig{
                .name = "queenDoesntTakeDefendedRookBlackToMove",
                .fen  = "2k5/8/2B1Q3/3P4/8/3r4/3r4/2Kr4 b - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::D3,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)
                               + getStaticPieceValue(Piece::Bishop)
                               - getStaticPieceValue(Piece::Rook)},
        SEETestConfig{
                .name = "kingCanDefend",
                .fen  = "2k5/8/4K3/3P4/8/3r4/8/8 b - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::D3,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Rook)},
        SEETestConfig{
                .name = "blackKingCanDefend",
                .fen  = "2K5/8/4k3/3p4/8/3R4/8/8 w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::D3,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Rook)},
        SEETestConfig{
                .name = "kingEnPriseIsDetected",
                .fen  = "2k5/8/4K3/3P4/8/3r4/3r4/8 b - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::D3,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "pawnCanDefendBlack",
                .fen  = "2K5/8/4p3/3p4/8/3R4/8/8 w - - 0 1",
                .move =
                        Move{.pieceToMove = Piece::Rook,
                             .from        = BoardPosition::D3,
                             .to          = BoardPosition::D5,
                             .flags       = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Rook)},
        SEETestConfig{
                .name = "pawnCannotDefendBackwardsBlack",
                .fen  = "2K5/8/8/3p4/4p3/3R4/8/8 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Rook, .from = BoardPosition::D3, .to = BoardPosition::D5, .flags = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "pawnCanDefendWhite",
                .fen  = "2K5/8/8/3P4/4P3/3r4/8/2k5 b - - 0 1",
                .move = Move{.pieceToMove = Piece::Rook, .from = BoardPosition::D3, .to = BoardPosition::D5, .flags = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Rook)},
        SEETestConfig{
                .name = "pawnCannotDefendBackwardsWhite",
                .fen  = "2K5/8/4P3/3P4/8/3r4/8/2k5 b - - 0 1",
                .move = Move{.pieceToMove = Piece::Rook, .from = BoardPosition::D3, .to = BoardPosition::D5, .flags = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "pawnIsConsideredForXray",
                .fen  = "2K5/8/2p1p3/3P4/4P3/5B2/8/2k5 b - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::C6, .to = BoardPosition::D5, .flags = MoveFlags::IsCapture},
                .expectedScore = 0},
        SEETestConfig{
                .name = "enPassant",
                .fen  = "2k5/8/8/8/3pP3/8/8/2K5 b - e3 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::D4, .to = BoardPosition::E3, .flags = MoveFlags::IsCapture | MoveFlags::IsEnPassant},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "enPassantDiscovered",
                .fen  = "2k5/8/4R3/8/3pP3/8/8/2K5 b - e3 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::D4, .to = BoardPosition::E3, .flags = MoveFlags::IsCapture | MoveFlags::IsEnPassant},
                .expectedScore = 0},
        SEETestConfig{
                .name = "enPassantDiscoveryBlocked",
                .fen  = "2k5/8/4R3/4P3/3pP3/8/8/2K5 b - e3 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::D4, .to = BoardPosition::E3, .flags = MoveFlags::IsCapture | MoveFlags::IsEnPassant},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "somethingWithAKnight",
                .fen  = "2K5/8/1n2q3/3p4/8/3R4/3R4/2kR4 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Rook, .from = BoardPosition::D3, .to = BoardPosition::D5, .flags = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)
                               + getStaticPieceValue(Piece::Knight)
                               - getStaticPieceValue(Piece::Rook)},
        // https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm#Position_1
        SEETestConfig{
                .name = "cpwSeeSwapPosition1",
                .fen  = "1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Rook, .from = BoardPosition::E1, .to = BoardPosition::E5, .flags = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        // https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm#Position_2
        SEETestConfig{
                .name = "cpwSeeSwapPosition2",
                .fen  = "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Knight, .from = BoardPosition::D3, .to = BoardPosition::E5, .flags = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Knight)},
        SEETestConfig{
                .name = "perftPosition4Continuation",
                .fen  = "r3k2r/Pppp1ppp/1b3nbN/nPP5/BB2P3/q4N2/Pp1P2PP/R2Q1RK1 b kq "
                        "- 0 1",
                .move = Move{.pieceToMove = Piece::Bishop, .from = BoardPosition::B6, .to = BoardPosition::C5, .flags = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "perftPosition4ContinuationLosingQueenCapture",
                .fen  = "r3k2r/Pppp1ppp/1b3nbN/nPP5/BB2P3/q4N2/Pp1P2PP/R2Q1RK1 b kq "
                        "- 0 1",
                .move = Move{.pieceToMove = Piece::Queen, .from = BoardPosition::A3, .to = BoardPosition::F3, .flags = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Knight) - getStaticPieceValue(Piece::Queen)},
        SEETestConfig{
                .name = "queenPromo",
                .fen  = "2K3n1/5P2/8/8/8/8/8/2k5 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::F7, .to = BoardPosition::F8, .flags = MoveFlags::None | Piece::Queen},
                .expectedScore =
                        getStaticPieceValue(Piece::Queen) - getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "queenPromoCaptureKnight",
                .fen  = "2K3n1/5P2/8/8/8/8/8/2k5 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::F7, .to = BoardPosition::G8, .flags = MoveFlags::IsCapture | Piece::Queen},
                .expectedScore = getStaticPieceValue(Piece::Knight)
                               + getStaticPieceValue(Piece::Queen)
                               - getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "defendedQueenPromo",
                .fen  = "2K4r/5P2/8/8/8/8/8/2k5 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::F7, .to = BoardPosition::F8, .flags = MoveFlags::None | Piece::Queen},
                .expectedScore = -getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "defendedQueenPromoCaptureKnight",
                .fen  = "2K3nr/5P2/8/8/8/8/8/2k5 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::F7, .to = BoardPosition::G8, .flags = MoveFlags::IsCapture | Piece::Queen},
                .expectedScore =
                        getStaticPieceValue(Piece::Knight) - getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "doublePromo",
                .fen  = "4n2r/3P1P2/8/8/8/8/8/2k2K2 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::D7, .to = BoardPosition::E8, .flags = MoveFlags::IsCapture | Piece::Queen},
                .expectedScore = getStaticPieceValue(Piece::Knight)
                               + getStaticPieceValue(Piece::Queen)
                               - getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "defendedByPromotingPawn",
                .fen  = "Q3n2r/5P2/8/8/8/8/8/2k2K2 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Queen, .from = BoardPosition::A8, .to = BoardPosition::E8, .flags = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Knight)},
        // From https://talkchess.com/viewtopic.php?t=77787
        SEETestConfig{
                .name = "forumPromotion",
                .fen  = "2r5/1P4pk/p2p1b1p/5b1n/BB3p2/2R2p2/P1P2P2/4RK2 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Rook, .from = BoardPosition::C3, .to = BoardPosition::C8, .flags = MoveFlags::IsCapture},
                .expectedScore = getStaticPieceValue(Piece::Rook)},
        // From https://talkchess.com/viewtopic.php?p=900686#p900686
        SEETestConfig{
                .name = "forumPromotion2",
                .fen  = "2nN3r/2P2ppk/5b1p/2B5/8/7P/3R2PK/8 b - - 0 1",
                .move = Move{.pieceToMove = Piece::Bishop, .from = BoardPosition::F6, .to = BoardPosition::D8, .flags = MoveFlags::IsCapture},
                .expectedScore =
                        getStaticPieceValue(Piece::Knight) - getStaticPieceValue(Piece::Bishop)
                        + getStaticPieceValue(Piece::Pawn) - getStaticPieceValue(Piece::Rook)},
        SEETestConfig{
                .name = "pawnPush",
                .fen  = "8/7r/8/8/8/5P2/8/2k2K2 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::F3, .to = BoardPosition::F4, .flags = MoveFlags::None},
                .expectedScore = 0},
        SEETestConfig{
                .name = "defendedPawnPush",
                .fen  = "8/8/8/8/7r/5P2/8/2k2K2 w - - 0 1",
                .move = Move{.pieceToMove = Piece::Pawn, .from = BoardPosition::F3, .to = BoardPosition::F4, .flags = MoveFlags::None},
                .expectedScore = -getStaticPieceValue(Piece::Pawn)},
        SEETestConfig{
                .name = "castle",
                .fen  = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1",
                .move = Move{.pieceToMove = Piece::King, .from = BoardPosition::E1, .to = BoardPosition::G1, .flags = MoveFlags::IsCastle},
                .expectedScore = 0});

INSTANTIATE_TEST_SUITE_P(SEETests, SEETests, testCases, getTestName);

}  // namespace SEETests
