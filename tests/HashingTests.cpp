#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

#include <unordered_map>
#include <unordered_set>

namespace HashingTests {

std::unordered_map<HashT, std::string> gHashToFen;

void findHashCollisions(GameState& gameState, const int depth, StackOfVectors<Move>& stack) {
    const HashT hash      = gameState.getBoardHash();
    const std::string fen = gameState.toFenNoMoveCounters();

    if (gHashToFen.contains(hash)) {
        EXPECT_EQ(gHashToFen[hash], fen);
        return;
    } else {
        gHashToFen.emplace(hash, fen);
    }

    if (depth == 0) {
        return;
    }

    const StackVector<Move> moves = gameState.generateMoves(stack);

    for (const Move move : moves) {
        const auto unmakeInfo = gameState.makeMove(move);
        findHashCollisions(gameState, depth - 1, stack);
        gameState.unmakeMove(move, unmakeInfo);
    }
}

struct HashCollisionTestConfig {
    std::string fen;
    int depth;
};

class HashCollisionTests : public ::testing::TestWithParam<HashCollisionTestConfig> {};

TEST_P(HashCollisionTests, FindHashCollisions) {
    const HashCollisionTestConfig config = GetParam();

    GameState gameState = GameState::fromFen(config.fen);
    StackOfVectors<Move> stack;
    findHashCollisions(gameState, config.depth, stack);
}

inline const std::string kKiwipeteFen =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
inline const std::string kPosition3Fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
inline const std::string kPosition4Fen =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
inline const std::string kPosition5Fen =
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
inline const std::string kPosition6Fen =
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";

std::string hashTestName(const ::testing::TestParamInfo<HashCollisionTestConfig>& info) {
    std::string fenName = "";
    if (info.param.fen == getStartingPositionFen()) {
        fenName = "root";
    } else if (info.param.fen == kKiwipeteFen) {
        fenName = "kiwipete";
    } else if (info.param.fen == kPosition3Fen) {
        fenName = "position3";
    } else if (info.param.fen == kPosition4Fen) {
        fenName = "position4";
    } else if (info.param.fen == kPosition5Fen) {
        fenName = "position5";
    } else if (info.param.fen == kPosition6Fen) {
        fenName = "position6";
    }

    return fenName + "_depth" + std::to_string(info.param.depth);
}

auto testCases = ::testing::Values(
        HashCollisionTestConfig{.fen = getStartingPositionFen(), .depth = 4},
        HashCollisionTestConfig{.fen = kKiwipeteFen, .depth = 3},
        HashCollisionTestConfig{.fen = kPosition3Fen, .depth = 5},
        HashCollisionTestConfig{.fen = kPosition4Fen, .depth = 4},
        HashCollisionTestConfig{.fen = kPosition5Fen, .depth = 3},
        HashCollisionTestConfig{.fen = kPosition6Fen, .depth = 3});

INSTANTIATE_TEST_CASE_P(HashCollisionTests, HashCollisionTests, testCases, hashTestName);

TEST(HashingTests, NullMoveEnPassantHashing) {
    const HashT withEnPassantTarget =
            GameState::fromFen("8/8/2n1k3/8/1pPp1BK1/pP1P4/P7/8 b - c3 0 1").getBoardHash();
    const HashT withoutEnPassantTarget =
            GameState::fromFen("8/8/2n1k3/8/1pPp1BK1/pP1P4/P7/8 b - - 0 1").getBoardHash();

    EXPECT_NE(withEnPassantTarget, withoutEnPassantTarget);

    GameState gameState = GameState::fromFen("8/5k2/2n1R3/6K1/1p1p1B2/pP1P4/P1P5/8 b - - 0 1");

    const HashT startHash = gameState.getBoardHash();

    const Move movef7e6       = Move::fromUci("f7e6", gameState);
    const auto movef7e6Unmake = gameState.makeMove(movef7e6);
    const HashT afterf7e6Hash = gameState.getBoardHash();

    const Move doublePush{
            .pieceToMove = Piece::Pawn, .from = BoardPosition::C2, .to = BoardPosition::C4};
    const auto gameStateUnmake      = gameState.makeMove(doublePush);
    const HashT afterDoublePushHash = gameState.getBoardHash();

    const auto nullMoveUnmake     = gameState.makeNullMove();
    const HashT afterNullMoveHash = gameState.getBoardHash();

    const Move moveg5g4       = Move::fromUci("g5g4", gameState);
    const auto moveg5gfUnmake = gameState.makeMove(moveg5g4);
    const HashT afterg5g4Hash = gameState.getBoardHash();

    EXPECT_EQ(afterg5g4Hash, withoutEnPassantTarget);

    gameState.unmakeMove(moveg5g4, moveg5gfUnmake);
    const HashT afterUnmakingg5g4Hash = gameState.getBoardHash();
    EXPECT_EQ(afterUnmakingg5g4Hash, afterNullMoveHash);

    gameState.unmakeNullMove(nullMoveUnmake);
    const HashT afterUnmakingNullMoveHash = gameState.getBoardHash();
    EXPECT_EQ(afterUnmakingNullMoveHash, afterDoublePushHash);

    gameState.unmakeMove(doublePush, gameStateUnmake);
    const HashT afterUnmakingDoublePushHash = gameState.getBoardHash();
    EXPECT_EQ(afterUnmakingDoublePushHash, afterf7e6Hash);

    gameState.unmakeMove(movef7e6, movef7e6Unmake);
    const HashT afterUnmakingf7e6Hash = gameState.getBoardHash();
    EXPECT_EQ(afterUnmakingf7e6Hash, startHash);

    std::set<HashT> hashes = {
            startHash, afterf7e6Hash, afterDoublePushHash, afterNullMoveHash, afterg5g4Hash};
    EXPECT_EQ(hashes.size(), 5);
}

}  // namespace HashingTests