#include "chess-engine-lib/GameState.h"

#include "MyGTest.h"

#include <unordered_map>

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

class HashingTests : public ::testing::TestWithParam<HashCollisionTestConfig> {};

TEST_P(HashingTests, FindHashCollisions) {
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

INSTANTIATE_TEST_CASE_P(HashingTests, HashingTests, testCases, hashTestName);

}  // namespace HashingTests