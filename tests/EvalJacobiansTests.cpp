#include "chess-engine-lib/Eval.h"
#include "chess-engine-lib/Math.h"

#include "MyGTest.h"

namespace EvalJacobiansTests {

namespace {

double numericDerivative(
        const GameState& gameState,
        const EvalParamArray& x,
        const std::size_t i,
        const double eps) {
    EvalCalcT yPlus;
    {
        EvalParamArray xPrime = x;
        xPrime[i] += eps;

        const Evaluator evaluator(evalParamsFromArray(xPrime));
        yPlus = evaluator.evaluateRaw(gameState);
    }

    EvalCalcT yMinus;
    {
        EvalParamArray xPrime = x;
        xPrime[i] -= eps;

        const Evaluator evaluator(evalParamsFromArray(xPrime));
        yMinus = evaluator.evaluateRaw(gameState);
    }

    return (yPlus - yMinus) / (2 * eps);
}

}  // namespace

TEST(EvalJacobiansTests, TestJacobians) {
    std::vector<GameState> gameStates = {
            // kiwipete
            GameState::fromFen(
                    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"),
            // position 3
            GameState::fromFen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"),
            // position 4
            GameState::fromFen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")};

    const EvalParams defaultParams         = EvalParams::getDefaultParams();
    const EvalParamArray defaultParamArray = evalParamsToArray(defaultParams);

    for (const GameState& gameState : gameStates) {
        const EvalWithGradient evalWithGradient =
                Evaluator(defaultParams).evaluateWithGradient(gameState);

        for (std::size_t paramIdx = 0; paramIdx < kNumEvalParams; ++paramIdx) {
            const EvalCalcT defaultValue = defaultParamArray[paramIdx];
            const double epsilon         = max(1e-3, 1e-3 * std::abs(defaultValue));
            const double numericDeriv =
                    numericDerivative(gameState, defaultParamArray, paramIdx, epsilon);

            const double symbolicDeriv = evalWithGradient.gradient[paramIdx];

            EXPECT_NEAR(numericDeriv, symbolicDeriv, 1e-1)
                    << paramIdx << " fen: " << gameState.toFen();
        }
    }
}

}  // namespace EvalJacobiansTests
