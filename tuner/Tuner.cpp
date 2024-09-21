#include "chess-engine-lib/Eval.h"
#include "chess-engine-lib/GameState.h"
#include "chess-engine-lib/Math.h"
#include "chess-engine-lib/MoveOrder.h"

#include "ceres/ceres.h"

#include <fstream>
#include <iostream>
#include <istream>
#include <memory>
#include <print>
#include <ranges>
#include <string>
#include <thread>
#include <vector>

#include <cstdlib>

namespace {

struct ScoredPosition {
    GameState gameState;
    double score;
};

struct EvalCostFunctor : ceres::SizedCostFunction<1, 1, kNumEvalParams> {
    EvalCostFunctor(
            const ScoredPosition& scoredPosition,
            const std::shared_ptr<std::vector<int>> constantParamIdxs)
        : scoredPosition_(scoredPosition), constantParamIdxs_(constantParamIdxs) {}

    bool Evaluate(
            double const* const* parameters, double* residuals, double** jacobians) const override {
        const double* const scaleParam   = parameters[0];
        const double* const paramsDouble = parameters[1];

        EvalParamArray paramsArray;
        for (int i = 0; i < paramsArray.size(); ++i) {
            paramsArray[i] = static_cast<float>(paramsDouble[i]);
        }

        const bool needParamJacobians = jacobians != nullptr && jacobians[1] != nullptr;

        const Evaluator evaluator(evalParamsFromArray(paramsArray));

        EvalWithGradient evalWithGradient;

        if (needParamJacobians) {
            evalWithGradient = evaluator.evaluateWithGradient(scoredPosition_.gameState);
        } else {
            evalWithGradient.eval = evaluator.evaluate(scoredPosition_.gameState);
        }

        const double sigmoid = 1. / (1. + std::pow(10., -evalWithGradient.eval / *scaleParam));

        residuals[0] = scoredPosition_.score - sigmoid;

        if (jacobians) {
            const double s = *scaleParam;
            const double x = evalWithGradient.eval;
            if (jacobians[0]) {
                const double power = std::pow(10, x / s);

                jacobians[0][0] = x * std::log(10.) * power / (s * s * (1 + power) * (1 + power));
            }

            if (jacobians[1]) {
                const double negPower = std::pow(10, -x / s);

                const double sigmoidDerivative =
                        std::log(10.) * negPower / (s * (1 + negPower) * (1 + negPower));

                for (int i = 0; i < kNumEvalParams; ++i) {
                    jacobians[1][i] = -sigmoidDerivative * evalWithGradient.gradient[i];
                }

                for (int i : *constantParamIdxs_) {
                    jacobians[1][i] = 0;
                }
            }
        }

        return true;
    }

  private:
    ScoredPosition scoredPosition_;
    std::shared_ptr<std::vector<int>> constantParamIdxs_;
};

void getScoredPositions(
        const std::string& annotatedFensPath,
        std::vector<ScoredPosition>& scoredPositions,
        const int dropoutRate) {
    std::srand(42);

    std::ifstream in(annotatedFensPath);

    const auto startingSize = scoredPositions.size();

    std::string inputLine;
    while (std::getline(in, inputLine)) {
        if (inputLine.empty()) {
            continue;
        }

        if ((std::rand() % dropoutRate) != 0) {
            continue;
        }

        std::stringstream lineSStream(inputLine);

        std::string token;
        lineSStream >> token;

        if (token != "score") {
            continue;
        }

        double score;
        lineSStream >> score;

        lineSStream >> token;
        if (token != "fen") {
            continue;
        }

        lineSStream >> std::ws;

        std::string fen;
        std::getline(lineSStream, fen);

        const GameState gameState = GameState::fromFen(fen);

        scoredPositions.push_back({gameState, score});
    }

    std::println(std::cerr, "Read {} scored positions", scoredPositions.size() - startingSize);
}

StackOfVectors<MoveEvalT> gMoveScoreStack = {};

bool isDraw(const GameState& gameState, StackOfVectors<Move>& stack) {
    if (gameState.isRepetition(/*repetitionThreshold =*/2)) {
        return true;
    }

    if (gameState.isFiftyMoves()) {
        const auto moves = gameState.generateMoves(stack);
        if (moves.size() == 0) {
            return evaluateNoLegalMoves(gameState);
        } else {
            return true;
        }
    }

    if (isInsufficientMaterial(gameState)) {
        return true;
    }

    return false;
}

void updateMateDistance(EvalT& score) {
    if (isMate(score)) {
        score = mateDistancePlus1(score);
    }
}

std::pair<EvalT, GameState> quiesce(
        GameState& gameState,
        EvalT alpha,
        EvalT beta,
        StackOfVectors<Move>& stack,
        const Evaluator& evaluator) {
    constexpr EvalT kDeltaPruningThreshold = 200;

    if (isDraw(gameState, stack)) {
        return {0, gameState};
    }

    const BitBoard enemyControl = gameState.getEnemyControl();
    const bool isInCheck        = gameState.isInCheck(enemyControl);

    EvalT standPat = -kInfiniteEval;
    if (!isInCheck) {
        // Stand pat
        standPat = evaluator.evaluate(gameState);
        if (standPat >= beta) {
            return {standPat, gameState};
        }

        alpha = max(alpha, standPat);
    }

    EvalT bestScore     = standPat;
    GameState bestState = gameState;

    auto moves = gameState.generateMoves(stack, enemyControl, /*capturesOnly =*/!isInCheck);
    if (moves.size() == 0) {
        if (isInCheck) {
            return {-kMateEval, gameState};
        }

        const auto allMoves = gameState.generateMoves(stack, enemyControl);
        if (allMoves.size() == 0) {
            // No legal moves, not in check, so stalemate.
            return {0, gameState};
        }

        return {bestScore, bestState};
    }

    auto moveScores = scoreMovesQuiesce(evaluator, moves, 0, gameState, gMoveScoreStack);

    for (int moveIdx = 0; moveIdx < moves.size(); ++moveIdx) {
        const Move move = selectBestMove(moves, moveScores, moveIdx);

        const auto unmakeInfo = gameState.makeMove(move);

        auto [score, state] = quiesce(gameState, -beta, -alpha, stack, evaluator);
        score               = -score;

        gameState.unmakeMove(move, unmakeInfo);

        updateMateDistance(score);

        alpha = max(alpha, score);
        if (score > bestScore) {
            bestScore = score;
            bestState = state;
        }

        if (alpha >= beta) {
            break;
        }
    }

    return {bestScore, bestState};
}

void quiescePositions(std::vector<ScoredPosition>& scoredPositions) {
    const Evaluator evaluator(EvalParams::getDefaultParams());
    StackOfVectors<Move> moveStack;

    std::vector<ScoredPosition> quiescedPositions;
    for (ScoredPosition& scoredPosition : scoredPositions) {
        const EvalT evalThreshold = 500;

        const EvalT baseEval = evaluator.evaluate(scoredPosition.gameState);
        if (std::abs(baseEval) >= evalThreshold) {
            continue;
        }

        const EvalT deltaThreshold = 50;
        const EvalT alpha          = baseEval - deltaThreshold - 1;
        const EvalT beta           = baseEval + deltaThreshold + 1;

        auto [score, state] = quiesce(scoredPosition.gameState, alpha, beta, moveStack, evaluator);

        const EvalT evalDelta = std::abs(baseEval - score);
        if (evalDelta >= deltaThreshold || std::abs(score) >= evalThreshold) {
            continue;
        }

        double scoreToUse = scoredPosition.score;
        if (state.getSideToMove() != scoredPosition.gameState.getSideToMove()) {
            scoreToUse = 1 - scoreToUse;
        }

        quiescedPositions.emplace_back(state, scoreToUse);
    }

    std::println("Obtained {} quiesced positions", quiescedPositions.size());

    std::swap(scoredPositions, quiescedPositions);
}

std::array<double, kNumEvalParams> getInitialParams() {
    std::array<double, kNumEvalParams> paramsDouble;
    const EvalParamArray params = evalParamsToArray(EvalParams::getDefaultParams());
    for (int i = 0; i < paramsDouble.size(); ++i) {
        paramsDouble[i] = static_cast<double>(params[i]);
    }

    return paramsDouble;
}

std::shared_ptr<std::vector<int>> getConstantParamIdxs() {
    std::shared_ptr<std::vector<int>> constantParamIdxs = std::make_shared<std::vector<int>>();

    EvalParams params = EvalParams::getDefaultParams();
    const auto getIdx = [&](const EvalCalcT& member) {
        return (int)((std::byte*)&member - (std::byte*)&params) / sizeof(EvalCalcT);
    };
    const auto setConstant = [&](const EvalCalcT& member) {
        constantParamIdxs->push_back(getIdx(member));
    };

    //// Fix one of the phase material values to fix the scale of the phase material values.
    //// (Otherwise it's a gauge freedom.)
    //setConstant(params.phaseMaterialValues[(int)Piece::Knight]);

    //// We need to fix 1 square in the piece-square tables to avoid a gauge freedom with the piece
    //// values.
    //const int constSquareIdx = (int)positionFromFileRank(3, 3);
    //for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
    //    setConstant(params.pieceSquareTablesWhiteEarly[pieceIdx][constSquareIdx]);
    //}

    // Fix piece values to avoid gauge freedoms with the piece-square tables.
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        setConstant(params.pieceValues[pieceIdx]);
    }

    // A pawn 1 square away from promotion is always a passed pawn, so this term has a gauge
    // freedom with the piece-square tables.
    setConstant(params.passedPawnBonus[1]);

    // Fix one value in the adjustment tables to avoid gauge freedoms with the piece values.
    setConstant(params.badBishopPenalty[4]);
    setConstant(params.knightPawnAdjustment[5]);
    setConstant(params.rookPawnAdjustment[5]);

    // Fix phase material values because of poor convergence otherwise
    setConstant(params.phaseMaterialValues[(int)Piece::Pawn]);
    setConstant(params.phaseMaterialValues[(int)Piece::Knight]);
    setConstant(params.phaseMaterialValues[(int)Piece::Bishop]);
    setConstant(params.phaseMaterialValues[(int)Piece::Rook]);
    setConstant(params.phaseMaterialValues[(int)Piece::Queen]);
    setConstant(params.phaseMaterialValues[(int)Piece::King]);

    // Fix unused values

    // Kings are always on the board, so their piece and phase material values are unused.
    setConstant(params.pieceValues[(int)Piece::King]);
    setConstant(params.phaseMaterialValues[(int)Piece::King]);

    // Pawns are never on the 1st or 8th ranks, so their piece-square tables for those ranks are
    // unused.
    for (int file = 0; file < kFiles; ++file) {
        const int pawnIdx       = (int)Piece::Pawn;
        const int rank1Position = (int)positionFromFileRank(file, 0);
        const int rank8Position = (int)positionFromFileRank(file, kRanks - 1);

        setConstant(params.pieceSquareTablesWhiteEarly[pawnIdx][rank1Position]);
        setConstant(params.pieceSquareTablesWhiteEarly[pawnIdx][rank8Position]);

        setConstant(params.pieceSquareTablesWhiteLate[pawnIdx][rank1Position]);
        setConstant(params.pieceSquareTablesWhiteLate[pawnIdx][rank8Position]);
    }

    // Pawns are never on the 8th rank, so the passed pawn bonus there is unused.
    setConstant(params.passedPawnBonus[0]);

    // We don't calculate mobility for pawns or kings.
    setConstant(params.mobilityBonusEarly[(int)Piece::Pawn]);
    setConstant(params.mobilityBonusEarly[(int)Piece::King]);
    setConstant(params.mobilityBonusLate[(int)Piece::Pawn]);
    setConstant(params.mobilityBonusLate[(int)Piece::King]);

    return constantParamIdxs;
}

void addResiduals(
        double& scaleParam,
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions,
        ceres::Problem& problem) {
    problem.AddParameterBlock(&scaleParam, 1);
    //problem.AddParameterBlock(paramsDouble.data(), kNumEvalParams, getParamsManifold());
    problem.AddParameterBlock(paramsDouble.data(), kNumEvalParams);

    const auto constantParamIdxs = getConstantParamIdxs();

    for (const auto& scoredPosition : scoredPositions) {
        ceres::CostFunction* costFunction = new EvalCostFunctor(scoredPosition, constantParamIdxs);
        problem.AddResidualBlock(costFunction, nullptr, &scaleParam, paramsDouble.data());
    }
}

void solve(ceres::Problem& problem) {

    ceres::Solver::Options options;
    options.minimizer_progress_to_stdout      = true;
    options.num_threads                       = std::thread::hardware_concurrency();
    options.parameter_tolerance               = 1e-3;
    options.initial_trust_region_radius       = 1e4;
    options.dense_linear_algebra_library_type = ceres::CUDA;
    options.use_mixed_precision_solves        = true;
    options.max_num_refinement_iterations     = 3;

    if (problem.NumResidualBlocks() > 2e5) {
        options.linear_solver_type = ceres::DENSE_NORMAL_CHOLESKY;
    } else {
        options.linear_solver_type = ceres::DENSE_QR;
    }

    ceres::Solver::Summary summary;
    ceres::Solve(options, &problem, &summary);
    std::cout << summary.FullReport() << "\n";
}

}  // namespace

int main(int argc, char** argv) {
    const std::vector<std::string> annotatedFensPaths = {
            R"(D:\annotated-fens\since_virtual_king_mobility_to_tune_old.txt)",
            R"(D:\annotated-fens\first-tuning-rounds.txt)"};

    const std::vector<int> dropoutRates = {4, 1};

    double scaleParam                               = 400.;
    std::array<double, kNumEvalParams> paramsDouble = getInitialParams();

    ceres::Problem problem;

    {
        std::vector<ScoredPosition> scoredPositions;
        for (int i = 0; i < annotatedFensPaths.size(); ++i) {
            getScoredPositions(annotatedFensPaths.at(i), scoredPositions, dropoutRates.at(i));
        }

        quiescePositions(scoredPositions);
        addResiduals(scaleParam, paramsDouble, scoredPositions, problem);
    }

    problem.SetParameterBlockConstant(paramsDouble.data());
    solve(problem);

    std::println("Scale param: {}", scaleParam);

    problem.SetParameterBlockVariable(paramsDouble.data());
    problem.SetParameterBlockConstant(&scaleParam);
    solve(problem);

    const std::string paramsString =
            paramsDouble | std::ranges::views::transform([](double d) { return std::to_string(d); })
            | std::ranges::views::join_with(std ::string(", ")) | std::ranges::to<std::string>();

    std::println("Params: {}", paramsString);
}
