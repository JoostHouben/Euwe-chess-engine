#include "chess-engine-lib/Eval.h"
#include "chess-engine-lib/GameState.h"

#include "ceres/ceres.h"

#include <fstream>
#include <iostream>
#include <istream>
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

static constexpr std::size_t kNumParams = std::tuple_size_v<EvalParamArray>;

struct EvalCostFunctor : ceres::SizedCostFunction<1, 1, kNumParams> {
    EvalCostFunctor(const ScoredPosition& scoredPosition) : scoredPosition_(scoredPosition) {}

    double calculateResidual(const double scale, const EvalParamArray& paramsArray) const {
        const Evaluator evaluator(evalParamsFromArray(paramsArray));

        const EvalCalcT rawEval = evaluator.evaluateRaw(scoredPosition_.gameState);

        const double sigmoid = 1. / (1. + std::pow(10., -rawEval / scale));

        return scoredPosition_.score - sigmoid;
    }

    bool Evaluate(
            double const* const* parameters, double* residuals, double** jacobians) const override {
        const double* const scaleParam   = parameters[0];
        const double* const paramsDouble = parameters[1];

        EvalParamArray paramsArray;
        for (int i = 0; i < paramsArray.size(); ++i) {
            paramsArray[i] = static_cast<float>(paramsDouble[i]);
        }

        residuals[0] = calculateResidual(*scaleParam, paramsArray);

        if (jacobians) {
            if (jacobians[0]) {
                static constexpr double kScaleDelta = 1.;

                const double residualPrime =
                        calculateResidual(*scaleParam + kScaleDelta, paramsArray);

                jacobians[0][0] = (residualPrime - residuals[0]) / kScaleDelta;
            }

            if (jacobians[1]) {
                for (int i = 0; i < paramsArray.size(); ++i) {
                    const double paramDelta = std::max(0.1, std::abs(paramsArray[i]) / 10.);

                    const double paramBackup = paramsArray[i];
                    paramsArray[i] += paramDelta;

                    const double residualPrime = calculateResidual(*scaleParam, paramsArray);

                    jacobians[1][i] = (residualPrime - residuals[0]) / paramDelta;

                    paramsArray[i] = paramBackup;
                }
            }
        }

        return true;
    }

  private:
    ScoredPosition scoredPosition_;
};

std::vector<ScoredPosition> getScoredPositions(std::string annotatedFensPath) {
    std::srand(42);

    std::ifstream in(annotatedFensPath);

    std::vector<ScoredPosition> scoredPositions;

    std::string inputLine;
    while (std::getline(in, inputLine)) {
        if (inputLine.empty()) {
            continue;
        }

        if ((std::rand() % 30) != 0) {
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

    std::println(std::cerr, "Read {} scored positions", scoredPositions.size());

    return scoredPositions;
}

std::array<double, kNumParams> getInitialParams() {
    std::array<double, kNumParams> paramsDouble;
    const EvalParamArray params = evalParamsToArray(EvalParams::getDefaultParams());
    for (int i = 0; i < paramsDouble.size(); ++i) {
        paramsDouble[i] = static_cast<double>(params[i]);
    }

    return paramsDouble;
}

ceres::SubsetManifold* getParamsManifold() {
    std::vector<int> constantParamIdxs;

    EvalParams params = EvalParams::getDefaultParams();
    const auto getIdx = [&](const EvalCalcT& member) {
        return (int)((std::byte*)&member - (std::byte*)&params) / sizeof(EvalCalcT);
    };
    const auto setConstant = [&](const EvalCalcT& member) {
        constantParamIdxs.push_back(getIdx(member));
    };

    setConstant(params.pieceValues[(int)Piece::King]);

    setConstant(params.phaseMaterialValues[(int)Piece::Knight]);
    setConstant(params.phaseMaterialValues[(int)Piece::King]);

    const int constSquareIdx = (int)positionFromFileRank(3, 3);
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        setConstant(params.pieceSquareTablesWhiteEarly[pieceIdx][0]);
        setConstant(params.pieceSquareTablesWhiteLate[pieceIdx][0]);
    }

    setConstant(params.passedPawnBonus[0]);

    setConstant(params.badBishopPenalty[4]);

    setConstant(params.knightPawnAdjustment[5]);
    setConstant(params.rookPawnAdjustment[5]);

    setConstant(params.mobilityBonusEarly[(int)Piece::Pawn]);
    setConstant(params.mobilityBonusLate[(int)Piece::Pawn]);

    return new ceres::SubsetManifold(kNumParams, constantParamIdxs);
}

void addResiduals(
        double& scaleParam,
        std::array<double, kNumParams>& paramsDouble,
        std::vector<ScoredPosition> scoredPositions,
        ceres::Problem& problem) {
    problem.AddParameterBlock(&scaleParam, 1);
    problem.AddParameterBlock(paramsDouble.data(), kNumParams, getParamsManifold());

    for (const auto& scoredPosition : scoredPositions) {
        ceres::CostFunction* costFunction = new EvalCostFunctor(scoredPosition);
        problem.AddResidualBlock(costFunction, nullptr, &scaleParam, paramsDouble.data());
    }
}

void solve(ceres::Problem& problem) {
    ceres::Solver::Options options;
    options.minimizer_progress_to_stdout = true;
    options.num_threads                  = std::thread::hardware_concurrency();
    options.parameter_tolerance          = 1e-3;
    options.initial_trust_region_radius  = 1e0;
    ceres::Solver::Summary summary;
    ceres::Solve(options, &problem, &summary);
    std::cout << summary.FullReport() << "\n";
}

}  // namespace

int main(int argc, char** argv) {
    const std::string annotatedFensPath =
            R"(D:\annotated-fens\since_search_state_to_virtual_king_mobility.txt)";

    double scaleParam                           = 400.;
    std::array<double, kNumParams> paramsDouble = getInitialParams();

    ceres::Problem problem;

    {
        std::vector<ScoredPosition> scoredPositions = getScoredPositions(annotatedFensPath);
        addResiduals(scaleParam, paramsDouble, std::move(scoredPositions), problem);
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
