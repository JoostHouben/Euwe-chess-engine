#include "Optimization.h"

#include "Utilities.h"

#include "chess-engine-lib/Eval.h"

#include "ceres/ceres.h"

#include <print>

namespace {

struct EvalCostFunctor : ceres::SizedCostFunction<1, 1, kNumEvalParams> {
    EvalCostFunctor(
            const ScoredPosition& scoredPosition,
            const std::shared_ptr<std::vector<int>> constantParamIdxs)
        : scoredPosition_(scoredPosition), constantParamIdxs_(constantParamIdxs) {}

    bool Evaluate(
            double const* const* parameters, double* residuals, double** jacobians) const override {
        const double* const scaleParam   = parameters[0];
        const double* const paramsDouble = parameters[1];

        const bool needParamJacobians = jacobians != nullptr && jacobians[1] != nullptr;

        const Evaluator evaluator(evalParamsFromDoubles(paramsDouble));

        EvalWithGradient evalWithGradient;

        if (needParamJacobians) {
            evalWithGradient = evaluator.evaluateWithGradient(scoredPosition_.gameState);
        } else {
            evalWithGradient.eval = evaluator.evaluateRaw(scoredPosition_.gameState);
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

std::vector<int> getConstantParamIdxs(bool fixPhaseValues) {
    std::vector<int> constantParamIdxs;

    EvalParams params = EvalParams::getDefaultParams();
    const auto getIdx = [&](const EvalCalcT& member) {
        return (int)((std::byte*)&member - (std::byte*)&params) / sizeof(EvalCalcT);
    };
    const auto setConstant = [&](const EvalCalcT& member) {
        constantParamIdxs.push_back(getIdx(member));
    };

    if (fixPhaseValues) {
        // Fix phase material values to avoid bad convergence
        for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
            setConstant(params.phaseMaterialValues[pieceIdx]);
        }
    } else {
        // Fix one of the phase material values to fix the scale of the phase material values.
        // (Otherwise it's a gauge freedom.)
        setConstant(params.phaseMaterialValues[(int)Piece::Knight]);

        // Kings are always on the board, so their phase material value has a gauge freedom with the
        // late eval terms: changing the phase material value of the king is akin to shifting the late
        // eval terms along the linear path to the early eval terms.
        setConstant(params.phaseMaterialValues[(int)Piece::King]);
    }

    // Fix piece values to avoid gauge freedoms with the piece-square tables.
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        setConstant(params.pieceValues[pieceIdx].early);
        setConstant(params.pieceValues[pieceIdx].late);
    }

    // A pawn 1 square away from promotion is always a passed pawn, so this term has a gauge
    // freedom with the piece-square tables.
    setConstant(params.passedPawnBonus[1].early);
    setConstant(params.passedPawnBonus[1].late);

    // Fix one value in the pawn adjustment tables to avoid gauge freedoms with the piece values.
    setConstant(params.bishopPawnSameColorBonus[4].early);
    setConstant(params.bishopPawnSameColorBonus[4].late);
    setConstant(params.knightPawnAdjustment[4].early);
    setConstant(params.knightPawnAdjustment[4].late);
    setConstant(params.rookPawnAdjustment[4].early);
    setConstant(params.rookPawnAdjustment[4].late);

    // Fix unused values

    // Pawns are never on the 1st or 8th ranks, so their piece-square tables for those ranks are
    // unused.
    for (int file = 0; file < kFiles; ++file) {
        const int pawnIdx       = (int)Piece::Pawn;
        const int rank1Position = (int)positionFromFileRank(file, 0);
        const int rank8Position = (int)positionFromFileRank(file, kRanks - 1);

        setConstant(params.pieceSquareTablesWhite[pawnIdx][rank1Position].early);
        setConstant(params.pieceSquareTablesWhite[pawnIdx][rank8Position].early);

        setConstant(params.pieceSquareTablesWhite[pawnIdx][rank1Position].late);
        setConstant(params.pieceSquareTablesWhite[pawnIdx][rank8Position].late);
    }

    // Pawns are never on the 8th rank, so the passed pawn bonus there is unused.
    setConstant(params.passedPawnBonus[0].early);
    setConstant(params.passedPawnBonus[0].late);

    // We don't calculate mobility for pawns or kings.
    setConstant(params.mobilityBonus[(int)Piece::Pawn].early);
    setConstant(params.mobilityBonus[(int)Piece::King].early);

    setConstant(params.mobilityBonus[(int)Piece::Pawn].late);
    setConstant(params.mobilityBonus[(int)Piece::King].late);

    // We don't calculate king tropism for pawns or kings.
    setConstant(params.kingTropismBonus[(int)Piece::Pawn].early);
    setConstant(params.kingTropismBonus[(int)Piece::King].early);

    setConstant(params.kingTropismBonus[(int)Piece::Pawn].late);
    setConstant(params.kingTropismBonus[(int)Piece::King].late);

    return constantParamIdxs;
}

void addResiduals(
        double& scaleParam,
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions,
        ceres::Problem& problem) {
    problem.AddParameterBlock(&scaleParam, 1);
    problem.AddParameterBlock(paramsDouble.data(), kNumEvalParams);

    const auto constantParamIdxs = std::make_shared<std::vector<int>>();
    *constantParamIdxs           = getConstantParamIdxs(/*fixPhaseValues*/ true);

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
    options.max_trust_region_radius           = 1e6;
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

void optimize(
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions) {
    double scaleParam = 400.;

    ceres::Problem problem;
    addResiduals(scaleParam, paramsDouble, scoredPositions, problem);

    problem.SetParameterBlockConstant(paramsDouble.data());
    solve(problem);

    std::println("Scale param: {}", scaleParam);

    problem.SetParameterBlockVariable(paramsDouble.data());
    problem.SetParameterBlockConstant(&scaleParam);
    solve(problem);
}
