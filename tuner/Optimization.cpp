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
        setConstant(params.pieceValuesEarly[pieceIdx]);
        setConstant(params.pieceValuesLate[pieceIdx]);
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

    //// Kings are always on the board, so their piece and phase material values are unused.
    //setConstant(params.pieceValuesEarly[(int)Piece::King]);
    //setConstant(params.pieceValuesLate[(int)Piece::King]);
    //setConstant(params.phaseMaterialValues[(int)Piece::King]);

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
