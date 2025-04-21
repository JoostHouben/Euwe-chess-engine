#include "Optimization.h"

#include "Utilities.h"

#include "chess-engine-lib/Eval.h"

#include "ceres/ceres.h"

#include <print>
#include <vector>

#include <cstdlib>

namespace {

using SparsityStructure = std::vector<std::pair<std::size_t, std::size_t>>;

const std::size_t firstTaperedTermIdx = EvalParams::getDefaultParams().getFirstTaperedTermIndex();

struct EvalCostFunctor : ceres::CostFunction {
    EvalCostFunctor(const ScoredPosition& scoredPosition, SparsityStructure sparsityStructure)
        : scoredPosition_(scoredPosition), sparsityStructure_(std::move(sparsityStructure)) {
        auto& parameterBlockSizes = *mutable_parameter_block_sizes();

        parameterBlockSizes.reserve(sparsityStructure_.size() + 1);
        for (const auto& [blockStart, blockSize] : sparsityStructure_) {
            parameterBlockSizes.push_back((int)blockSize);
        }
        parameterBlockSizes.push_back(1);

        set_num_residuals(1);
    }

    // NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    bool Evaluate(
            double const* const* parameters, double* residuals, double** jacobians) const override {
        const double* const scaleParam = parameters[sparsityStructure_.size()];

        bool needParamJacobians = false;
        if (jacobians) {
            for (std::size_t sparseIdx = 0; sparseIdx < sparsityStructure_.size(); ++sparseIdx) {
                if (jacobians[sparseIdx]) {
                    needParamJacobians = true;
                    break;
                }
            }
        }

        const Evaluator evaluator(getEvalParams(parameters));

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
            if (jacobians[sparsityStructure_.size()]) {
                const double power = std::pow(10, x / s);

                jacobians[sparsityStructure_.size()][0] =
                        x * std::log(10.) * power / (s * s * (1 + power) * (1 + power));
            }

            if (needParamJacobians) {
                const double negPower = std::pow(10, -x / s);

                const double sigmoidDerivative =
                        std::log(10.) * negPower / (s * (1 + negPower) * (1 + negPower));

                for (std::size_t sparseIdx = 0; sparseIdx < sparsityStructure_.size();
                     ++sparseIdx) {
                    if (jacobians[sparseIdx] == nullptr) {
                        continue;
                    }

                    const auto& [blockStart, blockSize] = sparsityStructure_[sparseIdx];
                    for (std::size_t blockIdx = 0; blockIdx < blockSize; ++blockIdx) {
                        const std::size_t denseIdx = blockStart + blockIdx;
                        jacobians[sparseIdx][blockIdx] =
                                -sigmoidDerivative * evalWithGradient.gradient[denseIdx];
                    }
                }
            }
        }

        return true;
    }

    EvalParams getEvalParams(const double* const* parameters) const {
        auto paramsArray = evalParamsToArray(EvalParams::getDefaultParams());

        for (std::size_t sparseIdx = 0; sparseIdx < sparsityStructure_.size(); ++sparseIdx) {
            const auto& [blockStart, blockSize] = sparsityStructure_[sparseIdx];
            for (std::size_t blockIdx = 0; blockIdx < blockSize; ++blockIdx) {
                const std::size_t denseIdx = blockStart + blockIdx;
                paramsArray[denseIdx]      = (EvalCalcT)parameters[sparseIdx][blockIdx];
            }
        }

        return evalParamsFromArray(paramsArray);
    }
    // NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)

  private:
    ScoredPosition scoredPosition_;
    SparsityStructure sparsityStructure_;
};

void setParameterBlocksConstantForSolvingEvalParams(
        std::array<double, kNumEvalParams>& paramsDouble,
        bool fixPhaseValues,
        ceres::Problem& problem) {

    const EvalParams params = EvalParams::getDefaultParams();
    const auto getPointer   = [&](const EvalCalcT& member) -> double* {
        return paramsDouble.data() + params.getParamIndex(member);
    };
    const auto setConstant = [&](const EvalCalcT& member) {
        problem.SetParameterBlockConstant(getPointer(member));
    };
    const auto setTaperedTermConstant = [&](const TaperedTerm& term) {
        setConstant(term.early);
    };

    // Tempo bonus is incorrectly optimized away.
    setTaperedTermConstant(params.tempoBonus);

    if (fixPhaseValues) {
        // Fix phase material values to avoid bad convergence
        for (const auto& phaseMaterialValue : params.phaseMaterialValues) {
            setConstant(phaseMaterialValue);
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
        setTaperedTermConstant(params.pieceValues[pieceIdx]);
    }

    // Fix one value in the pawn adjustment tables to avoid gauge freedoms with the piece values.
    setTaperedTermConstant(params.bishopPawnSameColorAdjustment[4]);
    setTaperedTermConstant(params.bishopEnemyPawnSameColorAdjustment[4]);
    setTaperedTermConstant(params.knightPawnAdjustment[4]);
    setTaperedTermConstant(params.rookPawnAdjustment[4]);
    setTaperedTermConstant(params.queenPawnAdjustment[4]);

    setTaperedTermConstant(params.controlNearEnemyKing[0]);
    setTaperedTermConstant(params.numKingAttackersAdjustment[0]);

    // For opposite colored bishop endgames with a large pawn delta we don't have enough data.
    // And fixing it to a factor of 1 is ok: the large pawn delta will give a sufficiently high
    // score to indicate a win.
    setConstant(params.oppositeColoredBishopFactor.back());
}

void setAllEvalParameterBlocksConstant(
        std::array<double, kNumEvalParams>& paramsDouble, ceres::Problem& problem) {
    // Set all parameters constant except for the scale parameter
    for (std::size_t i = 0; i < firstTaperedTermIdx; ++i) {
        problem.SetParameterBlockConstant(paramsDouble.data() + i);
    }
    for (std::size_t i = firstTaperedTermIdx; i < kNumEvalParams; i += 2) {
        problem.SetParameterBlockConstant(paramsDouble.data() + i);
    }
}

void setAllEvalParameterBlocksVariable(
        std::array<double, kNumEvalParams>& paramsDouble, ceres::Problem& problem) {
    // Set all parameters constant except for the scale parameter
    for (std::size_t i = 0; i < firstTaperedTermIdx; ++i) {
        problem.SetParameterBlockVariable(paramsDouble.data() + i);
    }
    for (std::size_t i = firstTaperedTermIdx; i < kNumEvalParams; i += 2) {
        problem.SetParameterBlockVariable(paramsDouble.data() + i);
    }
}

SparsityStructure getSparsityStructure(const GameState& gameState, const Evaluator& evaluator) {
    const EvalWithGradient evalWithGradient = evaluator.evaluateWithGradient(gameState);

    SparsityStructure sparsityStructure;
    for (std::size_t i = 0; i < firstTaperedTermIdx; ++i) {
        if (evalWithGradient.gradient[i] != 0) {
            sparsityStructure.push_back({i, 1});
        }
    }
    for (std::size_t i = firstTaperedTermIdx; i < kNumEvalParams; i += 2) {
        if (evalWithGradient.gradient[i] != 0 || evalWithGradient.gradient[i + 1] != 0) {
            sparsityStructure.push_back({i, 2});
        }
    }

    return sparsityStructure;
}

std::vector<double*> getSparseParameterBlocks(
        double& scaleParam,
        std::array<double, kNumEvalParams>& paramsDouble,
        const SparsityStructure& sparsityStructure) {
    std::vector<double*> sparseParameterBlocks;
    sparseParameterBlocks.reserve(sparsityStructure.size() + 1);

    for (const auto& [paramIdx, blockSize] : sparsityStructure) {
        sparseParameterBlocks.push_back(paramsDouble.data() + paramIdx);
    }

    sparseParameterBlocks.push_back(&scaleParam);

    return sparseParameterBlocks;
}

void addResiduals(
        double& scaleParam,
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions,
        const int subsampleRate,
        ceres::Problem& problem) {
    problem.AddParameterBlock(&scaleParam, 1);

    for (std::size_t i = 0; i < firstTaperedTermIdx; ++i) {
        problem.AddParameterBlock(paramsDouble.data() + i, 1);
    }
    for (std::size_t i = firstTaperedTermIdx; i < kNumEvalParams; i += 2) {
        problem.AddParameterBlock(paramsDouble.data() + i, 2);
    }

    Evaluator evaluator(evalParamsFromDoubles(paramsDouble));

    for (const auto& scoredPosition : scoredPositions) {
        if (std::rand() % subsampleRate != 0) {
            continue;
        }

        auto sparsityStructure = getSparsityStructure(scoredPosition.gameState, evaluator);

        auto sparseParameterBlocks =
                getSparseParameterBlocks(scaleParam, paramsDouble, sparsityStructure);

        ceres::CostFunction* costFunction =
                new EvalCostFunctor(scoredPosition, std::move(sparsityStructure));
        problem.AddResidualBlock(costFunction, nullptr, std::move(sparseParameterBlocks));
    }
}

void solve(ceres::Problem& problem, const bool useTrustRegionMethod) {
    ceres::Solver::Options options;
    options.minimizer_progress_to_stdout = true;
    options.num_threads                  = std::thread::hardware_concurrency();

    if (useTrustRegionMethod) {
        options.parameter_tolerance           = 1e-3;
        options.initial_trust_region_radius   = 1e4;
        options.max_trust_region_radius       = 1e6;
        options.max_num_refinement_iterations = 3;
        options.linear_solver_type            = ceres::SPARSE_NORMAL_CHOLESKY;
    } else {
        options.minimizer_type = ceres::MinimizerType::LINE_SEARCH;
    }

    ceres::Solver::Summary summary;
    ceres::Solve(options, &problem, &summary);
    std::println("\n{}\n", summary.FullReport());
}

void solveScale(
        ceres::Problem& problem,
        double& scaleParam,
        std::array<double, kNumEvalParams>& paramsDouble) {
    setAllEvalParameterBlocksConstant(paramsDouble, problem);
    problem.SetParameterBlockVariable(&scaleParam);

    solve(problem, /*useTrustRegionMethod*/ true);
}

void solveParams(
        ceres::Problem& problem,
        double& scaleParam,
        std::array<double, kNumEvalParams>& paramsDouble,
        const bool fixPhaseValues) {
    setAllEvalParameterBlocksVariable(paramsDouble, problem);
    setParameterBlocksConstantForSolvingEvalParams(paramsDouble, fixPhaseValues, problem);
    problem.SetParameterBlockConstant(&scaleParam);

    solve(problem, /*useTrustRegionMethod*/ true);
}

}  // namespace

void optimize(
        std::array<double, kNumEvalParams>& paramsDouble,
        const std::vector<ScoredPosition>& scoredPositions,
        const bool fixPhaseValues) {
    double scaleParam = 400;

    ceres::Problem problem;
    addResiduals(
            scaleParam,
            paramsDouble,
            scoredPositions,
            /*subSampleRate*/ 1,
            problem);

    solveScale(problem, scaleParam, paramsDouble);

    std::println("Scale param: {}", scaleParam);

    solveParams(problem, scaleParam, paramsDouble, fixPhaseValues);
}
