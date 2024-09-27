#include "LoadPositions.h"
#include "Optimization.h"
#include "PostProcessing.h"
#include "PreProcessing.h"
#include "ScoredPosition.h"
#include "Utilities.h"

#include "chess-engine-lib/Eval.h"
#include "chess-engine-lib/GameState.h"
#include "chess-engine-lib/Math.h"
#include "chess-engine-lib/MoveOrder.h"

#include <print>
#include <ranges>

namespace {

std::array<double, kNumEvalParams> getInitialParams() {
    const EvalParams defaultParams = EvalParams::getDefaultParams();
    std::println("Initial params:\n{}\n\n", evalParamsToString(defaultParams));

    return evalParamsToDoubles(defaultParams);
}

void printResults(const std::array<double, kNumEvalParams>& paramsDouble) {
    const EvalParams params = evalParamsFromDoubles(paramsDouble);
    std::println("Optimized params:\n{}\n", evalParamsToString(params));

    const std::string paramsString =
            paramsDouble | std::ranges::views::transform([](double d) { return std::to_string(d); })
            | std::ranges::views::join_with(std ::string(", ")) | std::ranges::to<std::string>();

    std::println("\nOptimized param values: {}", paramsString);
}

}  // namespace

int main(int argc, char** argv) {
    std::array<double, kNumEvalParams> paramsDouble = getInitialParams();

    const std::vector<std::string> annotatedFensPaths = {
            R"(D:\annotated-fens\since_virtual_king_mobility_to_tune_old.txt)",
            R"(D:\annotated-fens\first-tune-attempts-vs-untuned.txt)",
            R"(D:\annotated-fens\first-fine-tuning.txt)",
            R"(D:\annotated-fens\more-tapered-eval-terms-tuning.txt)",
            R"(D:\annotated-fens\bad-castling-bonus-tune.txt)",
            R"(D:\annotated-fens\attack-defend.txt)",
            R"(D:\annotated-fens\global-pawn-adjustment.txt)"};

    const std::vector<int> dropoutRates = {8, 2, 4, 2, 2, 2, 1};

    std::println("Loading positions...");
    std::vector<ScoredPosition> scoredPositions;
    for (int i = 0; i < annotatedFensPaths.size(); ++i) {
        loadScoredPositions(annotatedFensPaths.at(i), dropoutRates.at(i), scoredPositions);
    }

    std::println("Quiescing positions...");
    quiescePositions(scoredPositions);

    std::println("Optimizing...");
    optimize(paramsDouble, scoredPositions);

    std::println("Post-processing...");
    postProcess(paramsDouble, scoredPositions);

    printResults(paramsDouble);
}
