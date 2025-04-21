#include "LoadPositions.h"

#include "chess-engine-lib/RangePatches.h"

#include <execution>
#include <fstream>
#include <print>
#include <ranges>
#include <sstream>
#include <syncstream>

#include <cstdlib>

namespace {

// Version of std::getline that handles different line ending conventions gracefully, even if not
// matching the platform's convention.
// Source: https://stackoverflow.com/a/6089413
std::istream& safeGetline(std::istream& is, std::string& t) {
    t.clear();

    // The characters in the stream are read one-by-one using a std::streambuf.
    // That is faster than reading them one-by-one using the std::istream.
    // Code that uses streambuf this way must be guarded by a sentry object.
    // The sentry object performs various tasks,
    // such as thread synchronization and updating the stream state.

    std::istream::sentry se(is, true);
    std::streambuf* sb = is.rdbuf();

    for (;;) {
        int c = sb->sbumpc();
        switch (c) {
            case '\n':
                return is;
            case '\r':
                if (sb->sgetc() == '\n')
                    sb->sbumpc();
                return is;
            case std::streambuf::traits_type::eof():
                // Also handle the case when the last line has no line ending
                if (t.empty())
                    is.setstate(std::ios::eofbit);
                return is;
            default:
                t += (char)c;
        }
    }
}

std::vector<ScoredPosition> loadScoredPositions(
        const std::filesystem::path& annotatedFensPath,
        const int dropoutRate,
        std::ostream* logOutput) {
    std::ifstream in(annotatedFensPath);

    std::vector<ScoredPosition> scoredPositions;

    std::string inputLine;
    while (safeGetline(in, inputLine)) {
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

        double score{};
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

    if (logOutput) {
        std::osyncstream out(*logOutput);
        std::println(
                out,
                "Read {} scored positions from {}",
                scoredPositions.size(),
                annotatedFensPath.filename().string());
    }

    return scoredPositions;
}

}  // namespace

std::vector<ScoredPosition> loadScoredPositions(
        std::vector<std::pair<std::filesystem::path, int>> pathsAndDropoutRates,
        const int additionalDropOutRate,
        std::ostream* logOutput) {
    std::vector<std::vector<ScoredPosition>> nestedScoredPositions(pathsAndDropoutRates.size());

    std::transform(
            std::execution::par_unseq,
            pathsAndDropoutRates.begin(),
            pathsAndDropoutRates.end(),
            nestedScoredPositions.begin(),
            [&](const auto& pathAndDropoutRate) {
                return loadScoredPositions(
                        pathAndDropoutRate.first,
                        pathAndDropoutRate.second * additionalDropOutRate,
                        logOutput);
            });

    return std::ranges::views::join(nestedScoredPositions)
         | range_to<std::vector<ScoredPosition>>();
}
