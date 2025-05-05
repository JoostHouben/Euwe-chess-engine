#include "LoadPositions.h"

#include "chess-engine-lib/RangePatches.h"

#include <execution>
#include <fstream>
#include <print>
#include <ranges>
#include <sstream>
#include <syncstream>

#include <cmath>
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

std::optional<bool> parseBool(const std::string& str) {
    std::string lowerStr = str;
    std::transform(lowerStr.begin(), lowerStr.end(), lowerStr.begin(), ::tolower);
    if (lowerStr == "true") {
        return true;
    } else if (lowerStr == "false") {
        return false;
    } else {
        return std::nullopt;
    }
}

std::optional<AnnotatedPosition> loadPositionFromLine(std::string line) {
    std::stringstream lineSStream(std::move(line));

    std::string token;
    lineSStream >> token;

    if (token != "fen") {
        return std::nullopt;
    }

    std::string fen;
    while (true) {
        lineSStream >> token;
        if (token == "game_id" || !lineSStream.good()) {
            break;
        }
        if (!fen.empty()) {
            fen += " ";
        }
        fen += token;
    }
    if (token != "game_id" || !lineSStream.good()) {
        return std::nullopt;
    }
    const GameState gameState = GameState::fromFen(fen);

    std::uint64_t gameId{};
    lineSStream >> gameId;

    lineSStream >> token;
    if (token != "ply_count") {
        return std::nullopt;
    }
    int plyCount{};
    lineSStream >> plyCount;

    lineSStream >> token;
    if (token != "final_score") {
        return std::nullopt;
    }
    double finalScore{};
    lineSStream >> finalScore;

    lineSStream >> token;
    if (token != "search_eval") {
        return std::nullopt;
    }
    double searchEval{};
    lineSStream >> searchEval;
    if (std::isnan(searchEval)) {
        return std::nullopt;
    }
    const int searchEvalCp = (int)std::round(searchEval * 100.0);

    lineSStream >> token;
    if (token != "move_is_capture") {
        return std::nullopt;
    }
    lineSStream >> token;
    const auto moveIsCapture = parseBool(token);
    if (!moveIsCapture.has_value()) {
        return std::nullopt;
    }

    return AnnotatedPosition{
            .gameState     = gameState,
            .gameId        = gameId,
            .plyCount      = plyCount,
            .finalScore    = finalScore,
            .searchEvalCp  = searchEvalCp,
            .moveIsCapture = *moveIsCapture,
    };
}

std::vector<AnnotatedPosition> loadPositions(
        const std::filesystem::path& annotatedFensPath,
        const int dropoutRate,
        std::ostream* logOutput) {
    std::ifstream in(annotatedFensPath);

    std::vector<AnnotatedPosition> positions;

    std::string inputLine;
    while (safeGetline(in, inputLine)) {
        if (inputLine.empty()) {
            continue;
        }

        if ((std::rand() % dropoutRate) != 0) {
            continue;
        }

        auto maybeScoredPosition = loadPositionFromLine(std::move(inputLine));
        if (!maybeScoredPosition) {
            continue;
        }

        positions.push_back(std::move(*maybeScoredPosition));
    }

    if (logOutput) {
        std::osyncstream out(*logOutput);
        std::println(
                out,
                "Read {} scored positions from {}",
                positions.size(),
                annotatedFensPath.filename().string());
    }

    return positions;
}

}  // namespace

std::vector<AnnotatedPosition> loadPositions(
        std::vector<std::pair<std::filesystem::path, int>> pathsAndDropoutRates,
        const int additionalDropOutRate,
        std::ostream* logOutput) {
    std::vector<std::vector<AnnotatedPosition>> nestedPositions(pathsAndDropoutRates.size());

    std::transform(
            std::execution::par_unseq,
            pathsAndDropoutRates.begin(),
            pathsAndDropoutRates.end(),
            nestedPositions.begin(),
            [&](const auto& pathAndDropoutRate) {
                return loadPositions(
                        pathAndDropoutRate.first,
                        pathAndDropoutRate.second * additionalDropOutRate,
                        logOutput);
            });

    return std::ranges::views::join(nestedPositions) | range_to<std::vector<AnnotatedPosition>>();
}
