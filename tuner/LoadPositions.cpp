#include "LoadPositions.h"

#include <fstream>
#include <print>
#include <sstream>

#include <cstdlib>

void loadScoredPositions(
        const std::string& annotatedFensPath,
        const int dropoutRate,
        std::vector<ScoredPosition>& scoredPositions) {
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

    std::println("Read {} scored positions", scoredPositions.size() - startingSize);
}
