#include "chess-engine-lib/Engine.h"
#include "chess-engine-lib/Math.h"
#include "chess-engine-lib/Perft.h"
#include "chess-engine-lib/UciFrontEnd.h"

#include <iostream>
#include <print>
#include <ranges>
#include <sstream>

void runPerft() {
    GameState gameState = GameState::startingPosition();

    std::println("Make + unmake:");
    perftPrint(gameState, 7, true);
}

int main() {
    std::locale::global(std::locale("en_US.UTF-8"));

    while (true) {
        std::string command;
        std::cin >> command;

        if (command == "uci") {
            Engine engine;
            UciFrontEnd uciFrontEnd(engine);
            uciFrontEnd.run();
            break;
        } else if (command == "perft") {
            runPerft();
        } else if (command == "exit") {
            break;
        }
    }
}