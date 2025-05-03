#include "chess-engine-lib/Engine.h"
#include "chess-engine-lib/Math.h"
#include "chess-engine-lib/Perft.h"
#include "chess-engine-lib/UciFrontEnd.h"

#include <iostream>
#include <print>
#include <typeinfo>

void runPerft() {
    GameState gameState = GameState::startingPosition();

    std::println("Make + unmake:");
    perftPrint(gameState, 7, true);
}

int main() try {
    std::locale::global(std::locale("en_US.UTF-8"));

    while (true) {
        std::string command;
        std::cin >> command;

        if (command == "uci") {
            Engine engine;
            UciFrontEnd uciFrontEnd(engine, "safe-mobility-adjustment");
            uciFrontEnd.run();
            break;
        } else if (command == "perft") {
            runPerft();
        } else if (command == "exit") {
            break;
        }
    }
} catch (const std::exception& e) {
    std::println(std::cerr, "Uncaught exception of type '{}':\n'{}'", typeid(e).name(), e.what());
    return 1;
} catch (...) {
    std::println(std::cerr, "Uncaught exception of unknown type.");
    return 1;
}
