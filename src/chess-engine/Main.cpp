#include "chess-engine-lib/Engine.h"
#include "chess-engine-lib/GameState.h"
#include "chess-engine-lib/Perft.h"
#include "chess-engine-lib/UciFrontEnd.h"

#include <deque>
#include <exception>
#include <iostream>
#include <locale>
#include <print>
#include <string>
#include <typeinfo>

namespace {

void runPerft() {
    GameState gameState = GameState::startingPosition();

    std::println("Make + unmake:");
    perftPrint(gameState, 7, true);
}

const std::string kEngineName = "programmatic-command";

}  // namespace

int main(int argc, char** argv) try {
    std::locale::global(std::locale("en_US.UTF-8"));

    std::deque<std::string> extraArgs(argv + 1, argv + argc);

    while (true) {
        std::string command;

        if (!extraArgs.empty()) {
            command = extraArgs.front();
            extraArgs.pop_front();
        } else {
            std::cin >> command;
        }

        if (command == "uci") {
            Engine engine;
            UciFrontEnd uciFrontEnd(engine, kEngineName);

            while (!extraArgs.empty()) {
                uciFrontEnd.pushProgrammaticCommand(extraArgs.back());
                extraArgs.pop_back();
            }

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
