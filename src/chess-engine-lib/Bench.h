#pragma once

#include <string>
#include <vector>

enum class BenchSearchDepth {
    Deep,
    Shallow,
    TimeControl,
};

std::vector<std::string> getBenchCommands(BenchSearchDepth benchSearchDepth);
