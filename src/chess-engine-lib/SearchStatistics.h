#pragma once

#include <chrono>
#include <optional>

#include <cstdint>

struct SearchStatistics {
    std::uint64_t normalNodesSearched = 0;
    std::uint64_t qNodesSearched      = 0;
    std::uint64_t tTableHits          = 0;
    float ttableUtilization           = 0.0f;
    int selectiveDepth                = 0;
    std::optional<int> tbHits         = std::nullopt;

    std::chrono::milliseconds timeElapsed{};
    float nodesPerSecond = 0.0f;
};
