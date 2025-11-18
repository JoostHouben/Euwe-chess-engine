#include "SearchStatistics.h"

#include <algorithm>
#include <chrono>
#include <cstdint>

SearchStatistics& SearchStatistics::operator+=(const SearchStatistics& other) {
    normalNodesSearched += other.normalNodesSearched;
    qNodesSearched += other.qNodesSearched;
    tTableHits += other.tTableHits;

    ttableUtilization = std::max(ttableUtilization, other.ttableUtilization);
    selectiveDepth    = std::max(selectiveDepth, other.selectiveDepth);

    if (tbHits || other.tbHits) {
        tbHits = tbHits.value_or(0) + other.tbHits.value_or(0);
    }

    timeElapsed += other.timeElapsed;

    const std::uint64_t totalNodes = normalNodesSearched + qNodesSearched;
    using FloatSeconds             = std::chrono::duration<float>;
    const float seconds            = std::chrono::duration_cast<FloatSeconds>(timeElapsed).count();
    nodesPerSecond                 = static_cast<float>(totalNodes) / seconds;

    return *this;
}

SearchStatistics SearchStatistics::operator+(const SearchStatistics& other) const {
    SearchStatistics result = *this;
    result += other;
    return result;
}
