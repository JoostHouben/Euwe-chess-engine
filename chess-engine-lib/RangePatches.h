#pragma once

#include <string>
#include <string_view>

// Replacement for join_with and to<string>, necessary because of some clang-20 / libstdc++
// compatibility issue.
// Usage:
//   range | joinToString(pattern)
// Equivalent to:
//   range | std::ranges::views::join_with(std::string_view(pattern)) | std::ranges::to<std::string>()

struct JoinToString {
    explicit JoinToString(std::string_view pattern) : pattern_(pattern) {}

    template <typename R>
    std::string operator()(R&& range) const {
        std::string result;
        bool first = true;
        for (const auto& value : std::forward<R>(range)) {
            if (!first) {
                result += pattern_;
            }
            result += value;
            first = false;
        }
        return result;
    }

  private:
    std::string_view pattern_;
};

inline JoinToString joinToString(std::string_view pattern) {
    return JoinToString(pattern);
}

template <typename R>
std::string operator|(R&& range, JoinToString f) {
    return f(std::forward<R>(range));
}

// Replacement for to<T>, necessary because of some clang-20 / libstdc++ compatibility issue.
// Usage:
//   range | range_to<T>()
// Equivalent to:
//   range | std::ranges::to<T>()

template <typename T>
struct RangeTo {
    template <typename R>
    T operator()(R&& range) const {
        T result{};
        for (auto&& value : std::forward<R>(range)) {
            result.push_back(std::move(value));
        }
        return result;
    }
};

template <typename T>
RangeTo<T> range_to() {
    return RangeTo<T>();
}

template <typename R, typename T>
T operator|(R&& range, RangeTo<T> f) {
    return f(std::forward<R>(range));
}
