#pragma once

#include <ostream>

#include <cstdint>

enum class ConsoleColor {
    White,
    BrightWhite,
    Green,
    BrightGreen,
    Yellow,
    Red,
};

class ScopedConsoleColor {
  public:
    explicit ScopedConsoleColor(ConsoleColor color, std::ostream& stream);
    ~ScopedConsoleColor();

    ScopedConsoleColor(const ScopedConsoleColor&)            = delete;
    ScopedConsoleColor& operator=(const ScopedConsoleColor&) = delete;

    ScopedConsoleColor(ScopedConsoleColor&&)            = delete;
    ScopedConsoleColor& operator=(ScopedConsoleColor&&) = delete;

  private:
#ifdef _WIN32
    std::uint16_t oldAttributes_;
#endif

#ifdef _POSIX_SOURCE
    bool isTerminal_;
    std::ostream& stream_;
#endif
};
