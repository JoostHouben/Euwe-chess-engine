#pragma once

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
    explicit ScopedConsoleColor(ConsoleColor color);
    ~ScopedConsoleColor();

  private:
#ifdef _WIN32
    std::uint16_t oldAttributes_;
#endif
};
