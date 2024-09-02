#include "ConsoleColor.h"

#include "MyAssert.h"

#ifdef _WIN32
#include <Windows.h>

static_assert(sizeof(WORD) == sizeof(std::uint16_t));
#endif

#ifdef _POSIX_SOURCE
#include <unistd.h>

#include <stdio.h>
#endif

ScopedConsoleColor::ScopedConsoleColor(
        const ConsoleColor color, [[maybe_unused]] std::ostream& stream)
#ifdef _POSIX_SOURCE
    : stream_(stream)
#endif
{
#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO screenBufferInfo;
    HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
    GetConsoleScreenBufferInfo(hStdout, &screenBufferInfo);
    oldAttributes_ = screenBufferInfo.wAttributes;

    WORD newAttributes = 0;

    switch (color) {
        case ConsoleColor::White:
            newAttributes = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
            break;

        case ConsoleColor::BrightWhite:
            newAttributes =
                    FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY;
            break;

        case ConsoleColor::Green:
            newAttributes = FOREGROUND_GREEN;
            break;

        case ConsoleColor::BrightGreen:
            newAttributes = FOREGROUND_GREEN | FOREGROUND_INTENSITY;
            break;

        case ConsoleColor::Yellow:
            newAttributes = FOREGROUND_RED | FOREGROUND_GREEN;
            break;

        case ConsoleColor::Red:
            newAttributes = FOREGROUND_RED;
            break;

        default:
            UNREACHABLE;
    }

    SetConsoleTextAttribute(hStdout, newAttributes);
#endif

#ifdef _POSIX_SOURCE
    isTerminal_ = isatty(fileno(stdout));
    if (!isTerminal_) {
        return;
    }

    switch (color) {
        case ConsoleColor::White:
            stream << "\033[37m";
            break;

        case ConsoleColor::BrightWhite:
            stream << "\033[1;37m";
            break;

        case ConsoleColor::Green:
            stream << "\033[32m";
            break;

        case ConsoleColor::BrightGreen:
            stream << "\033[1;32m";
            break;

        case ConsoleColor::Yellow:
            stream << "\033[33m";
            break;

        case ConsoleColor::Red:
            stream << "\033[31m";
            break;

        default:
            UNREACHABLE;
    }
#endif
}

ScopedConsoleColor::~ScopedConsoleColor() {
#ifdef _WIN32
    HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleTextAttribute(hStdout, oldAttributes_);
#endif

#ifdef _POSIX_SOURCE
    if (!isTerminal_) {
        return;
    }

    stream_ << "\033[0m";
#endif
}
