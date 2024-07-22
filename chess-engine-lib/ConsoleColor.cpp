#include "ConsoleColor.h"

#include "MyAssert.h"

#ifdef _WIN32
#include <Windows.h>

static_assert(sizeof(WORD) == sizeof(std::uint16_t));
#endif

ScopedConsoleColor::ScopedConsoleColor(const ConsoleColor color) {
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
    // TODO: implement for other platforms
}

ScopedConsoleColor::~ScopedConsoleColor() {
#ifdef _WIN32
    HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleTextAttribute(hStdout, oldAttributes_);
#endif
    // TODO: implement for other platforms
}
