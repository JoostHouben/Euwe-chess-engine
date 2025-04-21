#include "Side.h"

#include "MyAssert.h"

namespace {

constexpr char kLowerCaseBit = 1 << 5;

}  // namespace

Side sideFromFenChar(char c) {
    bool isUpperCase = !(c & kLowerCaseBit);
    if (isUpperCase) {
        return Side::White;
    } else {
        return Side::Black;
    }
}

char toFenChar(Side side) {
    switch (side) {
        case Side::White:
            return 'w';
        case Side::Black:
            return 'b';
    }
    UNREACHABLE;
}
