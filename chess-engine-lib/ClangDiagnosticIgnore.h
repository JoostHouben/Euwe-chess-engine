#ifdef __clang__
// Incorrectly triggers because Visual Studio runs clang-tidy directly on headers
#pragma clang diagnostic ignored "-Wpragma-once-outside-header"

// Triggers on pragmas in the MSVC stdlib.
#pragma clang diagnostic ignored "-Wunknown-pragmas"

// "warning: the argument to A has side effects that will be discarded"
// Seems to spuriously trigger on 'getters' inside an assume.
// TODO: could maybe mark these as pure / const using clang function attributes.
#pragma clang diagnostic ignored "-Wassume"
#endif

#pragma once
