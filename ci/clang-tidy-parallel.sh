#!/bin/bash

if [ ! -f out/build/linux-clang-debug/compile_commands.json ]; then
    echo "compile_commands.json not found. Run: cmake --preset linux-clang-debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
    exit 1
fi

find src \
  ! -path "src/chess-engine-lib/Pyrrhic/*" \
  -type f \( -name "*.cpp" -o -name "*.h" \) \
  | parallel --progress "clang-tidy-19 {} -p out/build/linux-clang-debug/ -warnings-as-errors=* --config-file=ci/.clang-tidy-ci --quiet 2>/dev/null"

code=$?

if [ $code -ne 0 ]; then
    echo Clang-tidy reported errors in $code files.
else
    echo No errors found.
fi

exit $code
