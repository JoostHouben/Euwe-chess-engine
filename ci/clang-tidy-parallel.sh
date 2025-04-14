#!/bin/bash

find chess-engine chess-engine-lib tests tuner \
  ! -path "chess-engine-lib/Pyrrhic/*" \
  -type f \( -name "*.cpp" -o -name "*.h" \) \
  | parallel --progress "clang-tidy-19 {} -p out/build/linux-clang-debug/ -warnings-as-errors=* --config-file=ci/.clang-tidy-ci --quiet 2>/dev/null"

code=$?

if [ $code -ne 0 ]; then
    echo Clang-tidy reported errors in $code files.
else
    echo No errors found.
fi

exit $code
