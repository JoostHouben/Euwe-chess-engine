#!/bin/bash

if [ ! -f out/build/linux-clang-debug/compile_commands.json ]; then
    echo "compile_commands.json not found. Run: cmake --preset linux-clang-debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
    exit 1
fi

file_list=$(find src \
  ! -path "src/chess-engine-lib/Pyrrhic/*" \
  -type f \( -name "*.cpp" -o -name "*.h" \))

length=$(wc -w <<< "$file_list")
echo Running clang-tidy on $length files

overall_code=0

for file in $file_list; do
  clang-tidy-21 $file -p out/build/linux-clang-debug/ -warnings-as-errors=* --config-file=ci/.clang-tidy-ci --quiet
  code=$?
  if [ $code -ne 0 ]; then
      overall_code=$code
  fi
done

exit $overall_code
