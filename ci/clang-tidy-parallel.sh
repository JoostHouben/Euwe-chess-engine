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

echo "$file_list" |
  parallel --bar "clang-tidy-19 {} -p out/build/linux-clang-debug/ -warnings-as-errors=* --config-file=ci/.clang-tidy-ci --quiet 2>/dev/null" \
  2> >(
    perl -pe 'BEGIN{$/="\r";$|=1};s/\r/\n/g' |
    grep '%' |
    perl -pe 'BEGIN{$|=1}s/\e\[[0-9;]*[a-zA-Z]//g' |
    perl -pe "BEGIN{\$length=$length;$|=1} s|(\d+)% (\d+):\d+=\S+ (\S+).*|\$1% (\$2/\$length) -- \$3|" |
    perl -ne 'BEGIN{$|=1}$s{$_}++ or print')

code=$?

if [ $code -ne 0 ]; then
    echo Clang-tidy reported errors in $code files.
else
    echo No errors found.
fi

exit $code
