#!/bin/bash

file_list=$(find chess-engine chess-engine-lib tests tuner \
  ! -path "chess-engine-lib/Pyrrhic/*" \
  -type f \( -name "*.cpp" -o -name "*.h" \))

length=$(wc -w <<< "$file_list")
echo Running clang-tidy on $length files

counter=0
status=0
for file in $file_list; do
    ((counter++))
    echo $counter/$length: $file
    clang-tidy-19 "$file" -p out/build/linux-clang-debug/ -warnings-as-errors=* --config-file=ci/.clang-tidy-ci --quiet 2>/dev/null
    code=$?
    if [ $code -ne 0 ]; then
        status=$code
    fi
done

exit $status
