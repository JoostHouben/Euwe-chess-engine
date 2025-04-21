set(CMAKE_C_COMPILER "clang-19")
set(CMAKE_CXX_COMPILER "clang++-19")

set(CMAKE_CXX_FLAGS_INIT "-fsanitize=memory -stdlib=libc++ -L${CMAKE_CURRENT_LIST_DIR}/llvm-project/build/msan/lib -lc++abi -I${CMAKE_CURRENT_LIST_DIR}/llvm-project/build/msan/include -I${CMAKE_CURRENT_LIST_DIR}/llvm-project/build/msan/include/c++/v1")
set(CMAKE_C_FLAGS_INIT "-fsanitize=memory -stdlib=libc++ -L${CMAKE_CURRENT_LIST_DIR}/llvm-project/build/msan/lib -lc++abi -I${CMAKE_CURRENT_LIST_DIR}/llvm-project/build/msan/include -I${CMAKE_CURRENT_LIST_DIR}/llvm-project/build/msan/include/c++/v1")
