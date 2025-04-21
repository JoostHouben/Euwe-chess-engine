#!/bin/bash

# Prepare clean llvm-project directory
rm -rf llvm-project
mkdir llvm-project
cd llvm-project

# clone LLVM
git init
git remote add origin https://github.com/llvm/llvm-project
git fetch --depth=1 origin 923a5c4f83d2b3675bb88e9fe441daeaa4d69488
git checkout FETCH_HEAD

mkdir -p build/msan; cd build/msan

# configure cmake
cmake -GNinja -S ../../runtimes \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_RUNTIMES="libcxx;libcxxabi" \
    -DLIBCXXABI_USE_LLVM_UNWINDER=off \
    -DCMAKE_C_COMPILER=clang-19 \
    -DCMAKE_CXX_COMPILER=clang++-19 \
    -DLLVM_USE_SANITIZER=MemoryWithOrigins

# build the libraries
cmake --build . -- cxx cxxabi
