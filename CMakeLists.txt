# CMakeList.txt : Top-level CMake project file, do global configuration
# and include sub-projects here.
#
cmake_minimum_required (VERSION 3.15)

set (CMAKE_USER_MAKE_RULES_OVERRIDE "${CMAKE_CURRENT_LIST_DIR}/CompilerOptions.cmake")

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT
    "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug>:EditAndContinue>$<$<CONFIG:RelWithDebInfo>:ProgramDatabase>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

if ("tests" IN_LIST TARGETS_TO_BUILD)
    list(APPEND VCPKG_MANIFEST_FEATURES "tests")
endif()

if ("tuner" IN_LIST TARGETS_TO_BUILD)
    list(APPEND VCPKG_MANIFEST_FEATURES "tuner")
endif()

project ("Euwe")

# Detect compiler
if (MSVC)
    # MSVC or clang-cl
    set(MSVC_LIKE TRUE)
    set(GCC_LIKE FALSE)

    if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        message("Detected clang-cl version ${CMAKE_CXX_COMPILER_VERSION}")

        set(CLANG_CL TRUE)
    else()
        set(CLANG_CL FALSE)
    endif()
else()
    # GCC or Clang (not clang-cl)
    set(MSVC_LIKE FALSE)
    set(GCC_LIKE TRUE)
    set(CLANG_CL FALSE)
endif()

# Set warnings, and treat them as errors
if (MSVC)
    add_compile_options(/W4 /WX)
    # Disable warning C4714: 'function' : marked as __forceinline not inlined
    add_compile_options(/wd4714)
else()
    add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()

# Enable exceptions support.
if (CLANG_CL)
    # Explicitly enable exceptions support for clang-cl (it's only enabled by CMake when
    # targeting the Windows-MSVC platform, see https://github.com/danvratil/qcoro/issues/90 for
    # details)
    add_compile_options(/EHsc)
endif()

# Set FP options: use precise math and disable contractions. This ensure consistent FP results
# across different compilers and optimization levels.
if (MSVC_LIKE)
    # Enable precise floating-point model. Implies no contractions.
    add_compile_options(/fp:precise)

    if (CLANG_CL AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS "20.0.0")
        message("Disabling floating-point contractions for clang-cl < 20.0.0")
        # /fp:precise implies disabling floating-point contractions (like fused multiply-add),
        # but prior to clang-cl version 20, clang-cl did not respect /fp:precise for that purpose.
        # So we need to explicitly disable floating-point contractions.
        add_compile_options(/clang:-ffp-contract=off)
    endif()
else() # GCC_LIKE
    # Precise floating-point model is the default, but contractions need to be explicitly disabled.
    add_compile_options(-ffp-contract=off)
endif()

# Set the architecture to AVX2 (and BMI2).
# This is needed for instructions like PDEP/PEXT, popcount, tzcount, etc., all of which are used
# heavily in core logic.
if (MSVC_LIKE)
    add_compile_options(/arch:AVX2)
else() # GCC_LIKE
    add_compile_options(-mavx2 -mbmi2)
endif()

# Enable Interprocedural Optimization (Link-time optimization / whole program optimization), if supported.
include(CheckIPOSupported)
check_ipo_supported(RESULT ipo_supported OUTPUT error)
if  (ipo_supported)
    message(STATUS "IPO / LTO enabled")
    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELWITHDEBINFO TRUE)
else()
    message(STATUS "IPO / LTO not supported: <${error}>")
endif()

# Ensure optimization and debug flags are set correctly.
if (CLANG_CL)
    # CMake doesn't seem to set optimization flags correctly for clang-cl, so we set them manually.
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2 /DNDEBUG")
endif()
if (MSVC)
    # Options to ensure that RelWithDebInfo builds are optimized.
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/O2 /Ob2 /DNDEBUG")
endif()

enable_testing()

# Include sub-projects.
add_subdirectory("src")
