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

# Set the architecture to AVX2 (and BMI2)
if (MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /arch:AVX2 /fp:precise")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mavx2 -mbmi2")
endif()

# Set warnings, and treat them as errors
if (MSVC)
    add_compile_options(/W4 /WX)
    # Disable warning C4714: 'function' : marked as __forceinline not inlined
    add_compile_options(/wd4714)
else()
    add_compile_options(-Wall -Wextra -pedantic -Werror)
endif()

if (MSVC)
    # Set debug options for MSVC
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/O2 /Ob2 /DNDEBUG")
    
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        message("Detected clang-cl")

        # Explicitly enable exceptions support for clang-cl (it's only enabled by CMake when targeting the Windows-MSVC platform,
        # see https://github.com/danvratil/qcoro/issues/90 for details)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc")

        # CMake doesn't seem to set optimization flags correctly for clang-cl, so we set them manually.
        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2 /DNDEBUG")
    endif()
endif()

enable_testing()

# Include sub-projects.
add_subdirectory("src")
