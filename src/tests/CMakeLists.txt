find_package(GTest REQUIRED)
include(GoogleTest)

add_executable(tests
    "BitBoardTests.cpp"
    "BoardPositionTests.cpp"
    "FenParsingTests.cpp"
    "FrontEndOptionTests.cpp"
    "GameStateHelpersTests.cpp"
    "GameStateTests.cpp"
    "HashingTests.cpp"
    "MoveGenerationTests.cpp"
    "MoveTests.cpp"
    "PieceTests.cpp"
    "SEETests.cpp"
    "StackOfVectorsTests.cpp"
    "EvalJacobiansTests.cpp")

# C++23 standard
set_property(TARGET tests PROPERTY CXX_STANDARD 23)

# Dynamically link the runtime libraries.
set_property(TARGET tests PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")

target_link_libraries(tests
    GTest::GTest
    GTest::Main
    chess-engine-lib-dyn-crt
)

# We disable test discovery for UBSAN. Since we run UBSAN with -fsanitize-trap=all, the test binary
# may crash, even when just discovering tests. This causes the build to fail.
# We don't want the build to fail, just running the tests. So in this scenario we disable test
# discovery.
if (NOT DISABLE_TEST_DISCOVERY)
    gtest_discover_tests(tests)
endif()