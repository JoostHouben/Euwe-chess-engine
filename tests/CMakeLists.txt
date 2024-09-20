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
)

# C++23 standard
set_property(TARGET tests PROPERTY CXX_STANDARD 23)

# Statically link the runtime libraries.
set_property(TARGET tests PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")

target_link_libraries(tests
    GTest::GTest
    GTest::Main
    chess-engine-lib
)

gtest_discover_tests(tests)