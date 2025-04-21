# Include sub-projects.
add_subdirectory("chess-engine-lib")

if ("engine" IN_LIST TARGETS_TO_BUILD)
    add_subdirectory("chess-engine")
endif()

if ("tests" IN_LIST TARGETS_TO_BUILD)
    add_subdirectory("tests")
endif()

if ("tuner" IN_LIST TARGETS_TO_BUILD)
    add_subdirectory("tuner")
endif()
