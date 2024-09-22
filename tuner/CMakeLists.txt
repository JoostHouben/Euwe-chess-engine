find_package(Ceres REQUIRED)

add_executable(Tuner
    "LoadPositions.cpp"
    "Main.cpp"
    "Optimization.cpp"
    "PreProcessing.cpp"
)

# C++23 standard
set_property(TARGET Tuner PROPERTY CXX_STANDARD 23)

# Statically link the runtime libraries.
set_property(TARGET Tuner PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")

target_link_libraries(Tuner
    Ceres::ceres
    chess-engine-lib
)
