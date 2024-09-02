add_executable(Euwe "Main.cpp")

# C++23 standard
set_property(TARGET Euwe PROPERTY CXX_STANDARD 23)

# Statically link the runtime libraries.
set_property(TARGET Euwe PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

target_link_libraries(Euwe chess-engine-lib)
