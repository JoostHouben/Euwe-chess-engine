# == Standard target ==

add_executable(Euwe "Main.cpp")
# C++23 standard
set_property(TARGET Euwe PROPERTY CXX_STANDARD 23)
# Statically link the runtime libraries.
set_property(TARGET Euwe PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
# Link the statically linked runtime version of the chess engine library.
target_link_libraries(Euwe chess-engine-lib-stat-crt)

## == PGO targets ==
if (ENABLE_PGO AND (GCC_LIKE OR CLANG_CL))
	# == Profile generation target ==

	add_executable(Euwe-ProfileGen "Main.cpp")
	# C++23 standard
	set_property(TARGET Euwe-ProfileGen PROPERTY CXX_STANDARD 23)
	# Statically link the runtime libraries.
	set_property(TARGET Euwe-ProfileGen PROPERTY
	  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
	target_link_libraries(Euwe-ProfileGen chess-engine-lib-pgo-gen)

	target_compile_options(Euwe-ProfileGen PRIVATE -fprofile-generate)

	# == Profile use target ==

	add_executable(Euwe-PGO "Main.cpp")
	# C++23 standard
	set_property(TARGET Euwe-PGO PROPERTY CXX_STANDARD 23)
	# Statically link the runtime libraries.
	set_property(TARGET Euwe-PGO PROPERTY
	  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
	target_link_libraries(Euwe-PGO chess-engine-lib-pgo)

	target_compile_options(Euwe-PGO
		PRIVATE -fprofile-use=$<TARGET_FILE_DIR:Euwe-ProfileGen>/../euwe.profdata)
endif()
