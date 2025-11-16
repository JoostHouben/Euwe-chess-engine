if (NOT USE_PGO)

	# == Standard target ==

	add_executable(Euwe "Main.cpp")
	# C++23 standard
	set_property(TARGET Euwe PROPERTY CXX_STANDARD 23)
	# Statically link the runtime libraries.
	set_property(TARGET Euwe PROPERTY
	  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
	# Link the statically linked runtime version of the chess engine library.
	target_link_libraries(Euwe chess-engine-lib-stat-crt)

else() # USE_PGO

	# == Profile generation target ==

	add_executable(Euwe-ProfileGen "Main.cpp")
	# C++23 standard
	set_property(TARGET Euwe-ProfileGen PROPERTY CXX_STANDARD 23)
	# Statically link the runtime libraries.
	set_property(TARGET Euwe-ProfileGen PROPERTY
	  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
	target_link_libraries(Euwe-ProfileGen chess-engine-lib-pgo-gen)

	target_compile_options(Euwe-ProfileGen PRIVATE -fprofile-generate)
	target_link_options(Euwe-ProfileGen PRIVATE -fprofile-generate)

	# == Profile data collection target ==

	add_custom_target(Euwe-PGO-Collect
		COMMAND ${CMAKE_COMMAND} -E env LLVM_PROFILE_FILE=$<TARGET_FILE_DIR:Euwe-ProfileGen>/../euwe.profraw
			$<TARGET_FILE:Euwe-ProfileGen> uci "setoption name hash value 512" "position fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" "go depth 17" isready quit
		COMMENT "Running Euwe-ProfileGen to collect PGO data"
	)

	# == Profile data merge target ==

	add_custom_target(Euwe-PGO-Merge
		COMMAND ${PROFDATA_CMD} merge -output=$<TARGET_FILE_DIR:Euwe-ProfileGen>/../euwe.profdata $<TARGET_FILE_DIR:Euwe-ProfileGen>/../euwe.profraw
		DEPENDS Euwe-PGO-Collect
		COMMENT "Merging PGO data into euwe.profdata"
	)

	# == PGO-optimized target ==

	add_executable(Euwe "Main.cpp")
	add_dependencies(Euwe Euwe-PGO-Merge)
	# C++23 standard
	set_property(TARGET Euwe PROPERTY CXX_STANDARD 23)
	# Statically link the runtime libraries.
	set_property(TARGET Euwe PROPERTY
	  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
	target_link_libraries(Euwe chess-engine-lib-pgo)

	target_compile_options(Euwe
		PRIVATE -fprofile-use=$<TARGET_FILE_DIR:Euwe-ProfileGen>/../euwe.profdata)

endif()
