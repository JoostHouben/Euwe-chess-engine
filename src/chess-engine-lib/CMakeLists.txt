set(
    CHESS_ENGINE_LIB_SOURCES
    "BitBoard.cpp"
    "BoardHash.cpp"
    "ConsoleColor.cpp"
    "Engine.cpp"
    "Eval.cpp"
    "EvalParams.cpp"
    "EvalT.cpp"
    "FrontEndOption.cpp"
    "GameState.cpp"
    "GameStateStringOps.cpp"
    "Move.cpp"
    "MoveOrdering.cpp"
    "MoveSearcher.cpp"
    "PawnMasks.cpp"
    "Perft.cpp"
    "Piece.cpp"
    "PieceControl.cpp"
    "SEE.cpp"
    "Side.cpp"
    "Syzygy.cpp"
    "TimeManager.cpp"
    "UciFrontEnd.cpp"

    "Pyrrhic/tbprobe.cpp"
)

get_filename_component(PARENT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../ ABSOLUTE)

if (MSVC)
    # == Library with statically linked CRT. ==
    add_library(chess-engine-lib-stat-crt STATIC ${CHESS_ENGINE_LIB_SOURCES})
    target_include_directories(chess-engine-lib-stat-crt PUBLIC ${PARENT_DIR})
    set_property(TARGET chess-engine-lib-stat-crt PROPERTY CXX_STANDARD 23)

    # Statically link the runtime libraries.
    set_property(TARGET chess-engine-lib-stat-crt PROPERTY
      MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

    # == Library with dynamically linked CRT. ==
    add_library(chess-engine-lib-dyn-crt STATIC ${CHESS_ENGINE_LIB_SOURCES})
    target_include_directories(chess-engine-lib-dyn-crt PUBLIC ${PARENT_DIR})
    set_property(TARGET chess-engine-lib-dyn-crt PROPERTY CXX_STANDARD 23)

    # Dynamically link the runtime libraries.
    set_property(TARGET chess-engine-lib-dyn-crt PROPERTY
      MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")
else()
    # For non-MSVC builds, make static / dynamically linked CRT versions aliases of the same
    # underlying target.

    add_library(chess-engine-lib STATIC ${CHESS_ENGINE_LIB_SOURCES})
    target_include_directories(chess-engine-lib PUBLIC ${PARENT_DIR})
    set_property(TARGET chess-engine-lib PROPERTY CXX_STANDARD 23)

    add_library(chess-engine-lib-stat-crt ALIAS chess-engine-lib)
    add_library(chess-engine-lib-dyn-crt ALIAS chess-engine-lib)
endif()
