add_library(chess-engine-lib STATIC
    "BitBoard.cpp" 
    "BoardHash.cpp" 
    "ConsoleColor.cpp" 
    "Engine.cpp" 
    "Eval.cpp" 
    "EvalT.cpp" 
    "FrontEndOption.cpp" 
    "GameState.cpp" 
    "GameStateStringOps.cpp" 
    "Move.cpp" 
    "MoveOrder.cpp" 
    "MoveSearcher.cpp" 
    "PawnMasks.cpp" 
    "Perft.cpp" 
    "Piece.cpp" 
    "PieceControl.cpp" 
    "SEE.cpp" 
    "Side.cpp" 
    "TimeManager.cpp" 
    "UciFrontEnd.cpp"
)

get_filename_component(PARENT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../ ABSOLUTE)
target_include_directories(chess-engine-lib PUBLIC ${PARENT_DIR})

set_property(TARGET chess-engine-lib PROPERTY CXX_STANDARD 23)

# Statically link the runtime libraries.
set_property(TARGET chess-engine-lib PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
