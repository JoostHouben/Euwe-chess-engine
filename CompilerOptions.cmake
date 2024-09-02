# log all *_INIT variables
get_cmake_property(_varNames VARIABLES)
list (REMOVE_DUPLICATES _varNames)
list (SORT _varNames)
foreach (_varName ${_varNames})
    if (_varName MATCHES "_INIT$")
        message(STATUS "${_varName}=${${_varName}}")
    endif()
endforeach()

if (MSVC)
    # Remove the /MD and /MDd flags so that we can control the linking of the runtime library
    # explicitly using MSVC_RUNTIME_LIBRARY.
    string (REGEX REPLACE "/MD\\b" "" CMAKE_CXX_FLAGS_INIT "${CMAKE_CXX_FLAGS_INIT}")
    string (REGEX REPLACE "/MD\\b" "" CMAKE_CXX_FLAGS_RELEASE_INIT "${CMAKE_CXX_FLAGS_RELEASE_INIT}")
    string (REGEX REPLACE "/MDd\\b" "" CMAKE_CXX_FLAGS_DEBUG_INIT "${CMAKE_CXX_FLAGS_DEBUG_INIT}")
endif()
