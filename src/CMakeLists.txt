cmake_minimum_required (VERSION 2.6)
project (tako)

set (tako_VERSION_MAJOR 1)
set (tako_VERSION_MINOR 0)
set (tako_VERSION_PATCH 0)

# configure a header file to pass some of the CMake settings
# to the source code
configure_file (
  "${PROJECT_SOURCE_DIR}/takoConfig.h.in"
  "${PROJECT_BINARY_DIR}/takoConfig.h"
  )

# configure flags for GXX
set(CMAKE_CXX_STANDARD 14)
set(GXX_COVERAGE_COMPILE_FLAGS "-g")
set(GXX_COVERAGE_LINK_FLAGS    "-g -rdynamic")

set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GXX_COVERAGE_COMPILE_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GXX_COVERAGE_LINK_FLAGS}")

# add the binary tree to the search path for include files
# so that we will find TutorialConfig.h
include_directories("${PROJECT_BINARY_DIR}")

# include our deps
add_subdirectory("lib/rapidcheck")

# include our libraries
include_directories ("${PROJECT_SOURCE_DIR}/util")
add_subdirectory (util)
include_directories ("${PROJECT_SOURCE_DIR}/arg_parser")
add_subdirectory (arg_parser)
include_directories ("${PROJECT_SOURCE_DIR}/parser")
add_subdirectory (parser)

# add the executable
add_executable(tako main.cc)
target_link_libraries (tako util)
target_link_libraries (tako parser)
target_link_libraries (tako arg_parser)

# add the executable
add_executable(takoTest test.cc)
target_link_libraries(takoTest rapidcheck)
target_link_libraries (takoTest util)
target_link_libraries (takoTest parser)
