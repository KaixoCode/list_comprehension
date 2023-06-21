cmake_minimum_required (VERSION 3.0)
project (list_comprehension)

set (CMAKE_CXX_STANDARD 23)

add_subdirectory(libs)

include_directories(${INCLUDES})

option(BUILD_LIST_COMPREHENSION_EXAMPLE "Build the list comprehension example project" false)

if (${BUILD_LIST_COMPREHENSION_EXAMPLE})
if (TARGET list_comprehension_example)
else()
add_executable(list_comprehension_example
  ${list_comprehension_SOURCE_DIR}/example.cpp
)

target_include_directories(list_comprehension_example PRIVATE
  ${list_comprehension_SOURCE_DIR}/include/
  ${type_utils_SOURCE_DIR}/include/
)
endif()
endif()