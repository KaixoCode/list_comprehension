cmake_minimum_required (VERSION 3.0)
project (list_comprehension)

set (CMAKE_CXX_STANDARD 20)

include_directories(
  /
)

add_library(list_comprehension STATIC
  "${list_comprehension_SOURCE_DIR}/list_comprehension.hpp"
)

set_target_properties(list_comprehension PROPERTIES LINKER_LANGUAGE CXX)


add_executable(list_comprehension_example
  "${list_comprehension_SOURCE_DIR}/example.cpp"
)

add_executable(list_comprehension_tests
  "${list_comprehension_SOURCE_DIR}/tests.cpp"
)