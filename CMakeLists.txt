cmake_minimum_required (VERSION 3.0)
project (list_comprehension)

set (CMAKE_CXX_STANDARD 23)

add_subdirectory(libs)

set(INCLUDES
  ${list_comprehension_SOURCE_DIR}/include/
  ${type_utils_SOURCE_DIR}/include/
)

add_executable(list_comprehension_example
  "${list_comprehension_SOURCE_DIR}/example.cpp"
)

add_executable(list_comprehension_tests
  "${list_comprehension_SOURCE_DIR}/tests.cpp"
)

target_include_directories(list_comprehension_example PRIVATE
  ${INCLUDES}
)

target_include_directories(list_comprehension_tests PRIVATE
  ${INCLUDES}
)
