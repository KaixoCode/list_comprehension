
# ==============================================

cmake_minimum_required(VERSION 3.0)
project(list_comprehension)

# ==============================================

set (CMAKE_CXX_STANDARD 23)

# ==============================================

option(BUILD_LIST_COMPREHENSION_EXAMPLE "Build the list comprehension example project" false)
option(BUILD_LIST_COMPREHENSION_TEST "Build the list comprehension test project" false)

# ==============================================

include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include/")

# ==============================================

if (${BUILD_LIST_COMPREHENSION_EXAMPLE})
add_executable(list_comprehension_example "${CMAKE_CURRENT_SOURCE_DIR}/example/example.cpp")
endif()

# ==============================================

if (${BUILD_LIST_COMPREHENSION_TEST})
add_executable(list_comprehension_test "${list_comprehension_SOURCE_DIR}/test/test.cpp")
endif()

# ==============================================
