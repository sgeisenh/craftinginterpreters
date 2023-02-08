cmake_minimum_required(VERSION 3.22.1)
project(cpplox)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_EXPORT_COMPILE_COMMANDS True)
set(CMAKE_BUILD_TYPE RelWithDebInfo)

add_subdirectory(abseil-cpp)

include_directories(.)

add_executable(cpplox
  main.cc 
  compiler.cc 
  intern.cc 
  memory.cc
  scanner.cc 
  value.cc 
  vm.cc
)

target_link_libraries(cpplox absl::strings absl::str_format absl::flat_hash_map absl::flat_hash_set)
