cmake_minimum_required(VERSION 3.21)
project(SauScript)

set(CMAKE_CXX_STANDARD 20)

add_executable(SauScript SauScript.hpp SauScript.cpp Main.cpp)
