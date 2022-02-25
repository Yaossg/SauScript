cmake_minimum_required(VERSION 3.21)
project(SauScript)

set(CMAKE_CXX_FLAGS "-static")

set(CMAKE_CXX_STANDARD 20)

add_executable(SauScript Node.hpp Engine.cpp Main.cpp Environment.cpp Lib.hpp TypeSystem.hpp Keywords.hpp Operators.hpp Engine.hpp Token.hpp TypeSystem.cpp Operators.cpp SauScript.hpp)
