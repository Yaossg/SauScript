cmake_minimum_required(VERSION 3.21)
project(SauScript)

set(CMAKE_CXX_FLAGS "-static")

set(CMAKE_CXX_STANDARD 20)

add_executable(SauScript Node.hpp Engine.cpp Main.cpp Environment.cpp Lib.hpp TypeSystem.hpp Keyword.hpp Operator.hpp Engine.hpp Token.hpp TypeSystem.cpp Operator.cpp SauScript.hpp Unicode.hpp Unicode.cpp Token.cpp)
