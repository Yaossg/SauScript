#pragma once

#include <string>
#include <stdexcept>

namespace SauScript {

// utilities

template<typename... Ts>
struct overloaded : Ts... {
    explicit overloaded(Ts... ts): Ts(ts)... {}
    using Ts::operator()...;
};

// forward declarations

struct Token;
struct Function;
struct Object;
struct ScriptEngine;
struct ExprNode;
struct StmtsNode;

// diagnostics

struct SyntaxError : std::logic_error {
    SyntaxError(std::string const& msg): std::logic_error(msg) {}
};

struct RuntimeError : std::runtime_error {
    RuntimeError(std::string const& msg): std::runtime_error(msg) {}
};

inline std::string at(int line) {
    return line > 0 ? " at line " +  std::to_string(line) : " at unknown line";
}

}