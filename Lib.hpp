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

[[nodiscard]] inline std::vector<std::string> splitLines(std::string raw) {
    std::vector<std::string> lines;
    std::string line;
    for (const char* current = raw.data(); *current; ++current) {
        if (*current == '\n') {
            lines.push_back(line);
            line.clear();
        } else {
            line.push_back(*current);
        }
    }
    lines.push_back(line);
    return lines;
}

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

struct SourceCode;

struct SourceLocation {
    const SourceCode* code = nullptr;
    int line = 0, column = 0;
    [[nodiscard]] std::string at() const;
};

struct PlainRuntimeError : RuntimeError {
    PlainRuntimeError(std::string const& msg): RuntimeError(msg) {}

    [[noreturn]] void rethrow(SourceLocation location) const {
        throw RuntimeError(what() + location.at());
    }
};

}