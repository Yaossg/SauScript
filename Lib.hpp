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

struct SourceCode;

struct SourceLocation {
    const SourceCode* code = nullptr;
    int line = 0, column = 0;
    [[nodiscard]] std::string what() const;
};

enum class ErrorType {
    Syntax, Runtime
};

struct Error : std::exception {
    ErrorType type;
    std::string message;
    SourceLocation location;
    std::string descriptor;
    Error(ErrorType type, std::string message, SourceLocation location)
            : type(type), message(std::move(message)), location(location),
            descriptor(std::string(type == ErrorType::Syntax ? "Syntax" : "Runtime") + " Error: " + this->message +
                               location.what()) {}
    [[nodiscard]] const char* what() const noexcept override {
        return descriptor.c_str();
    }
};

struct RawError {
    ErrorType type;
    std::string message;
    [[noreturn]] void rethrow(SourceLocation location) const {
        throw Error(type, message, location);
    }
    [[noreturn]] void rethrowAsSyntaxError(SourceLocation location) const {
        throw Error(ErrorType::Syntax, message, location);
    }
};

[[noreturn]] inline void syntax(std::string const& message) {
    throw RawError{ErrorType::Syntax, message};
}

[[noreturn]] inline void syntax(std::string const& message, SourceLocation location) {
    throw Error(ErrorType::Syntax, message, location);
}

[[noreturn]] inline void runtime(std::string const& message) {
    throw RawError{ErrorType::Runtime, message};
}

[[noreturn]] inline void runtime(std::string const& message, SourceLocation location) {
    throw Error(ErrorType::Runtime, message, location);
}

[[noreturn]] inline void impossible() {
    std::fprintf(stderr, "Assertion failed");
    std::terminate();
}

}