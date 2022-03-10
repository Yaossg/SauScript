#pragma once

#include <string>
#include <stdexcept>
#include <memory>

#include "Forward.hpp"

namespace SauScript {

struct SourceLocation {
    std::shared_ptr<const SourceCode> code = nullptr;
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
            : type(type), message(std::move(message)), location(std::move(location)),
            descriptor(std::string(type == ErrorType::Syntax ? "Syntax" : "Runtime") + " Error: " + this->message + this->location.what()) {}
    [[nodiscard]] const char* what() const noexcept override {
        return descriptor.c_str();
    }
};

struct RawError {
    ErrorType type;
    std::string message;
    [[noreturn]] void rethrow(SourceLocation location) const {
        throw Error(type, message, std::move(location));
    }
    [[noreturn]] void rethrowAsSyntaxError(SourceLocation location) const {
        throw Error(ErrorType::Syntax, message, std::move(location));
    }
};

[[noreturn]] inline void syntax(std::string const& message) {
    throw RawError{ErrorType::Syntax, message};
}

[[noreturn]] inline void syntax(std::string const& message, SourceLocation location) {
    throw Error(ErrorType::Syntax, message, std::move(location));
}

[[noreturn]] inline void runtime(std::string const& message) {
    throw RawError{ErrorType::Runtime, message};
}

[[noreturn]] inline void runtime(std::string const& message, SourceLocation location) {
    throw Error(ErrorType::Runtime, message, std::move(location));
}

[[noreturn]] inline void impossible() {
    std::fprintf(stderr, "Assertion failed: Impossible to reach here\n");
    std::terminate();
}

}