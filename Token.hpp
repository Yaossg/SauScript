#pragma once

#include "TypeSystem.hpp"
#include "Keyword.hpp"

namespace SauScript {

// tokens

enum class TokenType {
    PUNCTUATOR, IDENTIFIER, KEYWORD, LINEBREAK,
    LITERAL_BOOL, LITERAL_INT, LITERAL_REAL, BRACE,

    TERMINATOR
};

struct Token {
    TokenType type;
    std::variant<int, std::string, int_t, real_t> parameter;
    SourceLocation location;
    bool operator ==(Token const& other) const {
        return type == other.type && parameter == other.parameter;
    }
    Token& at(SourceLocation where) {
        location = where;
        return *this;
    }
    [[nodiscard]] Keyword keyword() const {
        return Keyword(std::get<int>(parameter));
    }
    [[nodiscard]] std::string identifier() const {
        return std::get<std::string>(parameter);
    }
    [[nodiscard]] std::string punctuator() const {
        return std::get<std::string>(parameter);
    }
    [[nodiscard]] bool literal_bool() const {
        return std::get<int>(parameter);
    }
    [[nodiscard]] int_t literal_int() const {
        return std::get<int_t>(parameter);
    }
    [[nodiscard]] real_t literal_real() const {
        return std::get<real_t>(parameter);
    }
    [[nodiscard]] Type parseType() const {
        if (type != TokenType::IDENTIFIER) syntax("expected type name", location);
        std::string name = identifier();
        if (TYPES.contains(name))
            return TYPES[name];
        syntax("invalid type name", location);
    }

    static Token punctuator(std::string p) {
        return {TokenType::PUNCTUATOR, p};
    }
    static Token identifier(std::string id) {
        return {TokenType::IDENTIFIER, id};
    }
    static Token literal_bool(bool x) {
        return {TokenType::LITERAL_BOOL, x};
    }
    static Token literal_int(int_t x) {
        return {TokenType::LITERAL_INT, x};
    }
    static Token literal_real(real_t x) {
        return {TokenType::LITERAL_REAL, x};
    }
    static Token keyword(Keyword keyword) {
        return {TokenType::KEYWORD, int(keyword)};
    }
    static Token braceLeft() {
        return {TokenType::BRACE, 0};
    }
    static Token braceRight() {
        return {TokenType::BRACE, 1};
    }
    static Token parenLeft() {
        return {TokenType::BRACE, 2};
    }
    static Token parenRight() {
        return {TokenType::BRACE, 3};
    }
    static Token bracketLeft() {
        return {TokenType::BRACE, 4};
    }
    static Token bracketRight() {
        return {TokenType::BRACE, 5};
    }
    static Token linebreak() {
        return {TokenType::LINEBREAK, 0};
    }
    static Token terminator() {
        return {TokenType::TERMINATOR, 0};
    }
};

// source code & tokenizer

struct SourceCode {
    std::string raw;
    std::vector<std::string> lines;
    std::vector<Token> tokens;

    explicit SourceCode(std::string raw);

private:
    [[nodiscard]] int column() const {
        return int(current - start + 1);
    }

    [[nodiscard]] SourceLocation location() const {
        return {this, line, column()};
    }

    void tokenize();

private:
    const char* current;
    const char* start;
    int line;

    bool skipLineBreak(bool strict);
    Token parseNumber();
    Token parseIdentifier();
};

}