#pragma once

#include "TypeSystem.hpp"

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
    int line = 0;
    bool operator ==(Token const& other) const {
        return type == other.type && parameter == other.parameter;
    }
    Token& at(int where) {
        line = where;
        return *this;
    }
    [[nodiscard]] std::string at() const {
        return SauScript::at(line);
    }
    [[nodiscard]] int keyword() const {
        return std::get<int>(parameter);
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
    [[nodiscard]] Type parseType() const;

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
    static Token keyword(int keyword) {
        return {TokenType::KEYWORD, keyword};
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

[[nodiscard]] std::vector<Token> tokenize(char const* current);

}