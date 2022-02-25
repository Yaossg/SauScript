#pragma once

#include "Lib.hpp"

namespace SauScript::Operators {

// 0 io, parameterized jump, assignment, ternary
// 1 logical or
// 2 logical and
// 3 bit or
// 4 bit xor
// 5 bit and
// 6 equality
// 7 inequality
// 8 shift
// 9 addition
// 10 multiplication
// 11 prefix
// 12 postfix
// 13 primary expression

constexpr int LEVEL_ROOT = 0;
constexpr int LEVEL_PREFIX = 11;
constexpr int LEVEL_POSTFIX = 12;
constexpr int LEVEL_PRIMARY = 13;

struct Operator {
    using Unary = std::function<void(ExprNode*)>;
    using Binary = std::function<void(ExprNode*, ExprNode*)>;
    using Fn = std::variant<Unary, Binary>;

    std::string_view literal;
    Fn fn;

    [[nodiscard]] Unary const& asUnary() const { return std::get<Unary>(fn); }
    [[nodiscard]] Binary const& asBinary() const { return std::get<Binary>(fn); }
    [[nodiscard]] bool isBinary() const { return fn.index(); }
};

extern const std::vector<Operator> OPERATORS[13];

inline Operator const* find(std::string const& literal, int level) {
    auto first = OPERATORS[level].begin(), last = OPERATORS[level].end();
    auto result = std::find_if(first, last, [&literal](Operator const& op) { return op.literal == literal; });
    return result != last ? &*result : nullptr;
}

static thread_local std::vector<std::string_view> TOKENS;

inline std::vector<std::string_view> const& tokens() {
    if (TOKENS.empty()) {
        TOKENS = {"?", ":", ",", "."};
        for (auto&& ops : OPERATORS) {
            for (auto&& op : ops) {
                TOKENS.push_back(op.literal);
            }
        }
    }
    return TOKENS;
}
}

namespace SauScript {
using Operators::Operator;
}