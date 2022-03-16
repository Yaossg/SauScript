#pragma once

#include <functional>
#include <variant>

#include "Diagnostics.hpp"

namespace SauScript::Operators {

constexpr int LEVEL_ROOT = 0;
constexpr int LEVEL_PREFIX = 12;
constexpr int LEVEL_POSTFIX = 13;
constexpr int LEVEL_PRIMARY = 14;

struct Operator {
    std::string_view literal;

    [[nodiscard]] bool isRootUnary() const {
        return std::isalpha(literal[0]);
    }
};

extern const std::vector<Operator> OPERATORS[14];

inline Operator const* find(std::string const& literal, int level) {
    auto first = OPERATORS[level].begin(), last = OPERATORS[level].end();
    auto result = std::find_if(first, last, [&literal](Operator const& op) { return op.literal == literal; });
    return result != last ? &*result : nullptr;
}

static thread_local std::vector<std::string_view> TOKENS;

inline std::vector<std::string_view> const& tokens() {
    if (TOKENS.empty()) {
        TOKENS = {"?", ":", ",", ".", "@"};
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