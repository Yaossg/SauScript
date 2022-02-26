#pragma once

#include "Lib.hpp"

namespace SauScript::Keyword {
const std::string_view KW_TOKENS[] =
        {"while", "do", "if", "else", "try", "catch", "break", "continue", "for", "fn"};
enum {
    WHILE, DO, IF, ELSE, TRY, CATCH, BREAK, CONTINUE, FOR, FN,

    NAK // not a keyword
};

inline int parse(std::string const& kw) {
    using namespace Keyword;
    auto first = std::begin(KW_TOKENS), last = std::end(KW_TOKENS);
    return std::find(first, last, kw) - first;
}
}