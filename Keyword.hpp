#pragma once

#include "Lib.hpp"

namespace SauScript::Keyword {
const std::string_view KW_TOKENS[] =
        {"while", "if", "else", "try", "catch", "for", "fn"};
enum {
    WHILE, IF, ELSE, TRY, CATCH, FOR, FN,

    NAK // not a keyword
};

inline int parse(std::string const& kw) {
    using namespace Keyword;
    auto first = std::begin(KW_TOKENS), last = std::end(KW_TOKENS);
    return int(std::find(first, last, kw) - first);
}
}