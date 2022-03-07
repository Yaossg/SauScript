#pragma once

#include <cctype>
#include <functional>

#include "Diagnostics.hpp"

namespace SauScript::Unicode {

[[nodiscard]] inline bool isASCII(char32_t ch) {
    return ch <= 0x7F;
}
[[nodiscard]] inline bool isSurrogate(char32_t ch) {
    return 0xD800 <= ch && ch <= 0xDFFF;
}
[[nodiscard]] inline bool isUnicode(char32_t ch) {
    return ch <= 0x10FFFF && !isSurrogate(ch);
}

[[nodiscard]] std::string encodeUnicode(char32_t unicode);
[[nodiscard]] std::string encodeUnicode(std::u32string const& utf32);
[[nodiscard]] char32_t decodeUnicode(const char*& current);
[[nodiscard]] int decodeUnicode(std::function<int()> const& current);
[[nodiscard]] std::u32string decodeUnicode(std::string const& utf8);
[[nodiscard]] char32_t unquoteCharacter(const char*& current);
[[nodiscard]] std::string unquoteString(const char*& current);
[[nodiscard]] std::string quote(char32_t unicode);
[[nodiscard]] std::string quote(std::string const& string);
[[nodiscard]] int fgetc(FILE* in);
[[nodiscard]] std::string fgets(FILE* in);
[[nodiscard]] std::string fgets(FILE* in, std::function<bool(char32_t)> const& until);

}