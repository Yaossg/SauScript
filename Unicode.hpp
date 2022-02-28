#pragma once

#include <cctype>
#include <functional>

#include "Lib.hpp"

namespace SauScript::Unicode {

[[nodiscard]] std::string encodeUnicode(char32_t unicode);
[[nodiscard]] char32_t decodeUnicode(const char*& current);
[[nodiscard]] int decodeUnicode(std::function<int()> const& current);
[[nodiscard]] char32_t unquoteCharacter(const char*& current);
[[nodiscard]] std::string unquoteString(const char*& current);
[[nodiscard]] std::string quote(char32_t unicode);
[[nodiscard]] std::string quote(std::string string);

}