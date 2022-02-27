#pragma once

#include <cctype>
#include <functional>

#include "Lib.hpp"

namespace SauScript::Unicode {

[[nodiscard]] std::string encodeUnicode(char32_t unicode, int line);
[[nodiscard]] char32_t decodeUnicode(const char*& current, int line);
[[nodiscard]] int decodeUnicode(std::function<int()> current, int line = 0);
[[nodiscard]] char32_t unquoteCharacter(const char*& current, int line);
[[nodiscard]] std::string unquoteString(const char*& current, int line);
[[nodiscard]] std::string quote(char32_t unicode, int line);
[[nodiscard]] std::string quote(std::string string, int line);

}