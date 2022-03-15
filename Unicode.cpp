#include <bit>

#include "Unicode.hpp"

namespace SauScript::Unicode {

namespace Diagnostics {

[[noreturn]] void malformedASCII() { runtime("Malformed hexadecimal ASCII"); }
[[noreturn]] void malformedUnicode() { runtime("Malformed hexadecimal Unicode"); }
[[noreturn]] void malformedLiteral() { runtime("Malformed text literal"); }
[[noreturn]] void surrogate() { runtime("UTF-16 Surrogate code not allowed"); }
[[noreturn]] void terminate() { runtime("UTF-8 series terminated accidentally"); }
[[noreturn]] void overflow() { runtime("Unicode value overflowed"); }

}

char32_t hex(char8_t ch) {
    if ('0' <= ch && ch <= '9') return ch - '0';
    if ('a' <= ch && ch <= 'f') return ch - 'a' + 10;
    if ('A' <= ch && ch <= 'F') return ch - 'A' + 10;
    impossible();
}

char parseHexASCII(const char*& current) {
    char8_t ch1 = *++current;
    if (!('0' <= ch1 && ch1 < '8')) Diagnostics::malformedASCII();
    char8_t ch2 = *++current;
    if (!std::isxdigit(ch2)) Diagnostics::malformedASCII();
    return char((hex(ch1) << 4) | hex(ch2));
}

char32_t parseHexUnicode(const char*& current) {
    char8_t ch1 = *++current;
    if (ch1 != '0' && ch1 != '1') Diagnostics::malformedUnicode();
    char8_t ch2 = *++current;
    if (!std::isxdigit(ch2) || ch1 == '1' && ch2 != '0') Diagnostics::malformedUnicode();
    char8_t ch3 = *++current;
    if (!std::isxdigit(ch3)) Diagnostics::malformedUnicode();
    char8_t ch4 = *++current;
    if (!std::isxdigit(ch4)) Diagnostics::malformedUnicode();
    char8_t ch5 = *++current;
    if (!std::isxdigit(ch5)) Diagnostics::malformedUnicode();
    char8_t ch6 = *++current;
    if (!std::isxdigit(ch6)) Diagnostics::malformedUnicode();
    char32_t result = (hex(ch1) << 20) | (hex(ch2) << 16) | (hex(ch3) << 12) | (hex(ch4) << 8) | (hex(ch5) << 4) | hex(ch6);
    if (isSurrogate(result)) Diagnostics::surrogate();
    return result;
}

int bytes(char8_t byte) {
    switch(int ones = std::countl_one((unsigned char) byte)) {
        case 0:
            return 1;
        case 2:
        case 3:
        case 4:
            return ones;
        case 1:
        default:
            Diagnostics::overflow();
    }
}

std::string encodeUnicode(char32_t unicode) {
    if (isSurrogate(unicode)) Diagnostics::surrogate();
    if (unicode <= 0x7F) { // ASCII
        return {char(unicode)};
    } else if (unicode <= 0x7FF) { // 2 bytes
        return {char((unicode >> 6) | 0xC0),
                char((unicode & 0x3F) | 0x80)};
    } else if (unicode <= 0xFFFF) { // 3 bytes
        return {char((unicode >> 12) | 0xE0),
                char(((unicode >> 6) & 0x3F) | 0x80),
                char((unicode & 0x3F) | 0x80)};
    } else if (unicode <= 0x10FFFF) { // 4 bytes
        return {char((unicode >> 18) | 0xF0),
                char(((unicode >> 12) & 0x3F) | 0x80),
                char(((unicode >> 6) & 0x3F) | 0x80),
                char((unicode & 0x3F) | 0x80)};
    } else {
        Diagnostics::overflow();
    }
}

[[nodiscard]] std::string encodeUnicode(std::u32string const& utf32) {
    std::string utf8;
    for (char32_t ch : utf32)
        utf8 += Unicode::encodeUnicode(ch);
    return utf8;
}

char32_t decodeUnicode(const char*& current) {
    char32_t result;
    char8_t ch1 = *current;
    switch (bytes(ch1)) {
        case 1:
            result = ch1;
            break;
        case 2: {
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) Diagnostics::terminate();
            result = ((ch1 & ~0xC0) << 6) | (ch2 & ~0x80);
        } break;
        case 3: {
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) Diagnostics::terminate();
            char8_t ch3 = *++current;
            if (ch3 >> 6 != 0b10) Diagnostics::terminate();
            result = ((ch1 & ~0xE0) << 12) | ((ch2 & ~0x80) << 6) | (ch3 & ~0x80);
        } break;
        case 4: {
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) Diagnostics::terminate();
            char8_t ch3 = *++current;
            if (ch3 >> 6 != 0b10) Diagnostics::terminate();
            char8_t ch4 = *++current;
            if (ch4 >> 6 != 0b10) Diagnostics::terminate();
            result = ((ch1 & ~0xF0) << 18) | ((ch2 & ~0x80) << 12) | ((ch3 & ~0x80) << 6) | (ch4 & ~0x80);
        } break;
    }
    if (isSurrogate(result)) Diagnostics::surrogate();
    return result;
}

int decodeUnicode(std::function<int()> const& current) {
    char32_t result;
    int ch1 = current();
    if (ch1 == EOF) return EOF;
    switch (bytes(ch1)) {
        case 1:
            result = ch1;
            break;
        case 2: {
            int ch2 = current();
            if (ch2 == EOF || ch2 >> 6 != 0b10) Diagnostics::terminate();
            result = ((ch1 & ~0xC0) << 6) | (ch2 & ~0x80);
        } break;
        case 3: {
            int ch2 = current();
            if (ch2 == EOF || ch2 >> 6 != 0b10) Diagnostics::terminate();
            int ch3 = current();
            if (ch3 == EOF || ch3 >> 6 != 0b10) Diagnostics::terminate();
            result = ((ch1 & ~0xE0) << 12) | ((ch2 & ~0x80) << 6) | (ch3 & ~0x80);
        } break;
        case 4: {
            int ch2 = current();
            if (ch2 == EOF || ch2 >> 6 != 0b10) Diagnostics::terminate();
            int ch3 = current();
            if (ch3 == EOF || ch3 >> 6 != 0b10) Diagnostics::terminate();
            int ch4 = current();
            if (ch4 == EOF || ch4 >> 6 != 0b10) Diagnostics::terminate();
            result = ((ch1 & ~0xF0) << 18) | ((ch2 & ~0x80) << 12) | ((ch3 & ~0x80) << 6) | (ch4 & ~0x80);
        } break;
    }
    if (isSurrogate(result)) Diagnostics::surrogate();
    return result;
}

[[nodiscard]] std::u32string decodeUnicode(std::string const& utf8) {
    std::u32string ret;
    for (const char* current = utf8.data(); *current; ++current)
        ret += decodeUnicode(current);
    return ret;
}

char32_t unquoteCharacter(const char*& current) {
    char32_t result;
    char8_t ch1 = *++current;
    if (ch1 == '\\') {
        switch (*++current) {
            case '\'': result = '\''; break;
            case '\"': result = '\"'; break;
            case '\\': result = '\\'; break;
            case '0': result = '\0'; break;
            case 'a': result = '\a'; break;
            case 'b': result = '\b'; break;
            case 'f': result = '\f'; break;
            case 'n': result = '\n'; break;
            case 'r': result = '\r'; break;
            case 't': result = '\t'; break;
            case 'v': result = '\v'; break;
            case 'x': { // ASCII literal
                result = parseHexASCII(current);
                break;
            }
            case 'u': { // Unicode literal
                result = parseHexUnicode(current);
                break;
            }
            default: Diagnostics::malformedLiteral();
        }
    } else if (ch1 == '\'' || ch1 == '\n' || ch1 == '\0') {
        Diagnostics::malformedLiteral();
    } else result = decodeUnicode(current);
    if (*++current != '\'') Diagnostics::malformedLiteral();
    return result;
}

std::string unquoteString(const char*& current) {
    std::string result;
    char8_t ch1;
    while ((ch1 = *++current) != '"') {
        switch (bytes(ch1)) {
            case 1:
                if (ch1 == '\\') {
                    switch (*++current) {
                        case '\'': result += '\''; break;
                        case '\"': result += '\"'; break;
                        case '\\': result += '\\'; break;
                        case '0': result += '\0'; break;
                        case 'a': result += '\a'; break;
                        case 'b': result += '\b'; break;
                        case 'f': result += '\f'; break;
                        case 'n': result += '\n'; break;
                        case 'r': result += '\r'; break;
                        case 't': result += '\t'; break;
                        case 'v': result += '\v'; break;
                        case 'x': { // ASCII literal
                            result += parseHexASCII(current);
                            break;
                        }
                        case 'u': { // Unicode literal
                            result += encodeUnicode(parseHexUnicode(current));
                            break;
                        }
                        default: Diagnostics::malformedLiteral();
                    }
                } else if (ch1 == '\n' || ch1 == '\0') {
                    Diagnostics::malformedLiteral();
                } else result += ch1;
                break;
            case 2: {
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) Diagnostics::terminate();
                result += ch1;
                result += ch2;
            } break;
            case 3: {
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) Diagnostics::terminate();
                char8_t ch3 = *++current;
                if (ch3 >> 6 != 0b10) Diagnostics::terminate();
                result += ch1;
                result += ch2;
                result += ch3;
            } break;
            case 4: {
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) Diagnostics::terminate();
                char8_t ch3 = *++current;
                if (ch3 >> 6 != 0b10) Diagnostics::terminate();
                char8_t ch4 = *++current;
                if (ch4 >> 6 != 0b10) Diagnostics::terminate();
                result += ch1;
                result += ch2;
                result += ch3;
                result += ch4;
            } break;

        }
    }
    return result;
}

std::string escape(char32_t unicode) {
    if (isSurrogate(unicode)) Diagnostics::surrogate();
    if (unicode > 0x10FFFF) Diagnostics::overflow();
    if (unicode <= 0x7F) {
        switch (char ASCII = char(unicode); ASCII) {
            case '\'': return "\\\'";
            case '\"': return "\\\"";
            case '\\': return "\\\\";
            case '\0': return "\\0";
            case '\a': return "\\a";
            case '\b': return "\\b";
            case '\f': return "\\f";
            case '\n': return "\\n";
            case '\r': return "\\r";
            case '\t': return "\\t";
            case '\v': return "\\v";
            default:
                if (std::isprint(ASCII)) {
                    return {ASCII};
                } else {
                    char result[5];
                    std::sprintf(result, "\\x%02x", ASCII);
                    return result;
                }
        }
    }
    char result[9];
    std::sprintf(result, "\\u%06x", unicode);
    return result;
}

std::string quote(char32_t unicode) {
    return '\'' + escape(unicode) + '\'';
}

std::string quote(std::string const& string) {
    std::string result;
    for (const char* current = string.data(); *current; ++current)
        result += escape(decodeUnicode(current));
    return '"' + result + '"';
}

[[nodiscard]] int fgetc(FILE* in) {
    return decodeUnicode([in] { return std::fgetc(in); });
}

[[nodiscard]] std::string fgets(FILE* in) {
    return fgets(in, [](char32_t ch) { return isASCII(ch) && isspace(ch); });
}

[[nodiscard]] std::string fgets(FILE* in, std::function<bool(char32_t)> const& until) {
    int ch;
    std::u32string result;
    while (ch = Unicode::fgetc(in), !until(ch) && ch != EOF) result += char32_t(ch);
    return encodeUnicode(result);
}

}
