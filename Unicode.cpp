#include "Unicode.hpp"

namespace SauScript::Unicode {

[[noreturn]] void malformed() {
    throw PlainRuntimeError("Malformed hexadecimal unicode");
}

[[noreturn]] void illegal() {
    throw PlainRuntimeError("Illegal unicode value");
}

[[nodiscard]] bool isSurrogate(char32_t ch) {
    return 0xD800 <= ch && ch <= 0xDFFF;
}

char32_t hex(char ch) {
    if ('0' <= ch && ch <= '9') return ch - '0';
    if ('a' <= ch && ch <= 'f') return ch - 'a' + 10;
    if ('A' <= ch && ch <= 'F') return ch - 'A' + 10;
    throw RuntimeError("Assertion failed");
}

char parseHexASCII(const char*& current) {
    char8_t ch1 = *++current;
    if (!('0' <= ch1 && ch1 < '8')) malformed();
    char8_t ch2 = *++current;
    if (!std::isxdigit(ch2)) malformed();
    return char((hex(ch1) << 4) | hex(ch2));
}

char32_t parseHexUnicode(const char*& current) {
    char8_t ch1 = *++current;
    if (ch1 != '0' && ch1 != '1') malformed();
    char8_t ch2 = *++current;
    if (!std::isxdigit(ch2) || ch1 == '1' && ch2 != '0') malformed();
    char8_t ch3 = *++current;
    if (!std::isxdigit(ch3)) malformed();
    char8_t ch4 = *++current;
    if (!std::isxdigit(ch4)) malformed();
    char8_t ch5 = *++current;
    if (!std::isxdigit(ch5)) malformed();
    char8_t ch6 = *++current;
    if (!std::isxdigit(ch6)) malformed();
    char32_t result = (hex(ch1) << 20) | (hex(ch2) << 16) | (hex(ch3) << 12) | (hex(ch4) << 8) | (hex(ch5) << 4) | hex(ch6);
    if (isSurrogate(result)) illegal();
    return result;
}

std::string encodeUnicode(char32_t unicode) {
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
        illegal();
    }
}

char32_t decodeUnicode(const char*& current) {
    char32_t result;
    char8_t ch1 = *current;
    if (ch1 & 0x80) { // multibyte
        if (ch1 >> 5 == 0b110) { // 2 bytes
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) illegal();
            result = ((ch1 & ~0xC0) << 6) | (ch2 & ~0x80);
        } else if (ch1 >> 4 == 0b1110) { // 3 bytes
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) illegal();
            char8_t ch3 = *++current;
            if (ch3 >> 6 != 0b10) illegal();
            result = ((ch1 & ~0xE0) << 12) | ((ch2 & ~0x80) << 6) | (ch3 & ~0x80);
        } else if (ch1 >> 3 == 0b11110) { // 4 bytes
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) illegal();
            char8_t ch3 = *++current;
            if (ch3 >> 6 != 0b10) illegal();
            char8_t ch4 = *++current;
            if (ch4 >> 6 != 0b10) illegal();
            result = ((ch1 & ~0xF0) << 18) | ((ch2 & ~0x80) << 12) | ((ch3 & ~0x80) << 6) | (ch4 & ~0x80);
        } else {
            illegal();
        }
    } else result = ch1;
    if (isSurrogate(result)) illegal();
    return result;
}

int decodeUnicode(std::function<int()> const& current) {
    char32_t result;
    int ch1 = current();
    if (ch1 == EOF) return EOF;
    if (ch1 & 0x80) { // multibyte
        if (ch1 >> 5 == 0b110) { // 2 bytes
            int ch2 = current();
            if (ch2 == EOF) return EOF;
            if (ch2 >> 6 != 0b10) illegal();
            result = ((ch1 & ~0xC0) << 6) | (ch2 & ~0x80);
        } else if (ch1 >> 4 == 0b1110) { // 3 bytes
            int ch2 = current();
            if (ch2 == EOF) return EOF;
            if (ch2 >> 6 != 0b10) illegal();
            int ch3 = current();
            if (ch3 == EOF) return EOF;
            if (ch3 >> 6 != 0b10) illegal();
            result = ((ch1 & ~0xE0) << 12) | ((ch2 & ~0x80) << 6) | (ch3 & ~0x80);
        } else if (ch1 >> 3 == 0b11110) { // 4 bytes
            int ch2 = current();
            if (ch2 == EOF) return EOF;
            if (ch2 >> 6 != 0b10) illegal();
            int ch3 = current();
            if (ch3 == EOF) return EOF;
            if (ch3 >> 6 != 0b10) illegal();
            int ch4 = current();
            if (ch4 == EOF) return EOF;
            if (ch4 >> 6 != 0b10) illegal();
            result = ((ch1 & ~0xF0) << 18) | ((ch2 & ~0x80) << 12) | ((ch3 & ~0x80) << 6) | (ch4 & ~0x80);
        } else {
            illegal();
        }
    } else result = ch1;
    if (isSurrogate(result)) illegal();
    return result;
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
            default: illegal();
        }
    } else if (ch1 == '\'' || ch1 == '\n' || ch1 == '\0') {
        illegal();
    } else result = decodeUnicode(current);
    if (*++current != '\'') illegal();
    return result;
}

std::string unquoteString(const char*& current) {
    std::string result;
    char8_t ch1;
    while ((ch1 = *++current) != '"') {
        if (ch1 & 0x80) { // multibyte
            if (ch1 >> 5 == 0b110) { // 2 bytes
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) illegal();
                result += ch1;
                result += ch2;
            } else if (ch1 >> 4 == 0b1110) { // 3 bytes
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) illegal();
                char8_t ch3 = *++current;
                if (ch3 >> 6 != 0b10) illegal();
                result += ch1;
                result += ch2;
                result += ch3;
            } else if (ch1 >> 3 == 0b11110) { // 4 bytes
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) illegal();
                char8_t ch3 = *++current;
                if (ch3 >> 6 != 0b10) illegal();
                char8_t ch4 = *++current;
                if (ch4 >> 6 != 0b10) illegal();
                result += ch1;
                result += ch2;
                result += ch3;
                result += ch4;
            } else {
                illegal();
            }
        } if (ch1 == '\\') {
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
                default: illegal();
            }
        } else if (ch1 == '\n' || ch1 == '\0') {
            illegal();
        } else result += ch1;
    }
    return result;
}


std::string escape(char32_t unicode) {
    if (unicode > 0x10FFFF || isSurrogate(unicode)) illegal();
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

std::string quote(std::string string) {
    std::string result;
    for (const char* current = string.data(); *current; ++current)
        result += escape(decodeUnicode(current));
    return '"' + result + '"';
}

}
