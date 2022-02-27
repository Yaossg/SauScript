#include "Unicode.hpp"


namespace SauScript::Unicode {

[[noreturn]] void malformed(int line) {
    throw SyntaxError("Malformed hexadecimal unicode" + at(line));
}

[[noreturn]] void illegal(int line) {
    throw SyntaxError("Illegal unicode value" + at(line));
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

char parseHexASCII(const char*& current, int line) {
    char8_t ch1 = *++current;
    if (!('0' <= ch1 && ch1 < '8')) malformed(line);
    char8_t ch2 = *++current;
    if (!std::isxdigit(ch2)) malformed(line);
    return char((hex(ch1) << 4) | hex(ch2));
}

char32_t parseHexUnicode(const char*& current, int line) {
    char8_t ch1 = *++current;
    if (ch1 != '0' && ch1 != '1') malformed(line);
    char8_t ch2 = *++current;
    if (!std::isxdigit(ch2) || ch1 == '1' && ch2 != '0') malformed(line);
    char8_t ch3 = *++current;
    if (!std::isxdigit(ch3)) malformed(line);
    char8_t ch4 = *++current;
    if (!std::isxdigit(ch4)) malformed(line);
    char8_t ch5 = *++current;
    if (!std::isxdigit(ch5)) malformed(line);
    char8_t ch6 = *++current;
    if (!std::isxdigit(ch6)) malformed(line);
    char32_t result = (hex(ch1) << 20) | (hex(ch2) << 16) | (hex(ch3) << 12) | (hex(ch4) << 8) | (hex(ch5) << 4) | hex(ch6);
    if (isSurrogate(result)) illegal(line);
    return result;
}

std::string encodeUnicode(char32_t unicode, int line) {
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
        illegal(line);
    }
}

char32_t decodeUnicode(const char*& current, int line) {
    char32_t result;
    char8_t ch1 = *current;
    if (ch1 & 0x80) { // multibyte
        if (ch1 >> 5 == 0b110) { // 2 bytes
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) illegal(line);
            result = ((ch1 & ~0xC0) << 6) | (ch2 & ~0x80);
        } else if (ch1 >> 4 == 0b1110) { // 3 bytes
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) illegal(line);
            char8_t ch3 = *++current;
            if (ch3 >> 6 != 0b10) illegal(line);
            result = ((ch1 & ~0xE0) << 12) | ((ch2 & ~0x80) << 6) | (ch3 & ~0x80);
        } else if (ch1 >> 3 == 0b11110) { // 4 bytes
            char8_t ch2 = *++current;
            if (ch2 >> 6 != 0b10) illegal(line);
            char8_t ch3 = *++current;
            if (ch3 >> 6 != 0b10) illegal(line);
            char8_t ch4 = *++current;
            if (ch4 >> 6 != 0b10) illegal(line);
            result = ((ch1 & ~0xF0) << 18) | ((ch2 & ~0x80) << 12) | ((ch3 & ~0x80) << 6) | (ch4 & ~0x80);
        } else {
            illegal(line);
        }
    } else result = ch1;
    if (isSurrogate(result)) illegal(line);
    return result;
}

int decodeUnicode(std::function<int()> current, int line) {
    char32_t result;
    int ch1 = current();
    if (ch1 == EOF) return EOF;
    if (ch1 & 0x80) { // multibyte
        if (ch1 >> 5 == 0b110) { // 2 bytes
            int ch2 = current();
            if (ch2 == EOF) return EOF;
            if (ch2 >> 6 != 0b10) illegal(line);
            result = ((ch1 & ~0xC0) << 6) | (ch2 & ~0x80);
        } else if (ch1 >> 4 == 0b1110) { // 3 bytes
            int ch2 = current();
            if (ch2 == EOF) return EOF;
            if (ch2 >> 6 != 0b10) illegal(line);
            int ch3 = current();
            if (ch3 == EOF) return EOF;
            if (ch3 >> 6 != 0b10) illegal(line);
            result = ((ch1 & ~0xE0) << 12) | ((ch2 & ~0x80) << 6) | (ch3 & ~0x80);
        } else if (ch1 >> 3 == 0b11110) { // 4 bytes
            int ch2 = current();
            if (ch2 == EOF) return EOF;
            if (ch2 >> 6 != 0b10) illegal(line);
            int ch3 = current();
            if (ch3 == EOF) return EOF;
            if (ch3 >> 6 != 0b10) illegal(line);
            int ch4 = current();
            if (ch4 == EOF) return EOF;
            if (ch4 >> 6 != 0b10) illegal(line);
            result = ((ch1 & ~0xF0) << 18) | ((ch2 & ~0x80) << 12) | ((ch3 & ~0x80) << 6) | (ch4 & ~0x80);
        } else {
            illegal(line);
        }
    } else result = ch1;
    if (isSurrogate(result)) illegal(line);
    return result;
}

char32_t unquoteCharacter(const char*& current, int line) {
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
                result = parseHexASCII(current, line);
                break;
            }
            case 'u': { // Unicode literal
                result = parseHexUnicode(current, line);
                break;
            }
            default: illegal(line);
        }
    } else if (ch1 == '\'' || ch1 == '\n' || ch1 == '\0') {
        illegal(line);
    } else result = decodeUnicode(current, line);
    if (*++current != '\'') illegal(line);
    return result;
}

std::string unquoteString(const char*& current, int line) {
    std::string result;
    char8_t ch1;
    while ((ch1 = *++current) != '"') {
        if (ch1 & 0x80) { // multibyte
            if (ch1 >> 5 == 0b110) { // 2 bytes
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) illegal(line);
                result += ch1;
                result += ch2;
            } else if (ch1 >> 4 == 0b1110) { // 3 bytes
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) illegal(line);
                char8_t ch3 = *++current;
                if (ch3 >> 6 != 0b10) illegal(line);
                result += ch1;
                result += ch2;
                result += ch3;
            } else if (ch1 >> 3 == 0b11110) { // 4 bytes
                char8_t ch2 = *++current;
                if (ch2 >> 6 != 0b10) illegal(line);
                char8_t ch3 = *++current;
                if (ch3 >> 6 != 0b10) illegal(line);
                char8_t ch4 = *++current;
                if (ch4 >> 6 != 0b10) illegal(line);
                result += ch1;
                result += ch2;
                result += ch3;
                result += ch4;
            } else {
                illegal(line);
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
                    result += parseHexASCII(current, line);
                    break;
                }
                case 'u': { // Unicode literal
                    result += encodeUnicode(parseHexUnicode(current, line), line);
                    break;
                }
                default: illegal(line);
            }
        } else if (ch1 == '\n' || ch1 == '\0') {
            illegal(line);
        } else result += ch1;
    }
    return result;
}


std::string escape(char32_t unicode, int line) {
    if (unicode > 0x10FFFF || isSurrogate(unicode)) illegal(line);
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

std::string quote(char32_t unicode, int line) {
    return '\'' + escape(unicode, line) + '\'';
}

std::string quote(std::string string, int line) {
    std::string result;
    for (const char* current = string.data(); *current; ++current)
        result += escape(decodeUnicode(current, line), line);
    return '"' + result + '"';
}

}
