#include "Token.hpp"
#include "Keyword.hpp"
#include "Operator.hpp"
#include "Unicode.hpp"

namespace SauScript {

std::string SourceLocation::at() const {
    if (code == nullptr) return " at unknown source";
    return " at line " + std::to_string(line) + '\n' + code->lines[line - 1] + '\n' + std::string(column - 1, '~') + '^';
}

SourceCode::SourceCode(std::string raw) : raw(std::move(raw)), lines(splitLines(this->raw)), current(this->raw.data()), start(current), line(1) {
    tokenize();
}

bool SourceCode::skipLineBreak(bool strict) {
    switch (*current) {
        case '#':
            while (*current && *current != '\n') ++current;
            if (*current == '\n')
                case '\n': ++current;
            ++line;
            start = current;
            return true;
        case ';':
            if (strict) return false;
            ++current;
            skipLineBreak(strict);
            return true;
    }
    return false;
}

bool isIdentifierStart(char ch) {
    return std::isalpha(ch) || ch == '_' || ch == '$';
}

bool isIdentifierContinue(char ch) {
    return std::isalnum(ch) || ch == '_' || ch == '$';
}

bool isNumberStart(char ch) {
    return std::isdigit(ch);
}

bool b(char ch) { return ch == '0' || ch == '1'; }
bool o(char ch) { return ch >= '0' && ch <= '7'; }
bool d(char ch) { return ch >= '0' && ch <= '9'; }
bool h(char ch) { return ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'f' || ch >= 'A' && ch <= 'F'; }

using predicate = bool (*) (char);
void scan(const char*& current, predicate p) {
    if (current[0] == '_') throw PlainRuntimeError("invalid underscore in number literal");
    while (*current == '_' || p(*current)) ++current;
    if (current[-1] == '_') throw PlainRuntimeError("invalid underscore in number literal");
}

Token SourceCode::parseNumber() {
    int base = 10;
    predicate p = d;
    if (*current == '0') {
        switch (*++current) {
            case 'x': case 'X': base = 16; p = h; ++current; break;
            case 'o': case 'O': base = 8;  p = o; ++current; break;
            case 'b': case 'B': base = 2;  p = b; ++current; break;
            default: --current;
        }
    }
    const char* start = current;
    scan(current, p);
    bool real = false;
    if (*current == '.') {
        real = true;
        auto dot = ++current;
        scan(current, p);
        if (current == dot)
            throw SyntaxError("use suffix '.0' to hint number's type to avoid ambiguity with dot operator" + at());
    }
    if (base == 10 && (*current == 'e' || *current == 'E')
        || base == 16 && (*current == 'p' || *current == 'P')) {
        real = true;
        ++current;
        if (*current == '+' || *current == '-') ++current;
        scan(current, p);
    }
    if (real && (base == 2 || base == 8))
        throw SyntaxError("binary or octal real literal" + at());
    if (*start == '0' && (real ? start[1] != '.' : start + 1 != current))
        throw SyntaxError("redundant 0 is forbidden to avoid ambiguity, use 0o if octal" + at());
    if (isalnum(*current)) throw SyntaxError("invalid suffix of number" + at());
    std::string number{start, current};
    erase(number, '_');
    try {
        return real
            ? Token::literal_real(std::stod((base == 10 ? "" : "0x") + number))
            : Token::literal_int(int_t(std::stoull(number, nullptr, base)));
    } catch (std::out_of_range&) {
        throw SyntaxError("number literal overflow" + at());
    }
}

Token SourceCode::parseIdentifier() {
    char const *first = current;
    while (isIdentifierContinue(*++current));
    std::string token{first, current};
    if (token == "false") {
        return Token::literal_bool(false);
    } else if (token == "true") {
        return Token::literal_bool(true);
    } else if (token == "__LINE__") {
        return Token::literal_int(location().line);
    } else if (token == "EOF") {
        return Token::literal_int(EOF);
    } else if (token == "nan") {
        return Token::literal_real(0.0 / 0.0);
    } else if (token == "inf") {
        return Token::literal_real(1.0 / 0.0);
    } else if (auto kw = Keyword::parse(token); kw != Keyword::NAK) {
        return Token::keyword(kw);
    } else if (auto &&ops = Operators::tokens(); std::find(ops.begin(), ops.end(), token) != ops.end()) {
        return Token::punctuator(token);
    } else {
        return Token::identifier(token);
    }
}

void SourceCode::tokenize() {
    while (char const ch = *current) try {
        auto old = location();
        if (ch == '\\') {
            ++current;
            if (skipLineBreak(true)) continue;
            throw SyntaxError("stray '\\'" + at());
        } else if (skipLineBreak(false)) {
            tokens.push_back(Token::linebreak().at(old));
        } else if (std::isspace(ch)) {
            ++current;
        } else {
            switch (ch) {
                case '(':
                    tokens.push_back(Token::parenLeft().at(location()));
                    ++current;
                    continue;
                case ')':
                    tokens.push_back(Token::parenRight().at(location()));
                    ++current;
                    continue;
                case '[':
                    tokens.push_back(Token::bracketLeft().at(location()));
                    ++current;
                    continue;
                case ']':
                    tokens.push_back(Token::bracketRight().at(location()));
                    ++current;
                    continue;
                case '{':
                    tokens.push_back(Token::braceLeft().at(location()));
                    ++current;
                    continue;
                case '}':
                    tokens.push_back(Token::braceRight().at(location()));
                    ++current;
                    continue;
                case '\'':
                    tokens.push_back(Token::literal_int(Unicode::unquoteCharacter(current)).at(location()));
                    ++current;
                    continue;
            }
            if (isIdentifierStart(ch)) {
                tokens.push_back(parseIdentifier().at(old));
            } else if (isNumberStart(ch)) {
                tokens.push_back(parseNumber().at(old));
            } else {
                std::string token;
                for (auto &&op: Operators::tokens()) {
                    if (std::string_view{current}.starts_with(op)) {
                        if (op.length() > token.length()) token = op;
                    }
                }
                if (!token.empty()) {
                    tokens.push_back(Token::punctuator(token).at(location()));
                    current += token.length();
                } else {
                    throw SyntaxError("unexpected token" + at());
                }
            }
        }
    } catch(PlainRuntimeError& pre) {
        throw SyntaxError(pre.what() + at());
    }
    tokens.push_back(Token::linebreak().at(location()));
    tokens.push_back(Token::terminator().at(location()));
}

}