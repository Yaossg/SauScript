#include "SauScript.hpp"

namespace SauScript {

Type Token::parseType() const {
    if (type != TokenType::IDENTIFIER) throw SyntaxError("expected type name" + at());
    std::string name = identifier();
    auto first = std::begin(TYPE_NAMES), last = std::end(TYPE_NAMES);
    Type type = Type(std::find(first, last, name) - first);
    if (type == Type::NAT)
        throw SyntaxError("invalid type name" + at());
    return type;
}

bool skipLineBreak(char const*& current, bool strict, int& line) {
    switch (*current) {
        case '#':
            while (*current && *current != '\n') ++current;
            if (*current == '\n')
                case '\n': ++current;
            ++line;
            return true;
        case ';':
            if (strict) return false;
            skipLineBreak(++current, strict, line);
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

void scan(const char*& current, predicate p, int line) {
    if (current[0] == '_') throw SyntaxError("invalid underscore in number literal" + at(line));
    while (*current == '_' || p(*current)) ++current;
    if (current[-1] == '_') throw SyntaxError("invalid underscore in number literal" + at(line));
}

Token parseNumber(const char*& current, int line) {
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
    scan(current, p, line);
    bool real = false;
    if (*current == '.') {
        real = true;
        auto dot = ++current;
        scan(current, p, line);
        if (current == dot)
            throw SyntaxError("use suffix '.0' to hint number's type to avoid ambiguity with dot operator" + at(line));
    }
    if (base == 10 && (*current == 'e' || *current == 'E')
        || base == 16 && (*current == 'p' || *current == 'P')) {
        real = true;
        ++current;
        if (*current == '+' || *current == '-') ++current;
        scan(current, p, line);
    }
    if (real && (base == 2 || base == 8))
        throw SyntaxError("binary or octal real literal" + at(line));
    if (*start == '0' && (real ? start[1] != '.' : start + 1 != current))
        throw SyntaxError("redundant 0 is forbidden to avoid ambiguity, use 0o if octal" + at(line));
    if (isalnum(*current)) throw SyntaxError("invalid suffix of number" + at(line));
    std::string number{start, current};
    erase(number, '_');
    return real
        ? Token::literal_real(std::stod((base == 10 ? "" : "0x") + number))
        : Token::literal_int(int_t(std::stoull(number, nullptr, base)));
}

[[nodiscard]] std::vector<Token> tokenize(char const* current) {
    std::vector<Token> tokens;
    int line = 1;
    while (char const ch = *current) {
        if (ch == '\\') {
            if (skipLineBreak(++current, true, line)) continue;
            throw SyntaxError("stray '\\'" + at(line));
        } else if (int old_line = line; skipLineBreak(current, false, line)) {
            tokens.push_back(Token::linebreak().at(old_line));
        } else if (std::isspace(ch)) {
            ++current;
        } else {
            switch (ch) {
                case '(':
                    tokens.push_back(Token::parenLeft().at(line));
                    ++current;
                    continue;
                case ')':
                    tokens.push_back(Token::parenRight().at(line));
                    ++current;
                    continue;
                case '[':
                    tokens.push_back(Token::bracketLeft().at(line));
                    ++current;
                    continue;
                case ']':
                    tokens.push_back(Token::bracketRight().at(line));
                    ++current;
                    continue;
                case '{':
                    tokens.push_back(Token::braceLeft().at(line));
                    ++current;
                    continue;
                case '}':
                    tokens.push_back(Token::braceRight().at(line));
                    ++current;
                    continue;
                case '\'':
                    tokens.push_back(Token::literal_int(Unicode::unquoteCharacter(current, line)).at(line));
                    ++current;
                    continue;
            }
            if (isIdentifierStart(ch)) {
                char const *first = current;
                while (isIdentifierContinue(*++current));
                std::string token{first, current};
                if (token == "false") {
                    tokens.push_back(Token::literal_bool(false).at(line));
                } else if (token == "true") {
                    tokens.push_back(Token::literal_bool(true).at(line));
                } else if (token == "__LINE__") {
                    tokens.push_back(Token::literal_int(line).at(line));
                } else if (token == "EOF") {
                    tokens.push_back(Token::literal_int(EOF).at(line));
                } else if (token == "nan") {
                    tokens.push_back(Token::literal_real(0.0 / 0.0).at(line));
                } else if (token == "inf") {
                    tokens.push_back(Token::literal_real(1.0 / 0.0).at(line));
                } else if (auto kw = Keyword::parse(token); kw != Keyword::NAK) {
                    tokens.push_back(Token::keyword(kw).at(line));
                } else if (auto &&ops = Operators::tokens(); std::find(ops.begin(), ops.end(), token) != ops.end()) {
                    tokens.push_back(Token::punctuator(token).at(line));
                } else {
                    tokens.push_back(Token::identifier(token).at(line));
                }
            } else if (isNumberStart(ch)) {
                tokens.push_back(parseNumber(current, line));
            } else {
                std::string token;
                for (auto &&op: Operators::tokens()) {
                    if (std::string_view{current}.starts_with(op)) {
                        if (op.length() > token.length()) token = op;
                    }
                }
                if (!token.empty()) {
                    tokens.push_back(Token::punctuator(token).at(line));
                    current += token.length();
                } else {
                    throw SyntaxError("unexpected token" + at(line));
                }
            }
        }
    }
    tokens.push_back(Token::linebreak().at(line));
    tokens.push_back(Token::terminator().at(line));
    return tokens;
}

}