#include "SauScript.hpp"

namespace SauScript {

int normalize(std::vector<Token>& tokens, int i = 0) {
    for (; i < tokens.size() && tokens[i].type == TokenType::PUNCTUATION; ++i) {
        int op = tokens[i].parseOp(PREFIX);
        if (op < 0) break;
        tokens[i].asOperator(op);
    }
    for (; i < tokens.size(); ++i) {
        if (tokens[i] == Token::parenLeft()) {
            ++i;
            i = normalize(tokens, i);
        } else if (tokens[i] == Token::parenRight()) {
            return i;
        } else if (tokens[i].type == TokenType::PUNCTUATION) {
            int op = tokens[i].parseOp(INFIX);
            if (op < 0) {
                tokens[i].asOperator(tokens[i].parseOp(POSTFIX));
            } else {
                tokens[i].asOperator(op);
                if (tokens[i + 1] != Token::parenLeft())
                    i = normalize(tokens, i + 1);
            }
        }
    }
    return i;
}

[[nodiscard]] auto transfix(std::vector<Token> const& infix) {
    std::stack<Token> ops;
    std::vector<Token> postfix;
    for (auto&& token : infix) {
        switch (token.type) {
            case TokenType::OPERATOR: {
                auto&& op = token.operator()();
                while (!ops.empty() && ops.top().type != TokenType::PAREN) {
                    auto&& top = ops.top().operator()();
                    if (op.operandType == PREFIX) break;
                    if (op.precedence > top.precedence) break;
                    if (op.precedence == top.precedence && op.associativity) break;
                    postfix.push_back(ops.top());
                    ops.pop();
                }
                ops.push(token);
            } break;
            case TokenType::PAREN:
                if (token.paren()) {
                    while (!ops.empty() && ops.top().type != TokenType::PAREN) {
                        postfix.push_back(ops.top());
                        ops.pop();
                    }
                    if (ops.empty()) throw SyntaxError("missing '(' to match ')' at line " + token.at());
                    ops.pop(); // "("
                } else {
                    ops.push(token);
                }
                break;
            default:
                postfix.push_back(token);
        }
    }
    while (!ops.empty()) {
        postfix.push_back(ops.top());
        ops.pop();
    }
    return postfix;
}

[[nodiscard]] auto tokenize(char const* source) {
    std::vector<Token> tokens;
    int line = 1;
    while (char ch = *source) {
        if (ch == '\\') {
            if (skipLineBreak(++source, true, line)) continue;
            throw SyntaxError("stray '\\' at line " + toLineString(line));
        } else if (skipLineBreak(source, false, line)) {
            tokens.push_back(Token::linebreak());
        } else if (std::isspace(ch)) {
            ++source;
        } else if (ch == '(') {
            tokens.push_back(Token::parenLeft().at(line));
            ++source;
        } else if (ch == ')') {
            tokens.push_back(Token::parenRight().at(line));
            ++source;
        } else if (isIdentifierStart(ch)) {
            char const* first = source;
            while (isIdentifierContinue(*++source));
            std::string token{first, source};
            if (token == "false") {
                tokens.push_back(Token::literal_bool(false).at(line));
            } else if (token == "true") {
                tokens.push_back(Token::literal_bool(true).at(line));
            } else if (token == "nan") {
                tokens.push_back(Token::literal_real(0.0 / 0.0).at(line));
            } else if (token == "inf") {
                tokens.push_back(Token::literal_real(1.0 / 0.0).at(line));
            } else if (auto kw = Keyword::parse(token); kw != Keyword::NAK) {
                tokens.push_back(Token::keyword(kw).at(line));
            } else if (std::any_of(std::begin(OPERATORS), std::end(OPERATORS),
                                   [&token] (Operator const& op) { return op.literal == token; })) {
                tokens.push_back(Token::punctuation(token).at(line));
            } else {
                tokens.push_back(Token::identifier(token).at(line));
            }
        } else if (isNumberStart(ch)) {
            try {
                size_t idx;
                auto x = std::stoll(source, &idx, 0);
                if (source[idx] != '.' && source[idx] != 'e' && source[idx] != 'E') {
                    tokens.push_back(Token::literal_int(x).at(line));
                    source += idx;
                    continue;
                }
            } catch (...) {

            }
            try {
                size_t idx;
                tokens.push_back(Token::literal_real(std::stod(source, &idx)).at(line));
                source += idx;
            } catch (...) {
                throw RuntimeError("invalid literal number at line " + toLineString(line));
            }
        } else {
            std::string token;
            for (auto&& op : OPERATORS) {
                if (std::string_view{source}.starts_with(op.literal)) {
                    if (op.literal.length() > token.length()) token = op.literal;
                }
            }
            if (!token.empty()) {
                tokens.push_back(Token::punctuation(token).at(line));
                source += token.length();
            } else {
                throw SyntaxError("unexpected token at line " + toLineString(line));
            }
        }
    }
    tokens.push_back(Token::linebreak());
    tokens.push_back(Token::eot().at(line));
    return tokens;
}

std::optional<Operand> ScriptEngine::findOperand(const std::string& name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto&& scope = *it;
        if (scope.contains(name)) return &scope[name];
    }
    return std::nullopt;
}

std::unique_ptr<ExprNode> ScriptEngine::compileExpression(std::vector<Token> tokens) {
    normalize(tokens);
    std::stack<std::unique_ptr<ExprNode>> operands;
    for (auto&& token : transfix(tokens)) {
        switch (token.type) {
            case TokenType::LINEBREAK: continue;
            case TokenType::OPERATOR:
                operands.push(token.operator()().apply(this, token.line, operands));
                break;
            case TokenType::LITERAL_BOOL:
                operands.push(std::make_unique<ValNode>(token.line, Number{token.literal_bool()}));
                break;
            case TokenType::LITERAL_INT:
                operands.push(std::make_unique<ValNode>(token.line, Number{token.literal_int()}));
                break;
            case TokenType::LITERAL_REAL:
                operands.push(std::make_unique<ValNode>(token.line, Number{token.literal_real()}));
                break;
            case TokenType::IDENTIFIER:
                operands.push(std::make_unique<RefNode>(this, token.line, token.identifier()));
                break;
            default:
                throw SyntaxError("invalid token to compile at line " + token.at());
        }
    }
    if (operands.empty()) throw SyntaxError("empty expression at line " + tokens.back().at());
    return std::move(operands.top());
}

std::unique_ptr<StmtNode> ScriptEngine::compileStatement(std::vector<Token> tokens) {
    if (tokens.empty()) return std::make_unique<StmtNoopNode>();
    if (auto&& first = tokens[0]; first.type == TokenType::KEYWORD) {
        using namespace Keyword;
        switch (first.keyword()) {
            case DEL:
                if (tokens.size() == 2 && tokens[1].type == TokenType::IDENTIFIER) {
                    return std::make_unique<DelNode>(this, first.line, tokens[1].identifier());
                } else {
                    throw SyntaxError("bad del statement at line " + first.at());
                }
            case LET:
                if (tokens.size() >= 3 && tokens[1].type == TokenType::IDENTIFIER
                    && tokens[2] == Token::punctuation("=")) {
                    std::string id = tokens[1].identifier();
                    tokens.erase(tokens.begin(), tokens.begin() + 3);
                    return std::make_unique<LetNode>(this, first.line, id, compileExpression(tokens));
                } else {
                    throw SyntaxError("bad let statement at line " + first.at());
                }
            case BREAK:
                if (tokens.size() == 1) {
                    return std::make_unique<BreakNode>(first.line);
                } else {
                    throw SyntaxError("bad break statement at line " + first.at());
                }
            case CONTINUE:
                if (tokens.size() == 1) {
                    return std::make_unique<ContinueNode>(first.line);
                } else {
                    throw SyntaxError("bad continue statement at line " + first.at());
                }
        }
    }
    return std::make_unique<StmtExprNode>(compileExpression(tokens));
}

std::unique_ptr<StmtNode> ScriptEngine::compileWhile(Token*& current) {
    using namespace Keyword;
    Token* first = ++current;
    while (*current != Token::keyword(DO)) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing do in while at line " + first->at());
        ++current;
    }
    auto cond = compileExpression({first, current});
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in while at line " + current->at());
    return std::make_unique<WhileNode>(this, std::move(cond), std::move(stmt));
}

std::unique_ptr<StmtNode> ScriptEngine::compileRepeat(Token*& current) {
    using namespace Keyword;
    auto stmt = compileStatements(++current);
    if (current->type == TokenType::KEYWORD)
        switch (current->keyword()) {
            case END:
                return std::make_unique<RepeatNode>(this, std::move(stmt));
            case UNTIL: {
                Token* first = ++current;
                while (*current != Token::keyword(END)) {
                    if (current->type == TokenType::EOT) throw SyntaxError("missing end in repeat at line " + current->at());
                    ++current;
                }
                return std::make_unique<RepeatNode>(this, std::move(stmt), compileExpression({first, current}));
            }
        }
    throw SyntaxError("end or until is expected at line " + current->at());
}

std::unique_ptr<StmtNode> ScriptEngine::compileFor(Token*& current) {
    using namespace Keyword;
    Token* first = ++current;
    while (*current != Token::linebreak()) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing first linebreak in for at line " + first->at());
        ++current;
    }
    auto init = compileStatement({first, current});
    first = ++current;
    while (*current != Token::linebreak()) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing second linebreak in for at line " + first->at());
        ++current;
    }
    auto cond = compileExpression({first, current});
    first = ++current;
    while (*current != Token::keyword(DO)) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing do in for at line " + first->at());
        ++current;
    }
    auto iter = compileExpression({first, current});
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in for at line " + current->at());
    return std::make_unique<ForNode>(this, std::move(init), std::move(cond), std::move(iter), std::move(stmt));
}

std::unique_ptr<StmtNode> ScriptEngine::compileDo(Token*& current) {
    using namespace Keyword;
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in do at line " + current->at());
    return std::make_unique<DoNode>(this, std::move(stmt));
}

std::unique_ptr<StmtNode> ScriptEngine::compileIf(Token*& current) {
    using namespace Keyword;
    Token* first = ++current;
    while (*current != Token::keyword(THEN)) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing then in if at line " + first->at());
        ++current;
    }
    auto cond = compileExpression({first, current});
    auto then = compileStatements(++current);
    if (current->type == TokenType::KEYWORD)
        switch (current->keyword()) {
            case END:
                return std::make_unique<IfNode>(this, std::move(cond), std::move(then));
            case ELSE: {
                auto else_ = compileStatements(++current);
                if (*current != Token::keyword(END)) throw SyntaxError("missing end in if at line " + current->at());
                return std::make_unique<IfNode>(this, std::move(cond), std::move(then), std::move(else_));
            }
            case ELIF: {
                *current = Token::keyword(IF);
                return std::make_unique<IfNode>(this, std::move(cond), std::move(then), compileIf(current));
            }
        }
    throw SyntaxError("end, else or elif is expected at line " + current->at());
}

std::unique_ptr<StmtNode> ScriptEngine::compileTry(Token*& current) {
    using namespace Keyword;
    auto try_ = compileStatements(++current);
    if (*current != Token::keyword(CATCH)) throw SyntaxError("try without catch at line " + current->at());
    if ((++current)->type != TokenType::IDENTIFIER) throw SyntaxError("catch without identifier at line " + current->at());
    auto name = current->identifier();
    auto catch_ = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in try at line " + current->at());
    return std::make_unique<TryNode>(this, std::move(try_), name, std::move(catch_));
}

std::unique_ptr<StmtNode> ScriptEngine::compileStatements(Token*& current) {
    std::vector<std::unique_ptr<StmtNode>> stmts;
    for (std::vector<Token> line; current->type != TokenType::EOT; ++current) {
        using namespace Keyword;
        switch (current->type) {
            case TokenType::KEYWORD:
                stmts.push_back(compileStatement(line));
                line.clear();
                switch (current->keyword()) {
                    case WHILE:
                        stmts.push_back(compileWhile(current));
                        continue;
                    case REPEAT:
                        stmts.push_back(compileRepeat(current));
                        continue;
                    case FOR:
                        stmts.push_back(compileFor(current));
                        continue;
                    case DO:
                        stmts.push_back(compileDo(current));
                        continue;
                    case IF:
                        stmts.push_back(compileIf(current));
                        continue;
                    case TRY:
                        stmts.push_back(compileTry(current));
                        continue;
                    case END:
                    case ELSE:
                    case ELIF:
                    case CATCH:
                    case UNTIL:
                        goto outer;
                }
                break;
            case TokenType::LINEBREAK:
                stmts.push_back(compileStatement(line));
                line.clear();
                continue;
        }
        line.push_back(*current);
    } outer:
    return std::make_unique<StmtSeqNode>(std::move(stmts));
}

std::unique_ptr<StmtNode> ScriptEngine::compile(const char* script) {
    auto tokens = tokenize(script);
    Token* current = tokens.data();
    return compileStatements(current);
}

void ScriptEngine::exec(const char* script, FILE* err) {
    try {
        compile(script)->exec();
    } catch (ScriptBreak e) {
        fprintf(err, "Wild break at line %s\n", toLineString(e.line).c_str());
    } catch (ScriptContinue e) {
        fprintf(err, "Wild continue at line %s\n", toLineString(e.line).c_str());
    } catch (ScriptException& caught) {
        fprintf(err, caught.what());
    } catch (SyntaxError& e) {
        fprintf(err, "Syntax error: %s\n", e.what());
    } catch (RuntimeError& e) {
        fprintf(err, "Runtime error: %s\n", e.what());
    }
}

template<typename Fn>
auto simpleBinary(Fn&& fn) {
    return [fn = std::forward<Fn&&>(fn)](ExprNode* lhs, ExprNode* rhs) {
        return std::visit([fn](auto lhs, auto rhs) { return Number{fn(lhs, rhs)}; }, lhs->eval().val().object, rhs->eval().val().object);
    };
}

template<typename Fn>
auto intBinary(char const* name, Fn&& fn, Assertion* an = noop_assert) {
    return [name, fn = std::forward<Fn&&>(fn), an](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval().val().asIntOp(name, lhs->line);
        auto b = rhs->eval().val().asIntOp(name, rhs->line);
        an(b, rhs->line);
        return Number{fn(a, b)};
    };
}

template<typename Fn>
auto simpleAssignment(Fn&& fn) {
    return [fn = std::forward<Fn&&>(fn)](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval();
        std::visit(fn, a.ref(lhs->line)->object, rhs->eval().val().object);
        return a;
    };
}

template<typename Fn>
auto intAssignment(char const* name, Fn&& fn, Assertion* an = noop_assert) {
    return [name, fn = std::forward<Fn&&>(fn), an](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval();
        int_t& r = a.ref(lhs->line)->asIntOp(name, lhs->line);
        int_t b = rhs->eval().val().asIntOp(name, lhs->line);
        an(b, rhs->line);
        fn(r, b);
        return a;
    };
}

const Operator OPERATORS[40] = {
        {15, LEFT_TO_RIGHT, POSTFIX, "++",  [](ExprNode* op){
            return Number{op->eval().ref(op->line)->asIntOp("++", op->line)++};
        }},
        {15, LEFT_TO_RIGHT, POSTFIX, "--",  [](ExprNode* op) {
            return Number{op->eval().ref(op->line)->asIntOp("--", op->line)--};
        }},
        {14, RIGHT_TO_LEFT, PREFIX, "++",   [](ExprNode* op){
            auto a = op->eval();
            ++a.ref(op->line)->asIntOp("++", op->line);
            return a;
        }},
        {14, RIGHT_TO_LEFT, PREFIX, "--",   [](ExprNode* op) {
            auto a = op->eval();
            --a.ref(op->line)->asIntOp("--", op->line);
            return a;
        }},
        {14, RIGHT_TO_LEFT, PREFIX, "+",    [](ExprNode* op) { return op->eval().val(); }},
        {14, RIGHT_TO_LEFT, PREFIX, "-",    [](ExprNode* op) { return op->eval().val().visit([](auto a) { return Number{-a}; }); }},
        {14, RIGHT_TO_LEFT, PREFIX, "!",    [](ExprNode* op) { return Number{!op->eval().val().asBool(op->line)}; }},
        {14, RIGHT_TO_LEFT, PREFIX, "~",    [](ExprNode* op) { return Number{~op->eval().val().asIntOp("~", op->line)}; }},
        {14, RIGHT_TO_LEFT, PREFIX, "input",[](ScriptEngine* engine, ExprNode* op) {
            return Number{op->eval().ref(op->line)->visit(overloaded {
                    [engine](int_t& a) { return fscanf(engine->in, "%lld", &a); },
                    [engine](real_t& a) { return fscanf(engine->in, "%lf", &a); }
            })};
        }},
        {12, LEFT_TO_RIGHT, INFIX, "*",  simpleBinary(std::multiplies<void>{})},
        {12, LEFT_TO_RIGHT, INFIX, "/",  [](ExprNode* lhs, ExprNode* rhs) {
            return std::visit(overloaded {
                    [line = rhs->line](int_t lhs, int_t rhs) { division_assert(rhs, line); return Number{lhs / rhs}; },
                    [](auto lhs, auto rhs) { return Number{lhs / rhs}; }
            }, lhs->eval().val().object, rhs->eval().val().object);
        }},
        {12, LEFT_TO_RIGHT, INFIX, "%",  intBinary("%", std::modulus<int_t>{}, division_assert)},
        {11, LEFT_TO_RIGHT, INFIX, "+",  simpleBinary(std::plus<void>{})},
        {11, LEFT_TO_RIGHT, INFIX, "-",  simpleBinary(std::minus<void>{})},
        {10, LEFT_TO_RIGHT, INFIX, "<<", intBinary("<<", [](int_t a, int_t b) { return a << b; }, shift_assert)},
        {10, LEFT_TO_RIGHT, INFIX, ">>", intBinary(">>", [](int_t a, int_t b) { return a >> b; }, shift_assert)},
        {8, LEFT_TO_RIGHT, INFIX, "<",   simpleBinary(std::less<void>{})},
        {8, LEFT_TO_RIGHT, INFIX, ">",   simpleBinary(std::greater<void>{})},
        {8, LEFT_TO_RIGHT, INFIX, "<=",  simpleBinary(std::less_equal<void>{})},
        {8, LEFT_TO_RIGHT, INFIX, ">=",  simpleBinary(std::greater_equal<void>{})},
        {7, LEFT_TO_RIGHT, INFIX, "==",  simpleBinary(std::equal_to<void>{})},
        {7, LEFT_TO_RIGHT, INFIX, "!=",  simpleBinary(std::not_equal_to<void>{})},
        {6, LEFT_TO_RIGHT, INFIX, "&",   intBinary("&", std::bit_and<int_t>{})},
        {5, LEFT_TO_RIGHT, INFIX, "^",   intBinary("^", std::bit_xor<int_t>{})},
        {4, LEFT_TO_RIGHT, INFIX, "|",   intBinary("|", std::bit_or<int_t>{})},
        {3, LEFT_TO_RIGHT, INFIX, "&&",  [](ExprNode* lhs, ExprNode* rhs) { return Number{lhs->eval().val().asBool(lhs->line) && rhs->eval().val().asBool(rhs->line)}; }},
        {2, LEFT_TO_RIGHT, INFIX, "||",  [](ExprNode* lhs, ExprNode* rhs) { return Number{lhs->eval().val().asBool(lhs->line) || rhs->eval().val().asBool(rhs->line)}; }},
        {1, RIGHT_TO_LEFT, INFIX, "=",   simpleAssignment([](auto& lhs, auto rhs) { lhs = rhs; })},
        {1, RIGHT_TO_LEFT, INFIX, "+=",  simpleAssignment([](auto& lhs, auto rhs) { lhs += rhs; })},
        {1, RIGHT_TO_LEFT, INFIX, "-=",  simpleAssignment([](auto& lhs, auto rhs) { lhs -= rhs; })},
        {1, RIGHT_TO_LEFT, INFIX, "*=",  simpleAssignment([](auto& lhs, auto rhs) { lhs *= rhs; })},
        {1, RIGHT_TO_LEFT, INFIX, "/=",  [](ExprNode* lhs, ExprNode* rhs) {
            auto a = lhs->eval();
            std::visit(overloaded {
                    [line = rhs->line](int_t& lhs, int_t rhs) { division_assert(rhs, line); lhs /= rhs; },
                    [](auto& lhs, auto rhs) { lhs /= rhs; }
            }, a.ref(lhs->line)->object, rhs->eval().val().object);
            return a;
        }},
        {1, RIGHT_TO_LEFT, INFIX, "%=",     intAssignment("%=", [](int_t& lhs, int_t rhs) { lhs %= rhs; }, division_assert)},
        {1, RIGHT_TO_LEFT, INFIX, "<<=",    intAssignment("<<=", [](int_t& lhs, int_t rhs) { lhs <<= rhs; }, shift_assert)},
        {1, RIGHT_TO_LEFT, INFIX, ">>=",    intAssignment(">>=", [](int_t& lhs, int_t rhs) { lhs >>= rhs; }, shift_assert)},
        {1, RIGHT_TO_LEFT, INFIX, "&=",     intAssignment("&=", [](int_t& lhs, int_t rhs) { lhs &= rhs; })},
        {1, RIGHT_TO_LEFT, INFIX, "^=",     intAssignment("^=", [](int_t& lhs, int_t rhs) { lhs ^= rhs; })},
        {1, RIGHT_TO_LEFT, INFIX, "|=",     intAssignment("|=", [](int_t& lhs, int_t rhs) { lhs |= rhs; })},
        {1, RIGHT_TO_LEFT, PREFIX, "print", [](ScriptEngine* engine, ExprNode* op) {
            return Number{op->eval().val().visit(overloaded {
                    [engine](int_t a) { return fprintf(engine->out, "%lld\n", a); },
                    [engine](real_t a) { return fprintf(engine->out, "%lg\n", a); }
            }) - 1};
        }},
        {1, RIGHT_TO_LEFT, PREFIX, "throw", [](ExprNode* op)->Operand { throw ScriptException{op->eval().val(), op->line}; }},
};

}