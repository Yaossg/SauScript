#include "SauScript.hpp"

namespace SauScript {

[[nodiscard]] auto tokenize(char const* source) {
    std::vector<Token> tokens;
    int line = 1;
    while (char const ch = *source) {
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
            } else if (auto&& ops = opTokens(); std::find(ops.begin(), ops.end(), token) != ops.end()) {
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
            } catch (...) {}
            try {
                size_t idx;
                tokens.push_back(Token::literal_real(std::stod(source, &idx)).at(line));
                source += idx;
            } catch (...) {
                throw RuntimeError("invalid literal number at line " + toLineString(line));
            }
        } else {
            std::string token;
            for (auto&& op : opTokens()) {
                if (std::string_view{source}.starts_with(op)) {
                    if (op.length() > token.length()) token = op;
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

std::unique_ptr<ExprNode> ScriptEngine::compileExpression(Token*& current, int level) {
    if (level > 13) {
        auto token = *current++;
        switch (token.type) {
            case TokenType::LITERAL_BOOL:
                return std::make_unique<ValNode>(token.line, Number{token.literal_bool()});
            case TokenType::LITERAL_INT:
                return std::make_unique<ValNode>(token.line, Number{token.literal_int()});
            case TokenType::LITERAL_REAL:
                return std::make_unique<ValNode>(token.line, Number{token.literal_real()});
            case TokenType::IDENTIFIER:
                return std::make_unique<RefNode>(this, token.line, token.identifier());
        }
        if (token == Token::parenLeft()) {
            auto expr = compileExpression(current, 0);
            if (*current++ != Token::parenRight()) throw SyntaxError("missing ')' to match '(' at line " + toLineString(current->line));
            return expr;
        }
        throw SyntaxError("invalid leaf node token for expression at line " + toLineString(current->line));
    }
    switch (Operator const* op; OP_LEVELS[level].operandType) {
        case OperandType::PREFIX: {
            if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                int line = current++->line;
                return std::make_unique<OpUnaryNode>(line, compileExpression(current, level), op->asUnary(this));
            }
            return compileExpression(current, level + 1);
        }
        case OperandType::POSTFIX: {
            auto expr = compileExpression(current, level + 1);
            if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                int line = current++->line;
                return std::make_unique<OpUnaryNode>(line, std::move(expr), op->asUnary(this));
            }
            return expr;
        }
        case OperandType::INFIX: {
            if (OP_LEVELS[level].associativity) {
                auto expr = compileExpression(current, level + 1);
                if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                    int line = current++->line;
                    return std::make_unique<OpBinaryNode>(line, std::move(expr), compileExpression(current, level), op->asBinary(this));
                }
                return expr;
            } else {
                auto expr = compileExpression(current, level + 1);
                while (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                    int line = current++->line;
                    expr = std::make_unique<OpBinaryNode>(line, std::move(expr), compileExpression(current, level + 1), op->asBinary(this));
                }
                return expr;
            }
        }
    }
    throw SyntaxError("Assertion failed");
}

std::unique_ptr<ExprNode> ScriptEngine::compileExpression(std::vector<Token> tokens) {
    if (tokens.empty()) throw RuntimeError("unexpected empty expression");
    tokens.push_back(Token::eot());
    Token* current = tokens.data();
    return compileExpression(current, 0);
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
auto simpleBinary(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        return std::visit([fn](auto lhs, auto rhs) { return Number{fn(lhs, rhs)}; }, lhs->eval().val().object, rhs->eval().val().object);
    };
}

template<typename Fn>
auto intBinary(char const* name, Fn fn, Assertion* an = noop_assert) {
    return [name, fn, an](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval().val().asIntOp(name, lhs->line);
        auto b = rhs->eval().val().asIntOp(name, rhs->line);
        an(b, rhs->line);
        return Number{fn(a, b)};
    };
}

template<typename Fn>
auto simpleAssignment(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval();
        std::visit(fn, a.ref(lhs->line)->object, rhs->eval().val().object);
        return a;
    };
}

template<typename Fn>
auto intAssignment(char const* name, Fn fn, Assertion* an = noop_assert) {
    return [name, fn, an](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval();
        int_t& r = a.ref(lhs->line)->asIntOp(name, lhs->line);
        int_t b = rhs->eval().val().asIntOp(name, lhs->line);
        an(b, rhs->line);
        fn(r, b);
        return a;
    };
}

const std::vector<Operator> OPERATORS[14] = {
        {
            {"print", [](ScriptEngine* engine, ExprNode* op) {
                return Number{op->eval().val().visit(overloaded {
                        [engine](int_t a) { return fprintf(engine->out, "%lld\n", a); },
                        [engine](real_t a) { return fprintf(engine->out, "%lg\n", a); }
                }) - 1};
            }},
            {"throw", [](ExprNode* op)->Operand { throw ScriptException{op->eval().val(), op->line}; }}
        },
        {
            {"=",  simpleAssignment([](auto& lhs, auto rhs) { lhs = rhs; })},
            {"+=", simpleAssignment([](auto& lhs, auto rhs) { lhs += rhs; })},
            {"-=", simpleAssignment([](auto& lhs, auto rhs) { lhs -= rhs; })},
            {"*=", simpleAssignment([](auto& lhs, auto rhs) { lhs *= rhs; })},
            {"/=", [](ExprNode* lhs, ExprNode* rhs) {
                auto a = lhs->eval();
                std::visit(overloaded {
                        [line = rhs->line](int_t& lhs, int_t rhs) { division_assert(rhs, line); lhs /= rhs; },
                        [](auto& lhs, auto rhs) { lhs /= rhs; }
                }, a.ref(lhs->line)->object, rhs->eval().val().object);
                return a;
            }},
            {"%=", intAssignment("%=", [](int_t& lhs, int_t rhs) { lhs %= rhs; }, division_assert)},
            {"<<=",intAssignment("<<=", [](int_t& lhs, int_t rhs) { lhs <<= rhs; }, shift_assert)},
            {">>=",intAssignment(">>=", [](int_t& lhs, int_t rhs) { lhs >>= rhs; }, shift_assert)},
            {"&=", intAssignment("&=", [](int_t& lhs, int_t rhs) { lhs &= rhs; })},
            {"^=", intAssignment("^=", [](int_t& lhs, int_t rhs) { lhs ^= rhs; })},
            {"|=", intAssignment("|=", [](int_t& lhs, int_t rhs) { lhs |= rhs; })}
        },
        {{"||", [](ExprNode *lhs, ExprNode *rhs) {
            return Number{lhs->eval().val().asBool(lhs->line) || rhs->eval().val().asBool(rhs->line)};
        }}},
        {{"&&", [](ExprNode *lhs, ExprNode *rhs) {
            return Number{lhs->eval().val().asBool(lhs->line) && rhs->eval().val().asBool(rhs->line)};
        }}},
        {{"|", intBinary("|", std::bit_or<int_t>{})}},
        {{"^", intBinary("^", std::bit_xor<int_t>{})}},
        {{"&", intBinary("&", std::bit_and<int_t>{})}},
        {
            {"==", simpleBinary(std::equal_to<>{})},
            {"!=", simpleBinary(std::not_equal_to<>{})}
        },
        {
            {"<",  simpleBinary(std::less<>{})},
            {">",  simpleBinary(std::greater<>{})},
            {"<=", simpleBinary(std::less_equal<>{})},
            {">=", simpleBinary(std::greater_equal<>{})}
        },
        {
            {"<<", intBinary("<<", [](int_t a, int_t b) { return a << b; }, shift_assert)},
            {">>", intBinary(">>", [](int_t a, int_t b) { return a >> b; }, shift_assert)}
        },
        {
            {"+", simpleBinary(std::plus<>{})},
            {"-", simpleBinary(std::minus<>{})}
        },
        {
            {"*", simpleBinary(std::multiplies<>{})},
            {"/", [](ExprNode* lhs, ExprNode* rhs) {
                return std::visit(overloaded {
                        [line = rhs->line](int_t lhs, int_t rhs) { division_assert(rhs, line); return Number{lhs / rhs}; },
                        [](auto lhs, auto rhs) { return Number{lhs / rhs}; }
                }, lhs->eval().val().object, rhs->eval().val().object);
            }},
            {"%", intBinary("%", std::modulus<int_t>{}, division_assert)}
        },
        {
            {"++", [](ExprNode* op) {
                auto a = op->eval();
                ++a.ref(op->line)->asIntOp("++", op->line);
                return a;
            }},
            {"--", [](ExprNode *op) {
                auto a = op->eval();
                --a.ref(op->line)->asIntOp("--", op->line);
                return a;
            }},
            {"+", [](ExprNode *op) { return op->eval().val(); }},
            {"-", [](ExprNode *op) {
                return op->eval().val().visit([](auto a) { return Number{-a}; });
            }},
            {"!", [](ExprNode *op) { return Number{!op->eval().val().asBool(op->line)}; }},
            {"~", [](ExprNode *op) { return Number{~op->eval().val().asIntOp("~", op->line)}; }},
            {"input", [](ScriptEngine *engine, ExprNode *op) {
                return Number{op->eval().ref(op->line)->visit(overloaded{
                        [engine](int_t &a) { return fscanf(engine->in, "%lld", &a); },
                        [engine](real_t &a) { return fscanf(engine->in, "%lf", &a); }
                })};
            }}
        },
        {
            {"++", [](ExprNode *op) {
                return Number{op->eval().ref(op->line)->asIntOp("++", op->line)++};
            }},
            {"--", [](ExprNode *op) {
                return Number{op->eval().ref(op->line)->asIntOp("--", op->line)--};
            }}
        }
};

}