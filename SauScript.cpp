#include "SauScript.hpp"

namespace SauScript {

[[nodiscard]] auto tokenize(char const* source) {
    std::vector<Token> tokens;
    int line = 1;
    while (char const ch = *source) {
        if (ch == '\\') {
            if (skipLineBreak(++source, true, line)) continue;
            throw SyntaxError("stray '\\'" + at(line));
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
        } else if (ch == '?' || ch == ':' || ch == ',') {
            tokens.push_back(Token::punctuation({ch}).at(line));
            ++source;
        } else if (isIdentifierStart(ch)) {
            char const* first = source;
            while (isIdentifierContinue(*++source));
            std::string token{first, source};
            if (token == "false") {
                tokens.push_back(Token::literal_bool(false).at(line));
            } else if (token == "true") {
                tokens.push_back(Token::literal_bool(true).at(line));
            } else if (token == "__LINE__") {
                tokens.push_back(Token::literal_int(line).at(line));
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
                throw RuntimeError("invalid literal number" + at(line));
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
                throw SyntaxError("unexpected token" + at(line));
            }
        }
    }
    tokens.push_back(Token::linebreak());
    tokens.push_back(Token::eot().at(line));
    return tokens;
}

Operand ScriptEngine::findOperand(const std::string& name, int line) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto&& scope = *it;
        if (scope.contains(name)) return &scope[name];
    }
    throw RuntimeError("reference to undefined variable '" + name + "'" + at(line));
}

std::unique_ptr<ExprNode> ScriptEngine::compileExpression(Token*& current, int level = 0) {
    if (level >= 13) {
        auto token = *current++;
        switch (token.type) {
            case TokenType::LITERAL_BOOL:
                return std::make_unique<ValNode>(token.line, Object{token.literal_bool()});
            case TokenType::LITERAL_INT:
                return std::make_unique<ValNode>(token.line, Object{token.literal_int()});
            case TokenType::LITERAL_REAL:
                return std::make_unique<ValNode>(token.line, Object{token.literal_real()});
            case TokenType::IDENTIFIER:
                return std::make_unique<RefNode>(this, token.line, token.identifier());
        }
        if (token == Token::parenLeft()) {
            auto expr = compileExpression(current);
            if (*current++ != Token::parenRight())
                throw SyntaxError("missing ')' to match '('" + current->at());
            return expr;
        }
        throw SyntaxError("unexpected leaf node token for expression" + current->at());
    }
    switch (Operator const* op; OP_LEVELS[level].operandType) {
        case OperandType::PREFIX: {
            if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                int line = current++->line;
                return std::make_unique<OpUnaryNode>(line, compileExpression(current, level), op);
            }
            return compileExpression(current, level + 1);
        }
        case OperandType::POSTFIX: {
            auto expr = compileExpression(current, level + 1);
            while (true) {
                if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                    int line = current++->line;
                    expr = std::make_unique<OpUnaryNode>(line, std::move(expr), op);
                } else if (*current == Token::parenLeft()) {
                    int line = current++->line;
                    std::vector<std::unique_ptr<ExprNode>> args;
                    while (true) {
                        if (*current == Token::parenRight()) break;
                        args.push_back(compileExpression(current));
                        if (*current == Token::parenRight()) break;
                        if (*current++ != Token::punctuation(","))
                            throw SyntaxError("unexpected token interrupt function invocation" + current->at());
                    }
                    ++current;
                    expr = std::make_unique<OpInvokeNode>(line, this, std::move(expr), std::move(args));
                } else break;
            }
            return expr;
        }
        case OperandType::INFIX: {
            if (OP_LEVELS[level].associativity) {
                auto expr = compileExpression(current, level + 1);
                if (current->type == TokenType::PUNCTUATION) {
                    if ((op = findOperator(current->punctuation(), level))) {
                        int line = current++->line;
                        return std::make_unique<OpBinaryNode>(line, std::move(expr), compileExpression(current, level), op);
                    } else if (level == 0 && *current == Token::punctuation("?")) {
                        int line = current++->line;
                        auto lhs = compileExpression(current, level);
                        if (*current == Token::punctuation(":")) {
                            auto rhs = compileExpression(++current, level);
                            return std::make_unique<OpTernaryNode>(line, std::move(expr), std::move(lhs), std::move(rhs));
                        } else {
                            throw SyntaxError("missing ':' for '?'" + at(line));
                        }
                    }
                }
                return expr;
            } else {
                auto expr = compileExpression(current, level + 1);
                while (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                    int line = current++->line;
                    expr = std::make_unique<OpBinaryNode>(line, std::move(expr), compileExpression(current, level + 1), op);
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
    return compileExpression(current);
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
                    throw SyntaxError("bad del statement" + first.at());
                }
            case LET:
                if (tokens.size() >= 3 && tokens[1].type == TokenType::IDENTIFIER
                    && tokens[2] == Token::punctuation("=")) {
                    std::string id = tokens[1].identifier();
                    tokens.erase(tokens.begin(), tokens.begin() + 3);
                    return std::make_unique<LetNode>(this, first.line, id, compileExpression(tokens));
                } else {
                    throw SyntaxError("bad let statement" + first.at());
                }
            case BREAK:
                if (tokens.size() == 1) {
                    return std::make_unique<BreakNode>(first.line);
                } else {
                    throw SyntaxError("bad break statement" + first.at());
                }
            case CONTINUE:
                if (tokens.size() == 1) {
                    return std::make_unique<ContinueNode>(first.line);
                } else {
                    throw SyntaxError("bad continue statement" + first.at());
                }
            case THROW:
                if (tokens.size() > 1) {
                    tokens.erase(tokens.begin());
                    return std::make_unique<ThrowNode>(first.line, compileExpression(tokens));
                } else {
                    throw SyntaxError("bad throw statement" + first.at());
                }
            case PRINT:
                if (tokens.size() > 1) {
                    tokens.erase(tokens.begin());
                    return std::make_unique<PrintNode>(this, compileExpression(tokens));
                } else {
                    throw SyntaxError("bad print statement" + first.at());
                }
            case INPUT:
                if (tokens.size() == 2 && tokens[1].type == TokenType::IDENTIFIER) {
                    return std::make_unique<InputNode>(this, first.line, tokens[1].identifier());
                } else {
                    throw SyntaxError("bad input statement" + first.at());
                }
            case RETURN:
                if (tokens.size() > 1) {
                    tokens.erase(tokens.begin());
                    return std::make_unique<ReturnNode>(first.line, compileExpression(tokens));
                } else {
                    return std::make_unique<ReturnNode>(first.line, std::make_unique<ValNode>(first.line, Object{std::monostate()}));
                }
        }
    }
    return std::make_unique<StmtExprNode>(compileExpression(tokens));
}

std::unique_ptr<StmtNode> ScriptEngine::compileWhile(Token*& current) {
    using namespace Keyword;
    Token* first = ++current;
    while (*current != Token::keyword(DO)) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing do in while" + first->at());
        ++current;
    }
    auto cond = compileExpression({first, current});
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in while" + current->at());
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
                    if (current->type == TokenType::EOT) throw SyntaxError("missing end in repeat" + current->at());
                    ++current;
                }
                return std::make_unique<RepeatNode>(this, std::move(stmt), compileExpression({first, current}));
            }
        }
    throw SyntaxError("end or until is expected" + current->at());
}

std::unique_ptr<StmtNode> ScriptEngine::compileFor(Token*& current) {
    using namespace Keyword;
    Token* first = ++current;
    while (*current != Token::linebreak()) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing first linebreak in for" + first->at());
        ++current;
    }
    auto init = compileStatement({first, current});
    first = ++current;
    while (*current != Token::linebreak()) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing second linebreak in for" + first->at());
        ++current;
    }
    auto cond = compileExpression({first, current});
    first = ++current;
    while (*current != Token::keyword(DO)) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing do in for" + first->at());
        ++current;
    }
    auto iter = compileExpression({first, current});
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in for" + current->at());
    return std::make_unique<ForNode>(this, std::move(init), std::move(cond), std::move(iter), std::move(stmt));
}

std::unique_ptr<StmtNode> ScriptEngine::compileDo(Token*& current) {
    using namespace Keyword;
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in do" + current->at());
    return std::make_unique<DoNode>(this, std::move(stmt));
}

std::unique_ptr<StmtNode> ScriptEngine::compileIf(Token*& current) {
    using namespace Keyword;
    Token* first = ++current;
    while (*current != Token::keyword(THEN)) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing then in if" + first->at());
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
                if (*current != Token::keyword(END)) throw SyntaxError("missing end in if" + current->at());
                return std::make_unique<IfNode>(this, std::move(cond), std::move(then), std::move(else_));
            }
            case ELIF: {
                *current = Token::keyword(IF);
                return std::make_unique<IfNode>(this, std::move(cond), std::move(then), compileIf(current));
            }
        }
    throw SyntaxError("end, else or elif is expected" + current->at());
}

std::unique_ptr<StmtNode> ScriptEngine::compileTry(Token*& current) {
    using namespace Keyword;
    auto try_ = compileStatements(++current);
    if (*current != Token::keyword(CATCH)) throw SyntaxError("missing catch" + current->at());
    if ((++current)->type != TokenType::IDENTIFIER) throw SyntaxError("missing catch identifier" + current->at());
    auto name = current->identifier();
    auto catch_ = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in try" + current->at());
    return std::make_unique<TryNode>(this, std::move(try_), name, std::move(catch_));
}

std::unique_ptr<StmtNode> ScriptEngine::compileFunction(Token*& current) {
    int line = current++->line;
    if (current->type != TokenType::IDENTIFIER)
        throw SyntaxError("name of function expected" + current->at());
    std::string name = current++->identifier();
    if (*current++ != Token::parenLeft())
        throw SyntaxError("left parenthesis of function expected" + current->at());
    std::vector<Parameter> parameters;
    while (true) {
        if (*current == Token::parenRight()) break;
        if (current->type != TokenType::IDENTIFIER)
            throw SyntaxError("name of parameter expected" + current->at());
        std::string p_name = current++->identifier();
        if (*current++ != Token::punctuation(":"))
            throw SyntaxError("expected ':' after parameter name" + current->at());
        Type p_type = parseType(*current++);
        parameters.push_back({p_type, p_name});
        if (*current == Token::parenRight()) break;
        if (*current++ != Token::punctuation(","))
            throw SyntaxError("unexpected token interrupt function definition" + current->at());
    }
    if (*++current != Token::punctuation(":"))
        throw SyntaxError("expected ':' after parameter list" + current->at());
    Type return_type = parseType(*++current);
    std::unique_ptr<StmtNode> stmt;
    if (*++current == Token::punctuation("=")) {
        auto expr = compileExpression(++current);
        stmt = std::make_unique<ReturnNode>(expr->line, std::move(expr));
    } else {
        stmt = compileStatements(current);
        if (*current != Token::keyword(Keyword::END))
            throw SyntaxError("missing end in function" + current->at());
    }
    return std::make_unique<LetNode>(this, line, name, std::make_unique<ValNode>(
            line, Object{std::make_shared<Function>(Function{return_type, parameters, std::move(stmt)})}));
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
                    case FUNCTION:
                        stmts.push_back(compileFunction(current));
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
    } catch (ScriptReturn& e) {
        fprintf(out, "Script returned: %s\n", e.returned.toString().c_str());
    } catch (ScriptBreak& e) {
        fprintf(err, "Wild break%s\n", at(e.line).c_str());
    } catch (ScriptContinue& e) {
        fprintf(err, "Wild continue%s\n", at(e.line).c_str());
    } catch (ScriptException& caught) {
        fprintf(err, caught.what());
    } catch (SyntaxError& e) {
        fprintf(err, "Syntax error: %s\n", e.what());
    } catch (RuntimeError& e) {
        fprintf(err, "Runtime error: %s\n", e.what());
    }
}

Object Object::invoke(int line, ScriptEngine* engine, const std::vector<Object> &arguments) {
    if (!std::holds_alternative<FuncPtr>(object))
        throw RuntimeError("not a function" + at(line));
    auto&& fn = *std::get<FuncPtr>(object);
    if (fn.parameters.size() != arguments.size())
        throw RuntimeError("wrong number of arguments" + at(line));
    try {
        ScriptScope scope(engine);
        for (int i = 0; i < arguments.size(); ++i) {
            auto&& parameter = fn.parameters[i];
            auto&& argument = arguments[i];
            if (parameter.type == Type::REAL && argument.type() == Type::INT) {
                engine->local()[parameter.name] = argument.promote(line);
            } else if (parameter.type == argument.type()) {
                engine->local()[parameter.name] = argument;
            } else {
                throw RuntimeError("wrong type of the " + std::to_string(i + 1) + "th argument" + at(line));
            }
        }
        fn.stmt->exec();
    } catch (ScriptReturn& e) {
        if (e.returned.type() == Type::INT && fn.returnType == Type::REAL) {
            return e.returned.promote(e.line);
        } else if (e.returned.type() == fn.returnType) {
            return e.returned;
        } else {
            throw RuntimeError("invalid return type" + at(e.line));
        }
    }
    if (fn.returnType == Type::VOID)
        return Object{std::monostate()};
    throw RuntimeError("non-void function returns nothing" + at(line));
}

}