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
        } else {
            switch (ch) {
                case '(':
                    tokens.push_back(Token::parenLeft().at(line));
                    ++source;
                    continue;
                case ')':
                    tokens.push_back(Token::parenRight().at(line));
                    ++source;
                    continue;
                case '{':
                    tokens.push_back(Token::braceLeft().at(line));
                    ++source;
                    continue;
                case '}':
                    tokens.push_back(Token::braceRight().at(line));
                    ++source;
                    continue;
            }
            if (isIdentifierStart(ch)) {
                char const *first = source;
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
                } else if (auto &&ops = opTokens(); std::find(ops.begin(), ops.end(), token) != ops.end()) {
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
                for (auto &&op: opTokens()) {
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
    switch (Operator const* op; level) {
        case LEVEL_PRIMARY:
            switch (auto token = *current++; token.type) {
                case TokenType::LITERAL_BOOL:
                    return std::make_unique<ValNode>(this, token.line, Object{token.literal_bool()});
                case TokenType::LITERAL_INT:
                    return std::make_unique<ValNode>(this, token.line, Object{token.literal_int()});
                case TokenType::LITERAL_REAL:
                    return std::make_unique<ValNode>(this, token.line, Object{token.literal_real()});
                case TokenType::IDENTIFIER:
                    return std::make_unique<RefNode>(this, token.line, token.identifier());
                case TokenType::KEYWORD: {
                    using namespace Keyword;
                    switch (token.keyword()) {
                        case FUNCTION:
                            return compileFunction(current);
                        case FOR:
                            return compileFor(current);
                        case WHILE:
                            return compileWhile(current);
                        case DO:
                            return compileDoWhile(current);
                        case IF:
                            return compileIfElse(current);
                        case TRY:
                            return compileTry(current);
                        case BREAK:
                            return std::make_unique<JumpNode>(this, token.line, JumpTarget::BREAK);
                        case CONTINUE:
                            return std::make_unique<JumpNode>(this, token.line, JumpTarget::CONTINUE);
                        case CATCH:
                            throw SyntaxError("stray catch" + at(token.line));
                        case ELSE:
                            throw SyntaxError("stray else" + at(token.line));
                    }
                }
                case TokenType::BRACE:
                    if (token == Token::braceLeft()) {
                        auto stmt = compileStatements(current);
                        if (*current != Token::braceRight())
                            throw SyntaxError("missing '}' to match '{'" + current->at());
                        ++current;
                        return stmt;
                    }
                case TokenType::PAREN:
                    if (token == Token::parenLeft()) {
                        auto expr = compileExpression(current);
                        if (*current != Token::parenRight())
                            throw SyntaxError("missing ')' to match '('" + current->at());
                        ++current;
                        return expr;
                    }
                default:
                    throw SyntaxError("unexpected leaf node token for expression" + token.at());
            }
        case LEVEL_UNARY_PREFIX: {
            if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                int line = current++->line;
                return std::make_unique<OpUnaryNode>(this, line, compileExpression(current, level), op);
            }
            return compileExpression(current, level + 1);
        }
        case LEVEL_UNARY_POSTFIX: {
            auto expr = compileExpression(current, level + 1);
            while (true) {
                if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                    int line = current++->line;
                    expr = std::make_unique<OpUnaryNode>(this, line, std::move(expr), op);
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
        case LEVEL_ROOT: {
            if (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                int line = current++->line;
                return std::make_unique<OpUnaryNode>(this, line, compileExpression(current, level), op);
            }
            auto expr = compileExpression(current, level + 1);
            if (current->type == TokenType::PUNCTUATION) {
                if ((op = findOperator(current->punctuation(), level))) {
                    int line = current++->line;
                    return std::make_unique<OpBinaryNode>(this, line, std::move(expr), compileExpression(current, level), op);
                }
                if (*current == Token::punctuation("?")) {
                    int line = current++->line;
                    auto lhs = compileExpression(current, level);
                    if (*current == Token::punctuation(":")) {
                        auto rhs = compileExpression(++current, level);
                        return std::make_unique<OpTernaryNode>(this, line, std::move(expr), std::move(lhs), std::move(rhs));
                    } else {
                        throw SyntaxError("missing ':' for '?'" + at(line));
                    }
                }
            }
            return expr;
        }
        default: {
            auto expr = compileExpression(current, level + 1);
            while (current->type == TokenType::PUNCTUATION && (op = findOperator(current->punctuation(), level))) {
                int line = current++->line;
                expr = std::make_unique<OpBinaryNode>(this, line, std::move(expr),
                                                      compileExpression(current, level + 1), op);
            }
            return expr;
        }
    }
}

std::unique_ptr<ExprNode> ScriptEngine::compileWhile(Token*& current) {
    using namespace Keyword;
    int line = (--current)->line;
    auto cond = compileExpression(++current);
    if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in while" + current->at());
    auto stmt = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in while" + current->at());
    ++current;
    return std::make_unique<WhileNode>(this, line, std::move(cond), std::move(stmt));
}

std::unique_ptr<ExprNode> ScriptEngine::compileDoWhile(Token*& current) {
    using namespace Keyword;
    int line = (--current)->line;
    if (*++current != Token::braceLeft()) throw SyntaxError("missing '{' in do-while" + current->at());
    auto stmt = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in do-while" + current->at());
    if (*++current != Token::keyword(WHILE)) throw SyntaxError("missing 'while' in do-while" + current->at());
    auto cond = compileExpression(current);
    ++current;
    return std::make_unique<DoWhileNode>(this, line, std::move(stmt), std::move(cond));
}

std::unique_ptr<ExprNode> ScriptEngine::compileFor(Token*& current) {
    using namespace Keyword;
    int line = (--current)->line;
    auto init = compileExpression(++current);
    if (*current != Token::linebreak()) throw SyntaxError("missing first linebreak in for" + current->at());
    auto cond = compileExpression(++current);
    if (*current != Token::linebreak()) throw SyntaxError("missing second linebreak in for" + current->at());
    auto iter = compileExpression(++current);
    if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in for" + current->at());
    auto stmt = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in for" + current->at());
    ++current;
    return std::make_unique<ForNode>(this, line, std::move(init), std::move(cond), std::move(iter), std::move(stmt));
}

std::unique_ptr<ExprNode> ScriptEngine::compileIfElse(Token*& current) {
    using namespace Keyword;
    int line = (--current)->line;
    auto cond = compileExpression(++current);
    if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in if" + current->at());
    auto then = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in if" + current->at());
    std::unique_ptr<ExprNode> else_ = std::make_unique<NoopNode>(this, 0);
    if (current[1] == Token::keyword(ELSE)) {
        current += 2;
        if (*current == Token::keyword(IF)) {
            else_ = compileIfElse(++current);
            --current;
        } else {
            if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in else" + current->at());
            else_ = compileStatements(++current);
            if (*current != Token::braceRight()) throw SyntaxError("missing '}' in else" + current->at());
        }
    }
    ++current;
    return std::make_unique<IfElseNode>(this, line, std::move(cond), std::move(then), std::move(else_));
}

std::unique_ptr<ExprNode> ScriptEngine::compileTry(Token*& current) {
    using namespace Keyword;
    int line = (--current)->line;
    if (*++current != Token::braceLeft()) throw SyntaxError("missing '{' in try" + current->at());
    auto try_ = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in try" + current->at());
    if (*++current != Token::keyword(CATCH)) throw SyntaxError("missing catch in try" + current->at());
    if ((++current)->type != TokenType::IDENTIFIER) throw SyntaxError("missing catch identifier" + current->at());
    auto name = current->identifier();
    if (*++current != Token::braceLeft()) throw SyntaxError("missing '{' in catch" + current->at());
    auto catch_ = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing end in try" + current->at());
    ++current;
    return std::make_unique<TryNode>(this, line, std::move(try_), name, std::move(catch_));
}

std::unique_ptr<ExprNode> ScriptEngine::compileFunction(Token*& current) {
    int line = (--current)->line;
    if (*++current != Token::parenLeft())
        throw SyntaxError("left parenthesis of function expected" + (--current)->at());
    std::vector<Parameter> parameters;
    ++current;
    while (true) {
        if (*current == Token::parenRight()) break;
        if (current->type != TokenType::IDENTIFIER)
            throw SyntaxError("name of parameter expected" + current->at());
        std::string p_name = current++->identifier();
        if (*current++ != Token::punctuation(":"))
            throw SyntaxError("expected ':' after parameter name" + (--current)->at());
        Type p_type = parseType(*current++);
        parameters.push_back({p_type, p_name});
        if (*current == Token::parenRight()) break;
        if (*current++ != Token::punctuation(","))
            throw SyntaxError("unexpected token interrupt function definition" + (--current)->at());
    }
    if (*++current != Token::punctuation(":"))
        throw SyntaxError("expected ':' after parameter list" + (--current)->at());
    Type return_type = parseType(*++current);
    std::unique_ptr<ExprNode> stmt;
    if (*++current == Token::punctuation("=")) {
        auto expr = compileExpression(++current);
        stmt = std::make_unique<OpUnaryNode>(this, expr->line, std::move(expr), findOperator("return", LEVEL_ROOT));
    } else {
        if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in function" + current->at());
        stmt = compileStatements(++current);
        if (*current != Token::braceRight()) throw SyntaxError("missing '}' in function" + current->at());
        ++current;
    }
    return std::make_unique<ValNode>(this, line, Object{
        std::make_shared<Function>(Function{return_type, parameters, std::move(stmt)})});
}

std::unique_ptr<ExprNode> ScriptEngine::compileStatements(Token*& current) {
    std::vector<std::unique_ptr<ExprNode>> stmts;
    while (current->type != TokenType::EOT) {
        if (*current == Token::braceRight()) { break; }
        if (current->type == TokenType::LINEBREAK) { ++current; continue; }
        stmts.push_back(compileExpression(current));
    }
    int line = stmts.empty() ? 0 : stmts[0]->line;
    return std::make_unique<StmtsNode>(this, line, std::move(stmts));
}

std::unique_ptr<ExprNode> ScriptEngine::compile(const char* script) {
    auto tokens = tokenize(script);
    Token* current = tokens.data();
    return compileStatements(current);
}

void ScriptEngine::exec(const char* script, FILE* err) {
    try {
        Operand ret = compile(script)->exec();
        if (jumpTarget == JumpTarget::RETURN) ret = target;
        switch (jumpTarget) {
            case JumpTarget::THROW:
                fprintf(err, "Unhandled exception: %s", target.toString().c_str());
                break;
            case JumpTarget::BREAK:
                throw RuntimeError("Wild break jump" + at(jumpFrom));
            case JumpTarget::CONTINUE:
                throw RuntimeError("Wild continue jump" + at(jumpFrom));
            case JumpTarget::RETURN:
            case JumpTarget::NONE:
                if (ret.val().type() != Type::VOID)
                    fprintf(out, "%s\n", ret.val().toString().c_str());
        }
    } catch (SyntaxError& e) {
        fprintf(err, "Syntax error: %s\n", e.what());
    } catch (RuntimeError& e) {
        fprintf(err, "Runtime error: %s\n", e.what());
    }
    jumpTarget = JumpTarget::NONE;
}

Object Object::invoke(ScriptEngine* engine, int line, const std::vector<Object> &arguments) {
    if (!std::holds_alternative<FuncPtr>(object))
        throw RuntimeError("not a function" + at(line));
    auto&& fn = *std::get<FuncPtr>(object);
    if (fn.parameters.size() != arguments.size())
        throw RuntimeError("wrong number of arguments" + at(line));
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
    switch (engine->jumpTarget) {
        case JumpTarget::RETURN: {
            engine->jumpTarget = JumpTarget::NONE;
            int line = engine->jumpFrom;
            Object returned = engine->target;
            if (returned.type() == Type::INT && fn.returnType == Type::REAL) {
                return returned.promote(line);
            } else if (returned.type() == fn.returnType) {
                return returned;
            } else {
                throw RuntimeError("invalid return type" + at(line));
            }
        }
        default:
        case JumpTarget::THROW:
            throw Interruption{};
        case JumpTarget::BREAK:
            throw RuntimeError("Wild break jump" + at(engine->jumpFrom));
        case JumpTarget::CONTINUE:
            throw RuntimeError("Wild continue jump" + at(engine->jumpFrom));
        case JumpTarget::NONE:
            if (fn.returnType == Type::VOID) return {};
            else throw RuntimeError("non-void function returns nothing" + at(line));
    }
}

std::string Object::toString() const {
    return std::visit(overloaded {
            [](auto x) { return std::to_string(x); },
            [](std::monostate) { return std::string("<void>"); },
            [](FuncPtr const& ptr) { return ptr->toString(); }
    }, object);
}

std::string Function::toString() const {
    std::string ret = descriptor();
    ret += "{";
    ret += stmt->toString();
    ret += "}";
    return ret;
}
}