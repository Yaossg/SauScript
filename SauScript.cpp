#include "SauScript.hpp"

namespace SauScript {

[[nodiscard]] auto tokenize(char const* current) {
    std::vector<Token> tokens;
    int line = 1;
    while (char const ch = *current) {
        if (ch == '\\') {
            if (skipLineBreak(++current, true, line)) continue;
            throw SyntaxError("stray '\\'" + at(line));
        } else if (skipLineBreak(current, false, line)) {
            tokens.push_back(Token::linebreak().at(line - 1));
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
                } else if (token == "nan") {
                    tokens.push_back(Token::literal_real(0.0 / 0.0).at(line));
                } else if (token == "inf") {
                    tokens.push_back(Token::literal_real(1.0 / 0.0).at(line));
                } else if (auto kw = Keyword::parse(token); kw != Keyword::NAK) {
                    tokens.push_back(Token::keyword(kw).at(line));
                } else if (auto &&ops = opTokens(); std::find(ops.begin(), ops.end(), token) != ops.end()) {
                    tokens.push_back(Token::punctuator(token).at(line));
                } else {
                    tokens.push_back(Token::identifier(token).at(line));
                }
            } else if (isNumberStart(ch)) {
                try {
                    size_t idx;
                    auto x = std::stoull(current, &idx, 0);
                    if (current[idx] != '.' && current[idx] != 'e' && current[idx] != 'E' && current[idx] != 'p' && current[idx] != 'P') {
                        tokens.push_back(Token::literal_int(int_t(x)).at(line));
                        current += idx;
                        continue;
                    }
                } catch (...) {}
                try {
                    size_t idx;
                    tokens.push_back(Token::literal_real(std::stod(current, &idx)).at(line));
                    current += idx;
                } catch (...) {
                    throw RuntimeError("invalid literal number" + at(line));
                }
            } else {
                std::string token;
                for (auto &&op: opTokens()) {
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

Operand ScriptEngine::findOperand(const std::string& name, int line) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto&& scope = *it;
        if (scope.contains(name)) return &scope[name];
    }
    throw RuntimeError("reference to undefined variable '" + name + "'" + at(line));
}

std::unique_ptr<ExprNode> ScriptEngine::compileExpression(Token*& current, int level = LEVEL_ROOT) {
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
                            return compileTryCatch(current);
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
                case TokenType::LINEBREAK:
                    throw RuntimeError("unexpected linebreak" + at(token.line));
                default:
                    if (token == Token::braceLeft()) {
                        auto stmt = compileStatements(current);
                        if (*current != Token::braceRight())
                            throw SyntaxError("missing '}' to match '{'" + current->at());
                        ++current;
                        return stmt;
                    } else if (token == Token::bracketLeft()) {
                        std::vector<std::unique_ptr<ExprNode>> objs;
                        while (true) {
                            if (*current == Token::bracketRight()) break;
                            objs.push_back(compileExpression(current));
                            if (*current == Token::bracketRight()) break;
                            if (*current++ != Token::punctuator(","))
                                throw SyntaxError("unexpected token interrupt list literal" + current->at());
                        }
                        ++current;
                        return std::make_unique<OpListNode>(this, token.line, std::move(objs));
                    } else if (token == Token::parenLeft()) {
                        auto expr = compileExpression(current);
                        if (*current != Token::parenRight())
                            throw SyntaxError("missing ')' to match '('" + current->at());
                        ++current;
                        return expr;
                    }
                    throw SyntaxError("unexpected leaf node token for expression" + token.at());
            }
        case LEVEL_UNARY_PREFIX: {
            if (current->type == TokenType::PUNCTUATOR && (op = findOperator(current->punctuator(), level))) {
                int line = current++->line;
                return std::make_unique<OpUnaryNode>(this, line, compileExpression(current, level), op);
            }
            return compileExpression(current, level + 1);
        }
        case LEVEL_UNARY_POSTFIX: {
            auto expr = compileExpression(current, level + 1);
            while (true) {
                if (current->type == TokenType::PUNCTUATOR && (op = findOperator(current->punctuator(), level))) {
                    int line = current++->line;
                    expr = std::make_unique<OpUnaryNode>(this, line, std::move(expr), op);
                } else if (*current == Token::parenLeft()) {
                    int line = current++->line;
                    std::vector<std::unique_ptr<ExprNode>> args;
                    while (true) {
                        if (*current == Token::parenRight()) break;
                        args.push_back(compileExpression(current));
                        if (*current == Token::parenRight()) break;
                        if (*current++ != Token::punctuator(","))
                            throw SyntaxError("unexpected token interrupt function invocation" + current->at());
                    }
                    ++current;
                    expr = std::make_unique<OpInvokeNode>(this, line, std::move(expr), std::move(args));
                } else if (*current == Token::bracketLeft()) {
                    int line = current++->line;
                    auto index = compileExpression(current);
                    if (*current != Token::bracketRight())
                        throw SyntaxError("missing ']' to match '['" + current->at());
                    ++current;
                    expr = std::make_unique<OpIndexNode>(this, line, std::move(expr), std::move(index));
                } break;
            }
            return expr;
        }
        case LEVEL_ROOT: {
            if (current->type == TokenType::PUNCTUATOR && (op = findOperator(current->punctuator(), level))) {
                int line = current++->line;
                return std::make_unique<OpUnaryNode>(this, line, compileExpression(current, level), op);
            }
            auto expr = compileExpression(current, level + 1);
            if (current->type == TokenType::PUNCTUATOR) {
                if ((op = findOperator(current->punctuator(), level))) {
                    int line = current++->line;
                    return std::make_unique<OpBinaryNode>(this, line, std::move(expr), compileExpression(current, level), op);
                }
                if (*current == Token::punctuator("?")) {
                    int line = current++->line;
                    auto lhs = compileExpression(current, level);
                    if (*current == Token::punctuator(":")) {
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
            while (current->type == TokenType::PUNCTUATOR && (op = findOperator(current->punctuator(), level))) {
                int line = current++->line;
                if (current->type == TokenType::PUNCTUATOR && findOperator(current->punctuator(), LEVEL_ROOT))
                    return std::make_unique<OpBinaryNode>(this, line, std::move(expr),
                                                          compileExpression(current), op);
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
    auto cond = compileExpression(++current);
    if (*current != Token::linebreak()) throw SyntaxError("missing linebreak in do-while" + current->at());
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

std::unique_ptr<ExprNode> ScriptEngine::compileTryCatch(Token*& current) {
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
    return std::make_unique<TryCatchNode>(this, line, std::move(try_), name, std::move(catch_));
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
        if (*current++ != Token::punctuator(":"))
            throw SyntaxError("expected ':' after parameter name" + (--current)->at());
        Type p_type = parseType(*current++);
        parameters.push_back({p_type, p_name});
        if (*current == Token::parenRight()) break;
        if (*current++ != Token::punctuator(","))
            throw SyntaxError("unexpected token interrupt function definition" + (--current)->at());
    }
    if (*++current != Token::punctuator(":"))
        throw SyntaxError("expected ':' after parameter list" + current->at());
    Type return_type = parseType(*++current);
    std::unique_ptr<ExprNode> stmt;
    if (*++current != Token::punctuator("="))
        throw SyntaxError("expected '=' after return type" + current->at());
    stmt = compileExpression(++current);
    return std::make_unique<ValNode>(this, line, Object{
        std::make_shared<Function>(Function{return_type, parameters, std::move(stmt)})});
}

std::unique_ptr<StmtsNode> ScriptEngine::compileStatements(Token*& current) {
    std::vector<std::unique_ptr<ExprNode>> stmts;
    while (true) {
        if (*current == Token::terminator()) { break; }
        if (*current == Token::braceRight()) { break; }
        if (*current == Token::linebreak()) { ++current; continue; }
        stmts.push_back(compileExpression(current));
    }
    int line = stmts.empty() ? 0 : stmts[0]->line;
    return std::make_unique<StmtsNode>(this, line, std::move(stmts));
}

std::unique_ptr<StmtsNode> ScriptEngine::compile(const char* script) {
    auto tokens = tokenize(script);
    Token* current = tokens.data();
    auto stmts = compileStatements(current);
    if (*current != Token::terminator()) throw RuntimeError("stray tokens" + at(current->line));
    return stmts;
}

void ScriptEngine::exec(const char* script, FILE* err) {
    try {
        compile(script)->push_repl();
        Operand ret;
        switch (jumpTarget) {
            case JumpTarget::THROW:
                fprintf(err, "Unhandled exception: %s", target.toString().c_str());
                break;
            case JumpTarget::BREAK:
                throw RuntimeError("Wild break jump" + at(jumpFrom));
            case JumpTarget::CONTINUE:
                throw RuntimeError("Wild continue jump" + at(jumpFrom));
            case JumpTarget::RETURN:
                ret = target;
                break;
            case JumpTarget::NONE:
                ret = pop();
                break;
        }
        if (ret.val().type() != Type::VOID)
            fprintf(out, "%s\n", ret.val().toString().c_str());
    } catch (SyntaxError& e) {
        fprintf(err, "Syntax error: %s\n", e.what());
    } catch (RuntimeError& e) {
        fprintf(err, "Runtime error: %s\n", e.what());
    }
    if (!stack.empty()) {
        fprintf(err, "Memory leaked: %d\n", stack.size());
        while (!stack.empty()) {
            fprintf(err, "Remains: %s\n", stack.top().val().toString().c_str());
            stack.pop();
        }
    }
    jumpTarget = JumpTarget::NONE;
}

void Object::invoke(ScriptEngine* engine, int line, std::vector<Object> const& arguments) const {
    func_t fn;
    switch (type()) {
        case Type::FUNC: {
            fn = std::get<func_t>(object);
            if (fn->parameters.size() != arguments.size())
                throw RuntimeError("expected " + std::to_string(fn->parameters.size()) + "argument(s) but got "
                    + std::to_string(arguments.size()) + at(line));
            for (int i = 0; i < arguments.size(); ++i) {
                auto&& parameter = fn->parameters[i];
                auto&& argument = arguments[i];
                if (!(parameter.type == argument.type() || argument.type() == Type::INT && parameter.type == Type::REAL))
                    throw RuntimeError("mismatched type of the " + std::to_string(i + 1) + "th argument, expected "
                        + parameter.type_name() + " but got " + argument.type_name() + at(line));
            }
        } break;
        case Type::LIST: {
            std::vector<func_t> candidates;
            int least_promoted = arguments.size();
            for (auto&& obj : *std::get<list_t>(object)) {
                if (obj.type() != Type::FUNC) goto mismatch;
                if (func_t candidate = get<func_t>(obj.object); candidate->parameters.size() == arguments.size()) {
                    int promoted = 0;
                    for (int i = 0; i < arguments.size(); ++i) {
                        auto&& parameter = candidate->parameters[i];
                        auto&& argument = arguments[i];
                        if (argument.type() == Type::INT && parameter.type == Type::REAL) {
                            ++promoted;
                        } else if (parameter.type != argument.type()) {
                            goto mismatch;
                        }
                    }
                    if (promoted < least_promoted) {
                        least_promoted = promoted;
                        candidates.clear();
                        candidates.push_back(candidate);
                    } else if (promoted == least_promoted) {
                        candidates.push_back(candidate);
                    }
                }
                mismatch: ;
            }
            if (candidates.empty())
                throw RuntimeError("no function is matched in the set of overloads" + at(line));
            if (candidates.size() > 1)
                throw RuntimeError("multiple functions are matched in the set of overloads" + at(line));
            fn = candidates.front();
        } break;
        default:
            throw RuntimeError("not invocable" + at(line));
    }
    ScriptScope scope(engine);
    for (int i = 0; i < arguments.size(); ++i) {
        auto&& parameter = fn->parameters[i];
        auto&& argument = arguments[i];
        if (argument.type() == Type::INT && parameter.type == Type::REAL) {
            engine->local()[parameter.name] = argument.promote(line);
        } else if (parameter.type == argument.type()) {
            engine->local()[parameter.name] = argument;
        } else {
            throw RuntimeError("Assertion failed");
        }
    }
    fn->stmt->push();
    switch (engine->jumpTarget) {
        case JumpTarget::RETURN: {
            engine->jumpTarget = JumpTarget::NONE;
            int line = engine->jumpFrom;
            Object returned = engine->target;
            if (returned.type() == Type::INT && fn->returnType == Type::REAL) {
                engine->push(returned.promote(line));
            } else if (returned.type() == fn->returnType) {
                engine->push(returned);
            } else {
                throw RuntimeError("invalid return type" + at(line));
            }
        }
        case JumpTarget::NONE:
        default:
        case JumpTarget::THROW:
            return;
        case JumpTarget::BREAK:
            throw RuntimeError("Wild break jump" + at(engine->jumpFrom));
        case JumpTarget::CONTINUE:
            throw RuntimeError("Wild continue jump" + at(engine->jumpFrom));
    }
}

std::string Object::toString() const {
    return std::visit(overloaded {
            [](std::monostate) { return std::string("<void>"); },
            [](int_t x) { return std::to_string(x); },
            [](real_t x) {
                size_t len = std::snprintf(nullptr, 0, "%g", x);
                std::string ret(len, '\0');
                std::sprintf(ret.data(), "%g", x);
                if (ret == std::to_string((int_t)x)) ret += ".0";
                return ret;
            },
            [](func_t const& ptr) { return ptr->toString(); },
            [](list_t const& ptr) {
                bool first = true;
                std::string ret = "[";
                for (auto&& obj : *ptr) {
                    if (first) { first = false; } else { ret += ", "; }
                    ret += obj.toString();
                }
                return ret + "]";
            }
    }, object);
}

std::string Function::toString() const {
    std::string ret = descriptor();
    ret += " = {";
    ret += stmt->toString();
    ret += "}";
    return ret;
}
}