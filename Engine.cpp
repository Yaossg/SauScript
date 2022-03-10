#include "Node.hpp"

namespace SauScript {

Operand ScriptEngine::findOperand(const std::string& name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto&& scope = *it;
        if (scope.contains(name)) return &scope[name];
    }
    runtime("reference to undefined variable '" + name + "'");
}

void ScriptEngine::install(const std::string &name, Object const& fn) {
    if (!global().contains(name)) {
        global()[name] = fn;
    } else {
        switch (auto&& id = global()[name]; id.type()) {
            case Type::FUNC: {
                std::vector<Object> overloads{id, fn};
                id = {std::make_shared<List>(std::move(overloads))};
            } break;
            case Type::LIST:
                get<list_t>(id.object)->mut().push_back(fn);
                break;
            default:
                runtime("attempt to install external function for a incompatible name");
        }
    }
}

std::unique_ptr<ExprNode> ScriptEngine::compileExpression(Token*& current, int level = Operators::LEVEL_ROOT) {
    switch (Operator const* op; level) {
        case Operators::LEVEL_PRIMARY:
            switch (auto token = *current++; token.type) {
                case TokenType::LITERAL_BOOL:
                    return std::make_unique<ValNode>(this, token.location, Object{token.literal_bool()});
                case TokenType::LITERAL_INT:
                    return std::make_unique<ValNode>(this, token.location, Object{token.literal_int()});
                case TokenType::LITERAL_REAL:
                    return std::make_unique<ValNode>(this, token.location, Object{token.literal_real()});
                case TokenType::LITERAL_STRING:
                    return std::make_unique<ValNode>(this, token.location, Object{std::make_shared<String>(token.literal_string())});
                case TokenType::IDENTIFIER:
                    return std::make_unique<RefNode>(this, token.location, token.identifier());
                case TokenType::KEYWORD: {
                    switch (token.keyword()) {
                        using enum Keyword;
                        case FN:
                            return compileFunction(current);
                        case FOR:
                            return compileFor(current);
                        case WHILE:
                            return compileWhile(current);
                        case IF:
                            return compileIfElse(current);
                        case TRY:
                            return compileTryCatch(current);
                        case CATCH:
                            syntax("stray catch", token.location);
                        case ELSE:
                            syntax("stray else", token.location);
                    }
                }
                case TokenType::BRACE:
                    if (token == Token::braceLeft()) {
                        auto stmt = compileStatements(current);
                        if (*current != Token::braceRight())
                            syntax("missing '}' to match '{'", current->location);
                        ++current;
                        return stmt;
                    } else if (token == Token::bracketLeft()) {
                        return std::make_unique<ListLiteralNode>(this, token.location, compileExpressions(current, Token::bracketRight()));
                    } else if (token == Token::parenLeft()) {
                        while (current->type == TokenType::LINEBREAK) ++current;
                        auto expr = compileExpression(current);
                        while (current->type == TokenType::LINEBREAK) ++current;
                        if (*current != Token::parenRight())
                            syntax("missing ')' to match '('", current->location);
                        ++current;
                        return expr;
                    }
                case TokenType::LINEBREAK:
                    syntax("unexpected linebreak", token.location);
                case TokenType::TERMINATOR:
                    syntax("unexpected terminator", token.location);
                case TokenType::PUNCTUATOR:
                    syntax("unexpected punctuator", token.location);

            }
        case Operators::LEVEL_PREFIX: {
            if (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                SourceLocation location = current++->location;
                if (current->type == TokenType::LINEBREAK) syntax("redundant linebreak is forbidden after unary operator");
                return std::make_unique<OpUnaryNode>(this, location, compileExpression(current, level), op);
            }
            return compileExpression(current, level + 1);
        }
        case Operators::LEVEL_POSTFIX: {
            auto expr = compileExpression(current, level + 1);
            while (true) {
                if (*current == Token::punctuator(".")) {
                    while (current->type == TokenType::LINEBREAK) ++current;
                    auto id = *++current;
                    if (id.type != TokenType::IDENTIFIER) syntax("id-expression is expected after '.'", id.location);
                    if (*++current != Token::parenLeft()) syntax("expected member function invocation", id.location);
                    ++current;
                    expr = std::make_unique<OpInvokeNode>(this, id.location, std::make_unique<RefNode>(this, id.location, id.identifier()),
                                                          compileExpressions(current, Token::parenRight(), {std::move(expr)}));
                } else if (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                    SourceLocation location = current++->location;
                    while (current->type == TokenType::LINEBREAK) ++current;
                    expr = std::make_unique<OpUnaryNode>(this, location, std::move(expr), op, true);
                } else if (*current == Token::parenLeft()) {
                    SourceLocation location = current++->location;
                    expr = std::make_unique<OpInvokeNode>(this, location, std::move(expr), compileExpressions(current, Token::parenRight()));
                } else if (*current == Token::bracketLeft()) {
                    SourceLocation location = current++->location;
                    while (current->type == TokenType::LINEBREAK) ++current;
                    auto index = compileExpression(current);
                    if (*current != Token::bracketRight())
                        syntax("missing ']' to match '['", current->location);
                    ++current;
                    expr = std::make_unique<OpIndexNode>(this, location, std::move(expr), std::move(index));
                } else break;
            }
            return expr;
        }
        case Operators::LEVEL_ROOT: {
            if (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                SourceLocation location = current++->location;
                if (op->isBinary()) syntax("binary operator is used as unary", location);
                while (current->type == TokenType::LINEBREAK) ++current;
                return std::make_unique<OpUnaryNode>(this, location, compileExpression(current, level), op);
            }
            auto expr = compileExpression(current, level + 1);
            if (current->type == TokenType::PUNCTUATOR) {
                if ((op = Operators::find(current->punctuator(), level))) {
                    SourceLocation location = current++->location;
                    if (!op->isBinary()) syntax("unary operator is used as binary", location);
                    while (current->type == TokenType::LINEBREAK) ++current;
                    return std::make_unique<OpBinaryNode>(this, location, std::move(expr), compileExpression(current, level), op);
                } else if (*current == Token::punctuator("?")) {
                    SourceLocation location = current++->location;
                    while (current->type == TokenType::LINEBREAK) ++current;
                    if (auto lhs = compileExpression(current, level); *current == Token::punctuator(":")) {
                        while ((++current)->type == TokenType::LINEBREAK);
                        return std::make_unique<OpTernaryNode>(this, location, std::move(expr), std::move(lhs), compileExpression(current, level));
                    } else {
                        syntax("missing ':' for '?'", location);
                    }
                }
            }
            return expr;
        }
        default: {
            auto expr = compileExpression(current, level + 1);
            while (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                SourceLocation location = current++->location;
                while (current->type == TokenType::LINEBREAK) ++current;
                if (current->type == TokenType::PUNCTUATOR && Operators::find(current->punctuator(), Operators::LEVEL_ROOT))
                    return std::make_unique<OpBinaryNode>(this, location, std::move(expr),
                                                          compileExpression(current), op);
                expr = std::make_unique<OpBinaryNode>(this, location, std::move(expr),
                                                      compileExpression(current, level + 1), op);
            }
            return expr;
        }
    }
}

std::vector<std::unique_ptr<ExprNode>> ScriptEngine::compileExpressions(Token *&current, Token const& stop, std::unique_ptr<ExprNode> init) {
    std::vector<std::unique_ptr<ExprNode>> expressions;
    if (init) expressions.push_back(std::move(init));
    while (true) {
        while (current->type == TokenType::LINEBREAK) ++current;
        if (*current == stop) break;
        expressions.push_back(compileExpression(current));
        while (current->type == TokenType::LINEBREAK) ++current;
        if (*current == stop) break;
        if (*current++ != Token::punctuator(","))
            syntax("unexpected token interrupt series of expressions", (--current)->location);
    }
    ++current;
    return expressions;
}

std::unique_ptr<ExprNode> ScriptEngine::compileWhile(Token*& current) {
    SourceLocation location = (--current)->location;
    auto cond = compileExpression(++current);
    while (current->type == TokenType::LINEBREAK) ++current;
    if (*current != Token::braceLeft()) syntax("missing '{' in while", current->location);
    auto stmt = compileStatements(++current);
    if (*current != Token::braceRight()) syntax("missing '}' in while", current->location);
    ++current;
    return std::make_unique<WhileNode>(this, location, std::move(cond), std::move(stmt));
}

std::unique_ptr<ExprNode> ScriptEngine::compileFor(Token*& current) {
    SourceLocation location = (--current)->location;
    if ((++current)->type != TokenType::IDENTIFIER) syntax("expected for identifier", current->location);
    std::string name = current->identifier();
    if (*++current != Token::punctuator(":")) syntax("expected ':' in for", current->location);
    auto iter = compileExpression(++current);
    if (*current != Token::braceLeft()) syntax("missing '{' in for", current->location);
    auto stmt = compileStatements(++current);
    if (*current != Token::braceRight()) syntax("missing '}' in for", current->location);
    ++current;
    return std::make_unique<ForNode>(this, location, name, std::move(iter), std::move(stmt));
}

std::unique_ptr<ExprNode> ScriptEngine::compileIfElse(Token*& current) {
    SourceLocation location = (--current)->location;
    auto cond = compileExpression(++current);
    while (current->type == TokenType::LINEBREAK) ++current;
    if (*current != Token::braceLeft()) syntax("missing '{' in if", current->location);
    auto then = compileStatements(++current);
    if (*current != Token::braceRight()) syntax("missing '}' in if", current->location);
    std::unique_ptr<ExprNode> else_ = std::make_unique<StmtsNode>(this, current->location, std::vector<std::unique_ptr<ExprNode>>());
    bool skipped = false;
    while ((++current)->type == TokenType::LINEBREAK) skipped = true;
    if (*current == Token::keyword(Keyword::ELSE)) {
        while ((++current)->type == TokenType::LINEBREAK);
        if (*current == Token::keyword(Keyword::IF)) {
            else_ = compileIfElse(++current);
        } else {
            if (*current != Token::braceLeft()) syntax("missing '{' in else", current->location);
            else_ = compileStatements(++current);
            if (*current != Token::braceRight()) syntax("missing '}' in else", current->location);
            ++current;
        }
    } else if (skipped) {
        --current;
    }
    return std::make_unique<IfElseNode>(this, location, std::move(cond), std::move(then), std::move(else_));
}

std::unique_ptr<ExprNode> ScriptEngine::compileTryCatch(Token*& current) {
    SourceLocation location = (--current)->location;
    while ((++current)->type == TokenType::LINEBREAK);
    if (*current != Token::braceLeft()) syntax("missing '{' in try", current->location);
    auto try_ = compileStatements(++current);
    if (*current != Token::braceRight()) syntax("missing '}' in try", current->location);
    while ((++current)->type == TokenType::LINEBREAK);
    if (*current != Token::keyword(Keyword::CATCH)) syntax("expected catch in try", current->location);
    if ((++current)->type != TokenType::IDENTIFIER) syntax("expected catch identifier", current->location);
    auto name = current->identifier();
    while ((++current)->type == TokenType::LINEBREAK);
    if (*current != Token::braceLeft()) syntax("missing '{' in catch", current->location);
    auto catch_ = compileStatements(++current);
    if (*current != Token::braceRight()) syntax("missing '}' in catch", current->location);
    ++current;
    return std::make_unique<TryCatchNode>(this, location, std::move(try_), name, std::move(catch_));
}

std::unique_ptr<ExprNode> ScriptEngine::compileFunction(Token*& current) {
    SourceLocation location = (--current)->location;
    if (*++current != Token::parenLeft())
        syntax("left parenthesis of function expected", location);
    std::vector<Parameter> parameters;
    ++current;
    while (true) {
        if (*current == Token::parenRight()) break;
        if (current->type != TokenType::IDENTIFIER)
            syntax("name of parameter expected", current->location);
        std::string name = current++->identifier();
        Type type = Type::ANY;
        if (*current == Token::punctuator(":")) {
            type = (++current)->parseType();
            ++current;
        }
        parameters.push_back({type, name});
        if (*current == Token::parenRight()) break;
        if (*current++ != Token::punctuator(","))
            syntax("unexpected token interrupt function definition", (--current)->location);
    }
    ++current;
    Type returnType = Type::ANY;
    if (*current == Token::punctuator(":")) {
        returnType = (++current)->parseType();
        ++current;
    }
    std::unique_ptr<ExprNode> stmt;
    if (*current != Token::punctuator("="))
        syntax("expected '=' after return type", current->location);
    stmt = compileExpression(++current);
    return std::make_unique<ValNode>(this, location, Object{
        std::make_shared<Function>(Function{returnType, parameters, std::move(stmt)})});
}

std::unique_ptr<StmtsNode> ScriptEngine::compileStatements(Token*& current) {
    std::vector<std::unique_ptr<ExprNode>> stmts;
    SourceLocation location = current->location;
    while (true) {
        if (*current == Token::terminator()) { break; }
        if (*current == Token::braceRight()) { break; }
        if (current->type == TokenType::LINEBREAK) { ++current; continue; }
        stmts.push_back(compileExpression(current));
    }
    return std::make_unique<StmtsNode>(this, location, std::move(stmts));
}

std::unique_ptr<StmtsNode> ScriptEngine::compile(std::string const& script) {
    auto code = std::make_shared<SourceCode>(script);
    code->tokenize();
    Token* current = code->tokens.data();
    auto stmts = compileStatements(current);
    if (*current != Token::terminator()) syntax("stray tokens", current->location);
    return stmts;
}

Object ScriptEngine::eval(std::string const& script) {
    try {
        compile(script)->push_unscoped();
        Object ret;
        switch (jumpTarget) {
            case JumpTarget::BREAK:
                runtime("Wild break jump", jumpFrom);
            case JumpTarget::CONTINUE:
                runtime("Wild continue jump", jumpFrom);
            case JumpTarget::RETURN:
                ret = yield;
                break;
            case JumpTarget::NONE:
                ret = pop().val();
                break;
        }
        jumpTarget = JumpTarget::NONE;
        return ret;
    } catch (Error& e) {
        fprintf(err, "%s\n", e.what());
    } catch (RawError& e) {
        fprintf(err, "Fatal compiler internal error occurred, message reported: %s", e.message.c_str());
    }
    if (pushed) impossible();
    jumpTarget = JumpTarget::NONE;
    return {};
}

}