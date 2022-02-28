#include "Node.hpp"
#include "Keyword.hpp"

namespace SauScript {

Operand ScriptEngine::findOperand(const std::string& name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto&& scope = *it;
        if (scope.contains(name)) return &scope[name];
    }
    throw PlainRuntimeError("reference to undefined variable '" + name + "'");
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
                case TokenType::IDENTIFIER:
                    return std::make_unique<RefNode>(this, token.location, token.identifier());
                case TokenType::KEYWORD: {
                    using namespace Keyword;
                    switch (token.keyword()) {
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
                            throw SyntaxError("stray catch" + token.location.at());
                        case ELSE:
                            throw SyntaxError("stray else" + token.location.at());
                    }
                }
                case TokenType::LINEBREAK:
                    throw SyntaxError("unexpected linebreak" + token.location.at());
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
                        return std::make_unique<ListLiteralNode>(this, token.location, std::move(objs));
                    } else if (token == Token::parenLeft()) {
                        auto expr = compileExpression(current);
                        if (*current != Token::parenRight())
                            throw SyntaxError("missing ')' to match '('" + current->at());
                        ++current;
                        return expr;
                    }
                    throw SyntaxError("unexpected token" + token.at());
            }
        case Operators::LEVEL_PREFIX: {
            if (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                SourceLocation location = current++->location;
                return std::make_unique<OpUnaryNode>(this, location, compileExpression(current, level), op);
            }
            return compileExpression(current, level + 1);
        }
        case Operators::LEVEL_POSTFIX: {
            auto expr = compileExpression(current, level + 1);
            while (true) {
                if (*current == Token::punctuator(".")) {
                    auto id = *++current;
                    if (id.type != TokenType::IDENTIFIER) throw SyntaxError("id-expression is expected after '.'" + id.at());
                    if (*++current != Token::parenLeft()) throw SyntaxError("expected member function invocation" + id.at());
                    std::vector<std::unique_ptr<ExprNode>> args;
                    args.push_back(std::move(expr));
                    ++current;
                    while (true) {
                        if (*current == Token::parenRight()) break;
                        args.push_back(compileExpression(current));
                        if (*current == Token::parenRight()) break;
                        if (*current++ != Token::punctuator(","))
                            throw SyntaxError("unexpected token interrupt function invocation" + current->at());
                    }
                    ++current;
                    expr = std::make_unique<OpInvokeNode>(this, id.location, std::make_unique<RefNode>(this, id.location, id.identifier()), std::move(args));
                } else if (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                    SourceLocation location = current++->location;
                    expr = std::make_unique<OpUnaryNode>(this, location, std::move(expr), op, true);
                } else if (*current == Token::parenLeft()) {
                    SourceLocation location = current++->location;
                    std::vector<std::unique_ptr<ExprNode>> args;
                    while (true) {
                        if (*current == Token::parenRight()) break;
                        args.push_back(compileExpression(current));
                        if (*current == Token::parenRight()) break;
                        if (*current++ != Token::punctuator(","))
                            throw SyntaxError("unexpected token interrupt function invocation" + current->at());
                    }
                    ++current;
                    expr = std::make_unique<OpInvokeNode>(this, location, std::move(expr), std::move(args));
                } else if (*current == Token::bracketLeft()) {
                    SourceLocation location = current++->location;
                    auto index = compileExpression(current);
                    if (*current != Token::bracketRight())
                        throw SyntaxError("missing ']' to match '['" + current->at());
                    ++current;
                    expr = std::make_unique<OpIndexNode>(this, location, std::move(expr), std::move(index));
                } else break;
            }
            return expr;
        }
        case Operators::LEVEL_ROOT: {
            if (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                SourceLocation location = current++->location;
                if (op->isBinary()) throw SyntaxError("binary operator is used as unary" + location.at());
                return std::make_unique<OpUnaryNode>(this, location, compileExpression(current, level), op);
            }
            auto expr = compileExpression(current, level + 1);
            if (current->type == TokenType::PUNCTUATOR) {
                if ((op = Operators::find(current->punctuator(), level))) {
                    SourceLocation location = current++->location;
                    if (!op->isBinary()) throw SyntaxError("unary operator is used as binary" + location.at());
                    return std::make_unique<OpBinaryNode>(this, location, std::move(expr), compileExpression(current, level), op);
                }
                if (*current == Token::punctuator("?")) {
                    SourceLocation location = current++->location;
                    auto lhs = compileExpression(current, level);
                    if (*current == Token::punctuator(":")) {
                        auto rhs = compileExpression(++current, level);
                        return std::make_unique<OpTernaryNode>(this, location, std::move(expr), std::move(lhs), std::move(rhs));
                    } else {
                        throw SyntaxError("missing ':' for '?'" + location.at());
                    }
                }
            }
            return expr;
        }
        default: {
            auto expr = compileExpression(current, level + 1);
            while (current->type == TokenType::PUNCTUATOR && (op = Operators::find(current->punctuator(), level))) {
                SourceLocation location = current++->location;
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

std::unique_ptr<ExprNode> ScriptEngine::compileWhile(Token*& current) {
    using namespace Keyword;
    SourceLocation location = (--current)->location;
    auto cond = compileExpression(++current);
    if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in while" + current->at());
    auto stmt = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in while" + current->at());
    ++current;
    return std::make_unique<WhileNode>(this, location, std::move(cond), std::move(stmt));
}

std::unique_ptr<ExprNode> ScriptEngine::compileFor(Token*& current) {
    using namespace Keyword;
    SourceLocation location = (--current)->location;
    std::vector<std::unique_ptr<ExprNode>> init;
    init.push_back(compileExpression(++current));
    if (*current == Token::punctuator(":")) {
        if (auto* ref = dynamic_cast<RefNode*>(init.front().get())) {
            auto iter = compileExpression(++current);
            if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in for" + current->at());
            auto stmt = compileStatements(++current);
            if (*current != Token::braceRight()) throw SyntaxError("missing '}' in for" + current->at());
            ++current;
            return std::make_unique<ForEachNode>(this, location, ref->name, std::move(iter), std::move(stmt));
        } else {
            throw SyntaxError("expected id-expression in for-each");
        }
    }
    while (*current == Token::punctuator(","))
        init.push_back(compileExpression(++current));
    if (*current != Token::linebreak()) throw SyntaxError("missing first linebreak in for" + current->at());
    auto cond = compileExpression(++current);
    if (*current != Token::linebreak()) throw SyntaxError("missing second linebreak in for" + current->at());
    std::vector<std::unique_ptr<ExprNode>> iter;
    iter.push_back(compileExpression(++current));
    while (*current == Token::punctuator(","))
        iter.push_back(compileExpression(++current));
    if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in for" + current->at());
    auto stmt = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in for" + current->at());
    ++current;
    return std::make_unique<ForNode>(this, location,
                                     std::make_unique<StmtsNode>(this, location, std::move(init)),
                                     std::move(cond),
                                     std::make_unique<StmtsNode>(this, location, std::move(iter)),
                                     std::move(stmt));
}

std::unique_ptr<ExprNode> ScriptEngine::compileIfElse(Token*& current) {
    using namespace Keyword;
    SourceLocation location = (--current)->location;
    auto cond = compileExpression(++current);
    if (*current != Token::braceLeft()) throw SyntaxError("missing '{' in if" + current->at());
    auto then = compileStatements(++current);
    if (*current != Token::braceRight()) throw SyntaxError("missing '}' in if" + current->at());
    std::unique_ptr<ExprNode> else_ = std::make_unique<StmtsNode>(this, current->location, std::vector<std::unique_ptr<ExprNode>>());
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
    return std::make_unique<IfElseNode>(this, location, std::move(cond), std::move(then), std::move(else_));
}

std::unique_ptr<ExprNode> ScriptEngine::compileTryCatch(Token*& current) {
    using namespace Keyword;
    SourceLocation location = (--current)->location;
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
    return std::make_unique<TryCatchNode>(this, location, std::move(try_), name, std::move(catch_));
}

std::unique_ptr<ExprNode> ScriptEngine::compileFunction(Token*& current) {
    SourceLocation location = (--current)->location;
    if (*++current != Token::parenLeft())
        throw SyntaxError("left parenthesis of function expected" + location.at());
    std::vector<Parameter> parameters;
    ++current;
    while (true) {
        if (*current == Token::parenRight()) break;
        if (current->type != TokenType::IDENTIFIER)
            throw SyntaxError("name of parameter expected" + current->at());
        std::string name = current++->identifier();
        Type type = Type::ANY;
        if (*current == Token::punctuator(":")) {
            type = (++current)->parseType();
            ++current;
        }
        parameters.push_back({type, name});
        if (*current == Token::parenRight()) break;
        if (*current++ != Token::punctuator(","))
            throw SyntaxError("unexpected token interrupt function definition" + (--current)->at());
    }
    ++current;
    Type returnType = Type::ANY;
    if (*current == Token::punctuator(":")) {
        returnType = (++current)->parseType();
        ++current;
    }
    std::unique_ptr<ExprNode> stmt;
    if (*current != Token::punctuator("="))
        throw SyntaxError("expected '=' after return type" + current->at());
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
        if (*current == Token::linebreak()) { ++current; continue; }
        stmts.push_back(compileExpression(current));
    }
    return std::make_unique<StmtsNode>(this, location, std::move(stmts));
}

std::unique_ptr<StmtsNode> ScriptEngine::compile(std::string script) {
    auto code = std::make_unique<SourceCode>(script);
    Token* current = code->tokens.data();
    auto stmts = compileStatements(current);
    if (*current != Token::terminator()) throw SyntaxError("stray tokens" + current->location.at());
    compiled.push_back(std::move(code));
    return stmts;
}

void ScriptEngine::exec(std::string script, FILE* err) {
    try {
        compile(std::move(script))->push_unscoped();
        Operand ret;
        switch (jumpTarget) {
            case JumpTarget::THROW:
                fprintf(err, "Unhandled exception: %s", yield.toString().c_str());
                break;
            case JumpTarget::BREAK:
                throw RuntimeError("Wild break jump" + jumpFrom.at());
            case JumpTarget::CONTINUE:
                throw RuntimeError("Wild continue jump" + jumpFrom.at());
            case JumpTarget::RETURN:
                ret = yield;
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

}