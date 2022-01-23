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
            if (token == "false" || token == "true") {
                tokens.push_back(Token::literal(token).at(line));
            } else if (auto kw = Keyword::parse(token); kw != Keyword::NAK) {
                tokens.push_back(Token::keyword(kw).at(line));
            } else if (std::any_of(std::begin(OPERATORS), std::end(OPERATORS),
                                   [&token] (Operator const& op) { return op.literal == token; })) {
                tokens.push_back(Token::punctuation(token).at(line));
            } else {
                tokens.push_back(Token::identifier(token).at(line));
            }
        } else if (isNumberStart(ch)) {
            char const* first = source;
            while (isNumberContinue(*++source));
            tokens.push_back(Token::literal({first, source}).at(line));
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

std::optional<Object> ScriptEngine::findObject(const std::string &name) {
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
            case TokenType::LITERAL:
                operands.push(std::make_unique<ValNode>(token.line, parseNumber(token.line, token.literal())));
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

std::unique_ptr<StmtNode> ScriptEngine::compileWhile(Token *&current) {
    Token* first = ++current;
    using namespace Keyword;
    while (*current != Token::keyword(DO)) {
        if (current->type == TokenType::EOT) throw SyntaxError("missing do in while at line " + first->at());
        ++current;
    }
    auto cond = compileExpression({first, current});
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in while at line " + current->at());
    return std::make_unique<WhileNode>(this, std::move(cond), std::move(stmt));
}

std::unique_ptr<StmtNode> ScriptEngine::compileDo(Token *&current) {
    using namespace Keyword;
    auto stmt = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in do at line " + current->at());
    return std::make_unique<DoNode>(this, std::move(stmt));
}

std::unique_ptr<StmtNode> ScriptEngine::compileRepeat(Token *&current) {
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

std::unique_ptr<StmtNode> ScriptEngine::compileIf(Token *&current) {
    Token* first = ++current;
    using namespace Keyword;
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

std::unique_ptr<StmtNode> ScriptEngine::compileTry(Token *&current) {
    using namespace Keyword;
    auto try_ = compileStatements(++current);
    if (*current != Token::keyword(CATCH)) throw SyntaxError("try without catch at line " + current->at());
    if ((++current)->type != TokenType::IDENTIFIER) throw SyntaxError("catch without identifier at line " + current->at());
    auto name = current->identifier();
    auto catch_ = compileStatements(++current);
    if (*current != Token::keyword(END)) throw SyntaxError("missing end in try at line " + current->at());
    return std::make_unique<TryNode>(this, std::move(try_), name, std::move(catch_));
}

std::unique_ptr<StmtNode> ScriptEngine::compileStatements(Token *&current) {
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
                    case DO:
                        stmts.push_back(compileDo(current));
                        continue;
                    case REPEAT:
                        stmts.push_back(compileRepeat(current));
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

std::unique_ptr<StmtNode> ScriptEngine::compile(const char *script) {
    auto tokens = tokenize(script);
    Token* current = tokens.data();
    return compileStatements(current);
}

void ScriptEngine::exec(const char *script, FILE *err) {
    try {
        compile(script)->exec();
    } catch (ScriptBreak e) {
        fprintf(err, "Wild break at line %s\n", toLineString(e.line).c_str());
    } catch (ScriptContinue e) {
        fprintf(err, "Wild continue at line %s\n", toLineString(e.line).c_str());
    } catch (ScriptException caught) {
        fprintf(err, "Unhandled script exception caught: %d at line %s\n", caught.thrown, toLineString(caught.line).c_str());
    } catch (SyntaxError& e) {
        fprintf(err, "Syntax error: %s\n", e.what());
    } catch (RuntimeError& e) {
        fprintf(err, "Runtime error: %s\n", e.what());
    }
}

}