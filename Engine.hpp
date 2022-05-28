#pragma once

#include <functional>
#include <deque>
#include <stack>
#include <utility>

#include "Token.hpp"

namespace SauScript {

struct ScriptEngine {
    [[nodiscard]] std::unique_ptr<ExprNode> compileExpression(Token *&current, int level);
    [[nodiscard]] std::vector<std::unique_ptr<ExprNode>> compileExpressions(Token *&current, Token const& stop, std::unique_ptr<ExprNode> init = nullptr);
    [[nodiscard]] std::unique_ptr<ExprNode> compileWhile(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFor(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileIfElse(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileTryCatch(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFunction(Token *&current);
    [[nodiscard]] std::unique_ptr<StmtsNode> compileStatements(Token *&current);
    [[nodiscard]] std::unique_ptr<StmtsNode> compile(std::string const& script);
};
}