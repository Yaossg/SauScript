#pragma once

#include <functional>
#include <deque>
#include <stack>
#include <utility>

#include "Token.hpp"

namespace SauScript {

enum class JumpTarget {
    NONE, CONTINUE, BREAK, RETURN
};

struct ScriptEngine {
    using Scope = std::unordered_map<std::string, Object>;
    std::deque<Scope> scopes{{}, {}};
    FILE *out, *in, *err;
    JumpTarget jumpTarget = JumpTarget::NONE;
    SourceLocation jumpFrom;
    Object yield;

    void jump(JumpTarget jumpTarget, Object yield) {
        this->jumpTarget = jumpTarget;
        this->yield = std::move(yield);
    }

    Operand peek; bool pushed = false;
    void push(Operand const& operand) {
        if (pushed) impossible();
        peek = operand; pushed = true;
    }
    [[nodiscard]] Operand const& top() const { if (!pushed) impossible(); return peek; }
    Operand pop() { if (!pushed) impossible(); pushed = false; return peek; }

    void initEnv();
    ScriptEngine(FILE *out = stdout, FILE *in = stdin, FILE *err = stderr) : out(out), in(in), err(err) {
        initEnv();
    }

    Scope& global() { return scopes.front(); }
    Scope& local() { return scopes.back(); }

    void install(std::string const &name, Object const& fn);
    template<typename R, typename... Args>
    func_t external(std::function<R(Args...)> function);
    template<typename Fn>
    void install(std::string const &name, Fn fn) {
        install(name, {external(std::function{fn})});
    }

    Operand findOperand(std::string const &name);

    [[nodiscard]] std::unique_ptr<ExprNode> compileExpression(Token *&current, int level);
    [[nodiscard]] std::vector<std::unique_ptr<ExprNode>> compileExpressions(Token *&current, Token const& stop, std::unique_ptr<ExprNode> init = nullptr);
    [[nodiscard]] std::unique_ptr<ExprNode> compileWhile(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFor(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileIfElse(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileTryCatch(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFunction(Token *&current);
    [[nodiscard]] std::unique_ptr<StmtsNode> compileStatements(Token *&current);
    [[nodiscard]] std::unique_ptr<StmtsNode> compile(std::string const& script);

    Object eval(std::string const& script);
};

struct ScriptScope {
    ScriptEngine *engine;

    ScriptScope(ScriptEngine *engine) : engine(engine) {
        engine->scopes.emplace_back();
    }

    ~ScriptScope() {
        if (engine->jumpTarget == JumpTarget::NONE && std::uncaught_exceptions() == 0 && engine->pushed)
            engine->push(engine->pop().val());
        engine->scopes.pop_back();
    }
};

}