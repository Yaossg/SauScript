#pragma once

#include <functional>
#include <map>
#include <deque>
#include <stack>
#include <utility>

#include "TypeSystem.hpp"

namespace SauScript {

template<typename R, typename... Args>
std::function<func_t(ScriptEngine *)> external(std::function<R(Args...)> function);

void installEnvironment(ScriptEngine *list);

enum class JumpTarget {
    NONE, CONTINUE, BREAK, RETURN, THROW
};

struct ScriptEngine {
    using Scope = std::map<std::string, Object>;
    std::deque<Scope> scopes{{}, {}};
    FILE *out, *in;
    JumpTarget jumpTarget = JumpTarget::NONE;
    int jumpFrom = 0;
    Object yield;

    void jump(JumpTarget jumpTarget, int jumpFrom, Object yield) {
        this->jumpTarget = jumpTarget;
        this->jumpFrom = jumpFrom;
        this->yield = std::move(yield);
    }

    std::stack<Operand> stack;
    void push(Operand const &operand) { stack.push(operand); }
    [[nodiscard]] Operand& top() { return stack.top(); }
    Operand pop() { auto operand = top(); stack.pop(); return operand; }

    ScriptEngine(FILE *out = stdout, FILE *in = stdin) : out(out), in(in) {
        installEnvironment(this);
    }

    Scope &global() { return scopes.front(); }

    Scope &local() { return scopes.back(); }

    template<typename Fn>
    void installExternalFunction(std::string const &name, Fn fn);

    Operand findOperand(std::string const &name, int line);

    [[nodiscard]] std::unique_ptr<ExprNode> compileExpression(Token *&current, int level);
    [[nodiscard]] std::unique_ptr<ExprNode> compileWhile(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFor(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileIfElse(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileTryCatch(Token *&current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFunction(Token *&current);
    [[nodiscard]] std::unique_ptr<StmtsNode> compileStatements(Token *&current);
    [[nodiscard]] std::unique_ptr<StmtsNode> compile(char const *script);

    void exec(char const *script, FILE *err = stderr);
};

struct ScriptScope {
    ScriptEngine *engine;

    ScriptScope(ScriptEngine *engine) : engine(engine) {
        engine->scopes.emplace_back();
    }

    ~ScriptScope() {
        if (engine->jumpTarget == JumpTarget::NONE && std::uncaught_exceptions() == 0)
            engine->push(engine->pop().val());
        engine->scopes.pop_back();
    }
};


template<typename R, typename... Args>
std::function<func_t(ScriptEngine*)> external(std::function<R(Args...)> function);

template<typename Fn>
void ScriptEngine::installExternalFunction(const std::string &name, Fn fn) {
    Object wrapped{external(std::function{fn})(this)};
    if (!global().contains(name)) {
        global()[name] = wrapped;
    } else {
        switch (global()[name].type()) {
            case Type::FUNC:
                global()[name] = {std::make_shared<List>(List{global()[name], wrapped})};
                break;
            case Type::LIST:
                get<list_t>(global()[name].object)->objs.push_back(wrapped);
                break;
            default:
                throw RuntimeError("Assertion failed");
        }
    }
}



}