#pragma once

#include <utility>

#include "Engine.hpp"

namespace SauScript {

struct ExprNode {
    std::string descriptor;
    ScriptEngine* engine;
    int line;
    ExprNode(std::string descriptor, ScriptEngine* engine, int line): descriptor(std::move(descriptor)), engine(engine), line(line) {}

    virtual void push() const = 0;
    [[nodiscard]] virtual std::vector<ExprNode*> children() const { return {}; }
    [[nodiscard]] virtual std::string toString() const { return descriptor; }

    virtual ~ExprNode() = default;

    [[nodiscard]] std::string walk() const {
        int id = 0;
        std::string buf;
        walk(buf, id);
        return buf;
    }

private:
    void walk(std::string& buf, int& id) const {
        std::string pid = std::to_string(id);
        buf += "id_" + pid + "[\"" + descriptor + "\"]\n";
        for (auto&& child : children()) {
            ++id;
            buf += "id_" + pid + "-->" + "id_" + std::to_string(id) + "\n";
            child->walk(buf, id);
        }
    }
};

struct ValNode : ExprNode {
    Object val;
    ValNode(ScriptEngine* engine, int line, Object val): ExprNode(
            val.type() == Type::FUNC ? get<func_t>(val.object)->descriptor() : val.toString(),
            engine, line), val(std::move(val)) {}

    void push() const override {
        engine->push(val);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        if (val.type() == Type::FUNC) {
            auto&& fn = *get<func_t>(val.object);
            return {fn.stmt.get()};
        }
        return {};
    }
};

struct RefNode : ExprNode {
    std::string name;
    RefNode(ScriptEngine* engine, int line, std::string name): ExprNode(name, engine, line), name(std::move(name)) {}

    void push() const override {
        engine->push(engine->findOperand(name, line));
    }

    void initialize() {
        if (engine->local().contains(name))
            throw RuntimeError(name + " already exists in the local scope" + at(line));
        engine->push(&(engine->local()[name] = engine->pop().val()));
    }
};

struct OpUnaryNode : ExprNode {
    std::unique_ptr<ExprNode> operand;
    Operator const* op;
    bool postfix;
    OpUnaryNode(ScriptEngine* engine, int line,
                std::unique_ptr<ExprNode> operand,
                Operator const* op, bool postfix = false): ExprNode(std::string(op->literal), engine, line),
                operand(std::move(operand)), op(op), postfix(postfix) {}

    void push() const override {
        op->asUnary()(operand.get());
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {operand.get()};
    }

    [[nodiscard]] std::string toString() const override {
        if (postfix)
            return "(" + operand->toString() + " " + std::string(op->literal) + ")";
        else
            return "(" + std::string(op->literal) + " " + operand->toString() + ")";
    }
};

struct OpBinaryNode : ExprNode {
    std::unique_ptr<ExprNode> lhs, rhs;
    Operator const* op;
    OpBinaryNode(ScriptEngine* engine, int line,
                 std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs,
                 Operator const* op): ExprNode(std::string(op->literal), engine, line),
                 lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

    void push() const override {
        op->asBinary()(lhs.get(), rhs.get());
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "(" + lhs->toString() + std::string(op->literal) + rhs->toString() + ")";
    }
};

struct OpTernaryNode : ExprNode {
    std::unique_ptr<ExprNode> cond, lhs, rhs;
    OpTernaryNode(ScriptEngine* engine, int line,
                  std::unique_ptr<ExprNode> cond,
                  std::unique_ptr<ExprNode> lhs,
                  std::unique_ptr<ExprNode> rhs): ExprNode("?:", engine, line),
                  cond(std::move(cond)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    void push() const override {
        cond->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().asBool(line) ? lhs->push() : rhs->push();
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "(" + cond->toString() + "?" + lhs->toString() + ":" + rhs->toString() + ")";
    }
};

struct OpIndexNode : ExprNode {
    std::unique_ptr<ExprNode> list;
    std::unique_ptr<ExprNode> index;
    OpIndexNode(ScriptEngine* engine, int line,
                std::unique_ptr<ExprNode> list,
                std::unique_ptr<ExprNode> index): ExprNode("[]", engine, line),
                list(std::move(list)), index(std::move(index)) {}

    void push() const override {
        list->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto t = engine->pop();
        auto a = std::get<list_t>(t.val().object);
        index->push();
        if (engine->jumpTarget != JumpTarget::NONE) { engine->pop(); return; }
        auto b = engine->pop().val().asInt(line);
        if (b < 0 || b >= a->objs.size()) throw RuntimeError("list index access out of bound" + at(line));
        if (t.val_or_ref.index())
            engine->push(&a->objs.at(b));
        else
            engine->push(a->objs.at(b));
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {list.get(), index.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "(" + list->toString() + ")[" + index->toString() + "]";
    }
};

struct OpInvokeNode : ExprNode {
    std::unique_ptr<ExprNode> func;
    std::vector<std::unique_ptr<ExprNode>> args;
    OpInvokeNode(ScriptEngine* engine, int line,
                 std::unique_ptr<ExprNode> func,
                 std::vector<std::unique_ptr<ExprNode>> args): ExprNode("()", engine, line),
                 func(std::move(func)), args(std::move(args)) {}

    void push() const override {
        std::vector<Object> objects;
        for (auto&& arg : args) {
            arg->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            objects.push_back(engine->pop().val());
        }
        func->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().invoke(engine, line, objects);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret{func.get()};
        for (auto&& arg : args) {
            ret.push_back(arg.get());
        }
        return ret;
    }

    [[nodiscard]] std::string toString() const override {
        std::string ret = func->toString();
        ret += "(";
        bool first = true;
        for (auto&& arg : args) {
            if (first) { first = false; } else { ret += ", "; }
            ret += arg->toString();
        }
        ret += ")";
        return ret;
    }
};

struct ListLiteralNode : ExprNode {
    std::vector<std::unique_ptr<ExprNode>> objs;
    ListLiteralNode(ScriptEngine* engine, int line,
                    std::vector<std::unique_ptr<ExprNode>> objs): ExprNode("[]", engine, line),
                    objs(std::move(objs)) {}

    void push() const override {
        std::vector<Object> objects;
        for (auto&& obj : objs) {
            obj->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            objects.push_back(engine->pop().val());
        }
        engine->push(Object{std::make_shared<List>(std::move(objects))});
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret;
        for (auto&& obj : objs) {
            ret.push_back(obj.get());
        }
        return ret;
    }

    [[nodiscard]] std::string toString() const override {
        std::string ret = "[";
        bool first = true;
        for (auto&& obj : objs) {
            if (first) { first = false; } else { ret += ", "; }
            ret += obj->toString();
        }
        ret += "]";
        return ret;
    }
};

struct NoopNode : ExprNode {
    NoopNode(ScriptEngine* engine, int line): ExprNode("{}", engine, line) {}

    void push() const override {
        engine->push({});
    }
};

struct StmtsNode : ExprNode {
    std::vector<std::unique_ptr<ExprNode>> stmts;
    StmtsNode(ScriptEngine* engine, int line, std::vector<std::unique_ptr<ExprNode>> stmts)
        : ExprNode("{}", engine, line), stmts(std::move(stmts)) {}

    void push() const override {
        ScriptScope scope(engine);
        push_repl();
    }

    void push_repl() const {
        Operand ret;
        for (auto&& stmt : stmts) {
            stmt->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            ret = engine->pop();
        }
        engine->push(ret);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret;
        for (auto&& stmt : stmts) {
            ret.push_back(stmt.get());
        }
        return ret;
    }

    [[nodiscard]] std::string toString() const override {
        std::string ret = "{";
        for (auto&& stmt : stmts) {
            ret += stmt->toString();
            ret += ";";
        }
        ret += "}";
        return ret;
    }
};

struct JumpNode : ExprNode {
    JumpTarget jumpTarget;
    JumpNode(ScriptEngine* engine, int line, JumpTarget jumpTarget)
            : ExprNode(jumpTarget == JumpTarget::BREAK ? "break" : "continue", engine, line), jumpTarget(jumpTarget) {}

    void push() const override {
        engine->target = {};
        engine->jumpTarget = jumpTarget;
        engine->jumpFrom = line;
    }
};

struct WhileNode : ExprNode {
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> loop;
    WhileNode(ScriptEngine* engine, int line,
              std::unique_ptr<ExprNode> cond,
              std::unique_ptr<ExprNode> loop)
            : ExprNode("while", engine, line), cond(std::move(cond)), loop(std::move(loop)) {}

    void push() const override {
        ScriptScope scope(engine);
        begin:
        cond->push();
        if (engine->jumpTarget == JumpTarget::NONE) {
            if (!engine->pop().val().asBool(line)) goto end;
            loop->push();
            if (engine->jumpTarget == JumpTarget::NONE)
                engine->pop();
        }
        if (engine->jumpTarget == JumpTarget::CONTINUE) engine->jumpTarget = JumpTarget::NONE;
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        goto begin;
        end:
        Operand ret;
        if (engine->jumpTarget == JumpTarget::BREAK) {
            engine->jumpTarget = JumpTarget::NONE;
            ret = engine->target;
        }
        if (engine->jumpTarget == JumpTarget::NONE)
            engine->push(ret);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), loop.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "while " + cond->toString() + loop->toString();
    }
};

struct DoWhileNode : ExprNode {
    std::unique_ptr<ExprNode> loop;
    std::unique_ptr<ExprNode> cond;
    DoWhileNode(ScriptEngine* engine, int line,
                std::unique_ptr<ExprNode> loop,
                std::unique_ptr<ExprNode> cond)
            : ExprNode("do-while", engine, line), loop(std::move(loop)), cond(std::move(cond)) {}

    void push() const override {
        ScriptScope scope(engine);
        begin:
        loop->push();
        if (engine->jumpTarget == JumpTarget::NONE)
            engine->pop();
        if (engine->jumpTarget == JumpTarget::CONTINUE) engine->jumpTarget = JumpTarget::NONE;
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        cond->push();
        if (engine->jumpTarget == JumpTarget::CONTINUE) engine->jumpTarget = JumpTarget::NONE;
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        if (engine->pop().val().asBool(line)) goto begin;
        end:
        Operand ret;
        if (engine->jumpTarget == JumpTarget::BREAK) {
            engine->jumpTarget = JumpTarget::NONE;
            ret = engine->target;
        }
        if (engine->jumpTarget == JumpTarget::NONE)
            engine->push(ret);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {loop.get(), cond.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "do" + loop->toString() + "while " + cond->toString() + ";";
    }
};

struct ForNode : ExprNode {
    std::unique_ptr<ExprNode> init;
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> iter;
    std::unique_ptr<ExprNode> loop;
    ForNode(ScriptEngine* engine, int line,
            std::unique_ptr<ExprNode> init,
            std::unique_ptr<ExprNode> cond,
            std::unique_ptr<ExprNode> iter,
            std::unique_ptr<ExprNode> loop)
            : ExprNode("for", engine, line), init(std::move(init)), cond(std::move(cond)), iter(std::move(iter)), loop(std::move(loop)) {}

    void push() const override {
        ScriptScope scope(engine);
        init->push();
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        engine->pop();
        begin:
        cond->push();
        if (engine->jumpTarget == JumpTarget::NONE) {
            if (!engine->pop().val().asBool(line)) goto end;
            loop->push();
            if (engine->jumpTarget == JumpTarget::NONE)
                engine->pop();
        }
        if (engine->jumpTarget == JumpTarget::CONTINUE) engine->jumpTarget = JumpTarget::NONE;
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        iter->push();
        engine->pop();
        goto begin;
        end:
        Operand ret;
        if (engine->jumpTarget == JumpTarget::BREAK) {
            engine->jumpTarget = JumpTarget::NONE;
            ret = engine->target;
        }
        if (engine->jumpTarget == JumpTarget::NONE)
            engine->push(ret);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {init.get(), cond.get(), iter.get(), loop.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "for " + init->toString() + ";" + cond->toString() + ";" + iter->toString() + loop->toString();
    }
};

struct IfElseNode : ExprNode {
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> then;
    std::unique_ptr<ExprNode> else_;
    IfElseNode(ScriptEngine* engine, int line,
               std::unique_ptr<ExprNode> cond,
               std::unique_ptr<ExprNode> then,
               std::unique_ptr<ExprNode> else_)
            : ExprNode("if-else", engine, line), cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) {}

    void push() const override {
        ScriptScope scope(engine);
        cond->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().asBool(line) ? then->push() : else_->push();
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), then.get(), else_.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "if " + cond->toString() + then->toString() + "else" + else_->toString();
    }
};

struct TryCatchNode : ExprNode {
    std::unique_ptr<ExprNode> try_;
    std::string name;
    std::unique_ptr<ExprNode> catch_;
    TryCatchNode(ScriptEngine* engine, int line,
                 std::unique_ptr<ExprNode> try_,
                 std::string name,
                 std::unique_ptr<ExprNode> catch_)
            : ExprNode("try-catch", engine, line), try_(std::move(try_)), name(std::move(name)), catch_(std::move(catch_)) {}

    void push() const override {
        {
            ScriptScope scope(engine);
            try_->push();
        }
        if (engine->jumpTarget == JumpTarget::THROW) {
            engine->jumpTarget = JumpTarget::NONE;
            ScriptScope scope(engine);
            engine->local()[name] = engine->target;
            catch_->push();
        }
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {try_.get(), catch_.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "try" + try_->toString() + "catch " + name + catch_->toString();
    }
};

template<typename R, typename... Args>
struct ExternalFunctionInvocationNode : ExprNode {
    std::function<R(Args...)> function;

    ExternalFunctionInvocationNode(ScriptEngine *engine, std::function<R(Args...)> function)
            : ExprNode("<external function invocation>", engine, 0), function(function) {}

    void push() const override {
        engine->push(eval(std::index_sequence_for<Args...>()));
    }

    template<size_t... I>
    Object eval(std::index_sequence<I...>) const {
        auto functor = std::bind(function, engine->findOperand(externalParameterName<I>(), 0).val().template as<Args>()...);
        if constexpr(std::is_void_v<R>) {
            functor();
            return {};
        } else {
            return {functor()};
        }
    }
};

template<typename R, typename... Args>
std::function<func_t(ScriptEngine*)> external(std::function<R(Args...)> function) {
    static_assert(!std::is_reference_v<R>);
    static_assert((!sizeof...(Args) || ... || !std::is_reference_v<Args>));
    return [function](ScriptEngine* engine) {
        return std::make_shared<Function>(Function{parseType<R>(), externalParameters<Args...>(std::index_sequence_for<Args...>()),
                                                   std::make_unique<ExternalFunctionInvocationNode<R, Args...>>(engine, function)});
    };
}

}