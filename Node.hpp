#pragma once

#include <utility>

#include "Engine.hpp"
#include "Operator.hpp"

namespace SauScript {

struct ExprNode {
    std::string descriptor;
    ScriptEngine* engine;
    SourceLocation location;
    ExprNode(std::string descriptor, ScriptEngine* engine, SourceLocation location)
        : descriptor(std::move(descriptor)), engine(engine), location(location) {}

    void push() const try {
        do_push();
    } catch (PlainRuntimeError& pre) {
        pre.rethrow(location);
    }
    virtual void do_push() const = 0;
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
        buf += pid + "[\"" + descriptor + "\"]\n";
        for (auto&& child : children()) {
            ++id;
            buf += pid + "-->" + std::to_string(id) + "\n";
            child->walk(buf, id);
        }
    }
};

struct ValNode : ExprNode {
    Object val;
    ValNode(ScriptEngine* engine, SourceLocation location, Object val): ExprNode(
            val.type() == Type::FUNC ? get<func_t>(val.object)->descriptor() : val.toString(),
            engine, location), val(std::move(val)) {}

    void do_push() const override {
        engine->push(val);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        if (val.type() == Type::FUNC) {
            auto&& fn = *get<func_t>(val.object);
            return {fn.expr.get()};
        }
        return {};
    }

    [[nodiscard]] std::string toString() const override {
        return val.toString();
    }
};

struct RefNode : ExprNode {
    std::string name;
    RefNode(ScriptEngine* engine, SourceLocation location, std::string name): ExprNode(name, engine, location), name(std::move(name)) {}

    void do_push() const override {
        engine->push(engine->findOperand(name));
    }

    void initialize() {
        auto rhs = engine->pop().val();
        if (engine->local().contains(name))
            throw RuntimeError("redefinition of '" + name + "' in the local scope" + location.at());
        engine->push(&(engine->local()[name] = rhs));
    }
};

struct OpUnaryNode : ExprNode {
    std::unique_ptr<ExprNode> operand;
    Operator const* op;
    bool postfix;
    OpUnaryNode(ScriptEngine* engine, SourceLocation location,
                std::unique_ptr<ExprNode> operand,
                Operator const* op, bool postfix = false): ExprNode(std::string(op->literal), engine, location),
                operand(std::move(operand)), op(op), postfix(postfix) {}

    void do_push() const override {
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
    OpBinaryNode(ScriptEngine* engine, SourceLocation location,
                 std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs,
                 Operator const* op): ExprNode(std::string(op->literal), engine, location),
                 lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

    void do_push() const override {
        op->asBinary()(lhs.get(), rhs.get());
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "(" + lhs->toString() + " " + std::string(op->literal) + " " + rhs->toString() + ")";
    }
};

struct OpTernaryNode : ExprNode {
    std::unique_ptr<ExprNode> cond, lhs, rhs;
    OpTernaryNode(ScriptEngine* engine, SourceLocation location,
                  std::unique_ptr<ExprNode> cond,
                  std::unique_ptr<ExprNode> lhs,
                  std::unique_ptr<ExprNode> rhs): ExprNode("?:", engine, location),
                  cond(std::move(cond)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    void do_push() const override {
        cond->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().asBool() ? lhs->push() : rhs->push();
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "(" + cond->toString() + " ? " + lhs->toString() + " : " + rhs->toString() + ")";
    }
};

struct OpIndexNode : ExprNode {
    std::unique_ptr<ExprNode> list;
    std::unique_ptr<ExprNode> index;
    OpIndexNode(ScriptEngine* engine, SourceLocation location,
                std::unique_ptr<ExprNode> list,
                std::unique_ptr<ExprNode> index): ExprNode("[]", engine, location),
                list(std::move(list)), index(std::move(index)) {}

    void do_push() const override {
        list->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto t = engine->pop();
        auto a = std::get<list_t>(t.val().object);
        index->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto b = engine->pop().val().asInt();
        if (b < 0 || b >= a->objs.size()) throw RuntimeError("list index access out of bound" + location.at());
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
    OpInvokeNode(ScriptEngine* engine, SourceLocation location,
                 std::unique_ptr<ExprNode> func,
                 std::vector<std::unique_ptr<ExprNode>> args): ExprNode("()", engine, location),
                 func(std::move(func)), args(std::move(args)) {}

    void do_push() const override {
        std::vector<Object> objects;
        for (auto&& arg : args) {
            arg->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            objects.push_back(engine->pop().val());
        }
        func->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().invoke(engine, objects);
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret{func.get()};
        for (auto&& arg : args) {
            ret.push_back(arg.get());
        }
        return ret;
    }

    [[nodiscard]] std::string toString() const override {
        std::string ret = "(";
        ret += func->toString();
        ret += ")(";
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
    ListLiteralNode(ScriptEngine* engine, SourceLocation location,
                    std::vector<std::unique_ptr<ExprNode>> objs): ExprNode("[]", engine, location),
                    objs(std::move(objs)) {}

    void do_push() const override {
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

struct StmtsNode : ExprNode {
    std::vector<std::unique_ptr<ExprNode>> stmts;
    StmtsNode(ScriptEngine* engine, SourceLocation location, std::vector<std::unique_ptr<ExprNode>> stmts)
        : ExprNode("{}", engine, location), stmts(std::move(stmts)) {}

    void do_push() const override {
        ScriptScope scope(engine);
        push_unscoped();
    }

    void push_unscoped() const {
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

    [[nodiscard]] std::string toStringUnscoped() const {
        std::string ret;
        bool first = true;
        for (auto&& stmt : stmts) {
            if (first) { first = false; } else { ret += ","; }
            ret += stmt->toString();
        }
        return ret;
    }
};

struct WhileNode : ExprNode {
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> loop;
    WhileNode(ScriptEngine* engine, SourceLocation location,
              std::unique_ptr<ExprNode> cond,
              std::unique_ptr<ExprNode> loop): ExprNode("while", engine, location),
              cond(std::move(cond)), loop(std::move(loop)) {}

    void do_push() const override {
        ScriptScope scope(engine);
        std::vector<Object> yield;
        begin:
        cond->push();
        if (engine->jumpTarget == JumpTarget::NONE) {
            if (!engine->pop().val().asBool()) goto end;
            loop->push();
            if (engine->jumpTarget == JumpTarget::NONE)
                engine->pop().val().yield(yield);
        }
        if (engine->jumpTarget == JumpTarget::CONTINUE) {
            engine->jumpTarget = JumpTarget::NONE;
            engine->yield.yield(yield);
        }
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        goto begin;
        end:
        if (engine->jumpTarget == JumpTarget::BREAK) {
            engine->jumpTarget = JumpTarget::NONE;
            engine->push(engine->yield);
        } else if (engine->jumpTarget == JumpTarget::NONE) {
            engine->push(Object{std::make_shared<List>(std::move(yield))});
        }
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), loop.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "while " + cond->toString() + loop->toString();
    }
};

struct ForNode : ExprNode {
    std::unique_ptr<StmtsNode> init;
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StmtsNode> iter;
    std::unique_ptr<ExprNode> loop;
    ForNode(ScriptEngine* engine, SourceLocation location,
            std::unique_ptr<StmtsNode> init,
            std::unique_ptr<ExprNode> cond,
            std::unique_ptr<StmtsNode> iter,
            std::unique_ptr<ExprNode> loop): ExprNode("for", engine, location),
            init(std::move(init)), cond(std::move(cond)), iter(std::move(iter)), loop(std::move(loop)) {}

    void do_push() const override {
        ScriptScope scope(engine);
        std::vector<Object> yield;
        init->push_unscoped();
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        engine->pop();
        begin:
        cond->push();
        if (engine->jumpTarget == JumpTarget::NONE) {
            if (!engine->pop().val().asBool()) goto end;
            loop->push();
            if (engine->jumpTarget == JumpTarget::NONE)
                engine->pop().val().yield(yield);
        }
        if (engine->jumpTarget == JumpTarget::CONTINUE) {
            engine->jumpTarget = JumpTarget::NONE;
            engine->yield.yield(yield);
        }
        if (engine->jumpTarget != JumpTarget::NONE) goto end;
        iter->push_unscoped();
        engine->pop();
        goto begin;
        end:
        if (engine->jumpTarget == JumpTarget::BREAK) {
            engine->jumpTarget = JumpTarget::NONE;
            engine->push(engine->yield);
        } else if (engine->jumpTarget == JumpTarget::NONE) {
            engine->push(Object{std::make_shared<List>(std::move(yield))});
        }
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {init.get(), cond.get(), iter.get(), loop.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "for " + init->toStringUnscoped() + ";" + cond->toString() + ";" + iter->toStringUnscoped() + loop->toString();
    }
};

struct ForEachNode : ExprNode {
    std::string name;
    std::unique_ptr<ExprNode> iter;
    std::unique_ptr<ExprNode> loop;
    ForEachNode(ScriptEngine* engine, SourceLocation location,
                std::string name,
                std::unique_ptr<ExprNode> iter,
                std::unique_ptr<ExprNode> loop): ExprNode("for-each " + name, engine, location),
                name(std::move(name)), iter(std::move(iter)), loop(std::move(loop)) {}

    void do_push() const override {
        ScriptScope scope(engine);
        std::vector<Object> yield;
        iter->push();
        if (engine->jumpTarget == JumpTarget::NONE) {
            for (auto list = engine->pop().val().iterable(); auto&& obj : list->objs) {
                engine->local()[name] = obj;
                loop->push();
                if (engine->jumpTarget == JumpTarget::NONE)
                    engine->pop().val().yield(yield);
                if (engine->jumpTarget == JumpTarget::CONTINUE) {
                    engine->jumpTarget = JumpTarget::NONE;
                    engine->yield.yield(yield);
                }
                if (engine->jumpTarget != JumpTarget::NONE) break;
            }
        }
        if (engine->jumpTarget == JumpTarget::BREAK) {
            engine->jumpTarget = JumpTarget::NONE;
            engine->push(engine->yield);
        } else if (engine->jumpTarget == JumpTarget::NONE) {
            engine->push(Object{std::make_shared<List>(std::move(yield))});
        }
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {iter.get(), loop.get()};
    }

    [[nodiscard]] std::string toString() const override {
        return "for " + name + ":" + iter->toString() + loop->toString();
    }
};

struct IfElseNode : ExprNode {
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> then;
    std::unique_ptr<ExprNode> else_;
    IfElseNode(ScriptEngine* engine, SourceLocation location,
               std::unique_ptr<ExprNode> cond,
               std::unique_ptr<ExprNode> then,
               std::unique_ptr<ExprNode> else_): ExprNode("if-else", engine, location),
               cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) {}

    void do_push() const override {
        ScriptScope scope(engine);
        cond->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().asBool() ? then->push() : else_->push();
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
    TryCatchNode(ScriptEngine* engine, SourceLocation location,
                 std::unique_ptr<ExprNode> try_,
                 std::string name,
                 std::unique_ptr<ExprNode> catch_): ExprNode("try-catch", engine, location),
                 try_(std::move(try_)), name(std::move(name)), catch_(std::move(catch_)) {}

    void do_push() const override {
        {
            ScriptScope scope(engine);
            try_->push();
        }
        if (engine->jumpTarget == JumpTarget::THROW) {
            engine->jumpTarget = JumpTarget::NONE;
            ScriptScope scope(engine);
            engine->local()[name] = engine->yield;
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
            : ExprNode("<external function invocation>", engine, {}), function(function) {}

    void do_push() const override {
        engine->push(eval(std::index_sequence_for<Args...>()));
    }

    template<size_t... I>
    Object eval(std::index_sequence<I...>) const {
        auto functor = std::bind(function, engine->findOperand(externalParameterName<I>()).val().template as<Args>()...);
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