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
    [[nodiscard]] virtual std::vector<ExprNode*> children() const { return {}; }
    [[nodiscard]] virtual std::string dump() const { return descriptor; }

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
    ValNode(ScriptEngine* engine, SourceLocation location, Object val)
        : ExprNode(val.toString(StringifyScheme::TREE_NODE), engine, location), val(std::move(val)) {}

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        if (val.type() == Type::FUNC) {
            auto&& fn = *get<func_t>(val.object);
            return {fn.expr.get()};
        }
        return {};
    }

    [[nodiscard]] std::string dump() const override {
        return val.toString(StringifyScheme::DUMP);
    }
};

struct RefNode : ExprNode {
    std::string name;
    RefNode(ScriptEngine* engine, SourceLocation location, std::string name)
        : ExprNode(name, engine, location), name(std::move(name)) {}

    void initialize() {
        auto rhs = engine->pop().val();
        if (engine->local().contains(name))
            runtime("redefinition of '" + name + "' in the local scope", location);
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
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {operand.get()};
    }

    [[nodiscard]] std::string dump() const override {
        if (postfix)
            return "(" + operand->dump() + " " + std::string(op->literal) + ")";
        else
            return "(" + std::string(op->literal) + " " + operand->dump() + ")";
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
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "(" + lhs->dump() + " " + std::string(op->literal) + " " + rhs->dump() + ")";
    }
};

struct OpTernaryNode : ExprNode {
    std::unique_ptr<ExprNode> cond, lhs, rhs;
    OpTernaryNode(ScriptEngine* engine, SourceLocation location,
                  std::unique_ptr<ExprNode> cond,
                  std::unique_ptr<ExprNode> lhs,
                  std::unique_ptr<ExprNode> rhs): ExprNode("?:", engine, location),
                  cond(std::move(cond)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "(" + cond->dump() + " ? " + lhs->dump() + " : " + rhs->dump() + ")";
    }
};

struct OpIndexNode : ExprNode {
    std::unique_ptr<ExprNode> object;
    std::unique_ptr<ExprNode> index;
    OpIndexNode(ScriptEngine* engine, SourceLocation location,
                std::unique_ptr<ExprNode> object,
                std::unique_ptr<ExprNode> index): ExprNode("[]", engine, location),
                object(std::move(object)), index(std::move(index)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {object.get(), index.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "(" + object->dump() + ")[" + index->dump() + "]";
    }
};

struct OpInvokeNode : ExprNode {
    std::unique_ptr<ExprNode> func;
    std::vector<std::unique_ptr<ExprNode>> args;
    OpInvokeNode(ScriptEngine* engine, SourceLocation location,
                 std::unique_ptr<ExprNode> func,
                 std::vector<std::unique_ptr<ExprNode>> args): ExprNode("()", engine, location),
                 func(std::move(func)), args(std::move(args)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret{func.get()};
        for (auto&& arg : args) {
            ret.push_back(arg.get());
        }
        return ret;
    }

    [[nodiscard]] std::string dump() const override {
        std::string ret = "(";
        ret += func->dump();
        ret += ")(";
        bool first = true;
        for (auto&& arg : args) {
            if (first) { first = false; } else { ret += ", "; }
            ret += arg->dump();
        }
        ret += ")";
        return ret;
    }
};

struct ListLiteralNode : ExprNode {
    std::vector<std::unique_ptr<ExprNode>> elements;
    ListLiteralNode(ScriptEngine* engine, SourceLocation location,
                    std::vector<std::unique_ptr<ExprNode>> elements): ExprNode("[]", engine, location),
                    elements(std::move(elements)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret;
        for (auto&& element : elements) {
            ret.push_back(element.get());
        }
        return ret;
    }

    [[nodiscard]] std::string dump() const override {
        std::string ret = "[";
        bool first = true;
        for (auto&& element : elements) {
            if (first) { first = false; } else { ret += ", "; }
            ret += element->dump();
        }
        ret += "]";
        return ret;
    }
};


struct DictLiteralNode : ExprNode {
    std::vector<std::pair<std::unique_ptr<ExprNode>, std::unique_ptr<ExprNode>>> elements;
    DictLiteralNode(ScriptEngine* engine, SourceLocation location,
                    std::vector<std::pair<std::unique_ptr<ExprNode>, std::unique_ptr<ExprNode>>> elements): ExprNode("@[]", engine, location),
                    elements(std::move(elements)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret;
        for (auto&& [key, value] : elements) {
            ret.push_back(key.get());
            ret.push_back(value.get());
        }
        return ret;
    }

    [[nodiscard]] std::string dump() const override {
        std::string ret = "@[";
        bool first = true;
        for (auto&& [key, value] : elements) {
            if (first) { first = false; } else { ret += ", "; }
            ret += key->dump();
            ret += ": ";
            ret += value->dump();
        }
        ret += "]";
        return ret;
    }
};

struct StmtsNode : ExprNode {
    std::vector<std::unique_ptr<ExprNode>> stmts;
    StmtsNode(ScriptEngine* engine, SourceLocation location, std::vector<std::unique_ptr<ExprNode>> stmts)
        : ExprNode("{}", engine, location), stmts(std::move(stmts)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret;
        for (auto&& stmt : stmts) {
            ret.push_back(stmt.get());
        }
        return ret;
    }

    [[nodiscard]] std::string dump() const override {
        std::string ret = "{";
        for (auto&& stmt : stmts) {
            ret += stmt->dump();
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
            ret += stmt->dump();
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
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), loop.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "while " + cond->dump() + loop->dump();
    }
};

struct ForNode : ExprNode {
    std::string name;
    std::unique_ptr<ExprNode> iter;
    std::unique_ptr<ExprNode> loop;
    ForNode(ScriptEngine* engine, SourceLocation location,
            std::string name,
            std::unique_ptr<ExprNode> iter,
            std::unique_ptr<ExprNode> loop): ExprNode("for " + name, engine, location),
            name(std::move(name)), iter(std::move(iter)), loop(std::move(loop)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {iter.get(), loop.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "for " + name + ":" + iter->dump() + loop->dump();
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
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), then.get(), else_.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "if " + cond->dump() + then->dump() + "else" + else_->dump();
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
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {try_.get(), catch_.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "try" + try_->dump() + "catch " + name + catch_->dump();
    }
};

template<size_t I>
std::string externalParameterName() {
    return "$" + std::to_string(I);
}

template<typename... Args, size_t... I>
std::vector<Parameter> externalParameters(std::index_sequence<I...>) {
    return {{parseType<Args>(), externalParameterName<I>()}...};
}

template<typename R, typename... Args>
struct ESNode : ExprNode {
    std::function<R(Args...)> function;

    ESNode(ScriptEngine *engine, std::function<R(Args...)> function)
            : ExprNode("<external source>", engine, {}), function(function) {}
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
func_t ScriptEngine::external(std::function<R(Args...)> function) {
    static_assert(!std::is_reference_v<R>);
    static_assert((!sizeof...(Args) || ... || !std::is_reference_v<Args>));
    return std::make_shared<Function>(Function{parseType<R>(), externalParameters<Args...>(std::index_sequence_for<Args...>()),
                                               std::make_unique<ESNode<R, Args...>>(this, function)});
}

}