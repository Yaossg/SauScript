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
            : descriptor(std::move(descriptor)), engine(engine), location(std::move(location)) {}
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
            : ExprNode(val.toString(StringifyScheme::TREE_NODE), engine, std::move(location)), val(std::move(val)) {}

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {};
    }

    [[nodiscard]] std::string dump() const override {
        return val.toString(StringifyScheme::DUMP);
    }
};

struct FnNode : ExprNode {
    Function fn;
    FnNode(ScriptEngine* engine, SourceLocation location, Function fn)
            : ExprNode(fn.descriptor(), engine, std::move(location)), fn(std::move(fn)) {}

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {fn.expr.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return fn.toString();
    }
};

struct RefNode : ExprNode {
    std::string name;
    RefNode(ScriptEngine* engine, SourceLocation location, std::string name)
        : ExprNode(name, engine, std::move(location)), name(std::move(name)) {}
};

struct OpUnaryNode : ExprNode {
    std::unique_ptr<ExprNode> operand;
    Operator const* op;
    bool postfix;
    OpUnaryNode(ScriptEngine* engine, SourceLocation location,
                std::unique_ptr<ExprNode> operand,
                Operator const* op, bool postfix = false): ExprNode(std::string(op->literal), engine, std::move(location)),
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
                 Operator const* op): ExprNode(std::string(op->literal), engine, std::move(location)),
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
                  std::unique_ptr<ExprNode> rhs): ExprNode("?:", engine, std::move(location)),
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
                std::unique_ptr<ExprNode> index): ExprNode("[]", engine, std::move(location)),
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
                 std::vector<std::unique_ptr<ExprNode>> args): ExprNode("()", engine, std::move(location)),
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
                    std::vector<std::unique_ptr<ExprNode>> elements): ExprNode("[]", engine, std::move(location)),
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
                    std::vector<std::pair<std::unique_ptr<ExprNode>, std::unique_ptr<ExprNode>>> elements): ExprNode("@[]", engine, std::move(location)),
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
        : ExprNode("{}", engine, std::move(location)), stmts(std::move(stmts)) {}
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
              std::unique_ptr<ExprNode> loop): ExprNode("while", engine, std::move(location)),
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
            std::unique_ptr<ExprNode> loop): ExprNode("for " + name, engine, std::move(location)),
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
               std::unique_ptr<ExprNode> else_): ExprNode("if-else", engine, std::move(location)),
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
                 std::unique_ptr<ExprNode> catch_): ExprNode("try-catch", engine, std::move(location)),
                 try_(std::move(try_)), name(std::move(name)), catch_(std::move(catch_)) {}
    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {try_.get(), catch_.get()};
    }

    [[nodiscard]] std::string dump() const override {
        return "try" + try_->dump() + "catch " + name + catch_->dump();
    }
};

}