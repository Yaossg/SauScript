#include <utility>
#include <vector>
#include <stack>
#include <string>
#include <functional>
#include <variant>
#include <map>
#include <stdexcept>
#include <memory>
#include <optional>
#include <cstdio>
#include <cstring>
#include <string_view>
#include <cstdint>

namespace SauScript {

struct Function;
struct Object;
struct Token;
struct Operand;
struct Operator;
struct ScriptEngine;
struct ExprNode;
struct StmtsNode;

struct SyntaxError : std::logic_error {
    SyntaxError(std::string const& msg): std::logic_error(msg) {}
};

struct RuntimeError : std::runtime_error {
    RuntimeError(std::string const& msg): std::runtime_error(msg) {}
};

template<typename... Ts>
struct overloaded : Ts... {
    explicit overloaded(Ts... ts): Ts(ts)... {}
    using Ts::operator()...;
};

inline std::string at(int line) {
    return line > 0 ? " at line " +  std::to_string(line) : " at unknown line";
}

inline bool skipLineBreak(char const*& current, bool strict, int& line) {
    switch (*current) {
        case '#':
            while (*current && *current != '\n') ++current;
            if (*current == '\n')
                case '\n': ++current;
            ++line;
            return true;
        case ';':
            if (strict) return false;
            skipLineBreak(++current, strict, line);
            return true;
    }
    return false;
}

inline bool isIdentifierStart(char ch) {
    return std::isalpha(ch) || ch == '_' || ch == '$';
}

inline bool isIdentifierContinue(char ch) {
    return std::isalnum(ch) || ch == '_' || ch == '$';
}

inline bool isNumberStart(char ch) {
    return std::isdigit(ch);
}

static std::string_view TYPE_NAMES[] = {"void", "int", "real", "func", "list"};

enum class Type : size_t {
    VOID, INT, REAL, FUNC, LIST,

    NAT // not a type
};

inline std::string_view nameOf(Type type) {
    return TYPE_NAMES[(size_t) type];
}

using int_t = long long;
using real_t = double;
using func_t = std::shared_ptr<Function>;
using list_t = std::shared_ptr<std::vector<Object>>;

template<typename T>
constexpr Type parseType() {
    if constexpr(std::is_void_v<T> || std::is_same_v<T, std::monostate>) {
        return Type::VOID;
    } else if constexpr(std::is_integral_v<T>) {
        return Type::INT;
    } else if constexpr(std::is_floating_point_v<T>) {
        return Type::REAL;
    } else if constexpr(std::is_same_v<T, func_t>) {
        return Type::FUNC;
    } else if constexpr(std::is_same_v<T, list_t>) {
        return Type::LIST;
    } else throw SyntaxError("unsupported external type");
}

struct Parameter {
    Type type;
    std::string name;

    [[nodiscard]] std::string type_name() const {
        return std::string(nameOf(type));
    }

    [[nodiscard]] std::string toString() const {
        return name + ": " + type_name();
    }
};

struct Function {
    Type returnType;
    std::vector<Parameter> parameters;
    std::unique_ptr<ExprNode> stmt;

    [[nodiscard]] std::string descriptor() const {
        std::string ret = "function(";
        bool first = true;
        for (auto&& parameter : parameters) {
            if (first) { first = false; } else { ret += ", "; }
            ret += parameter.toString();
        }
        ret += "): ";
        ret += nameOf(returnType);
        return ret;
    }

    [[nodiscard]] std::string toString() const;
};

struct Object {
    std::variant<std::monostate, int_t, real_t, func_t, list_t> object;

    [[nodiscard]] Type type() const {
        return (Type) object.index();
    }

    [[nodiscard]] std::string type_name() const {
        return std::string(nameOf(type()));
    }

    [[nodiscard]] bool asBool(int line) const {
        if (type() == Type::INT)
            return std::get<int_t>(object) != 0;
        throw RuntimeError("expected int as bool but got " + type_name() + at(line));
    }

    [[nodiscard]] int_t& asInt(int line) {
        if (type() == Type::INT)
            return std::get<int_t>(object);
        throw RuntimeError("expected int but got " + type_name() + at(line));
    }

    [[nodiscard]] std::variant<int_t*, real_t*> asNumber(int line) {
        switch (type()) {
            case Type::INT: return &std::get<int_t>(object);
            case Type::REAL: return &std::get<real_t>(object);
        }
        throw RuntimeError("expected number but got " + type_name() + at(line));
    }

    [[nodiscard]] Object promote(int line) const {
        if (type() == Type::INT)
            return Object{(real_t)std::get<int_t>(object)};
        throw RuntimeError("invalid promotion" + at(line));
    }

    void invoke(ScriptEngine* engine, int line, std::vector<Object> const& arguments) const;

    [[nodiscard]] std::string toString() const;

    template<typename T>
    [[nodiscard]] T as() const {
        return std::get<(size_t)parseType<T>()>(object);
    }
};

struct Operand {
    std::variant<Object, Object*> val_or_ref;
    Operand() = default;
    Operand(Operand const&) = default;
    Operand(Object val): val_or_ref(val) {}
    Operand(Object* ref): val_or_ref(ref) {}

    [[nodiscard]] Object val() const {
        return std::visit(overloaded {
            [](Object const& o) { return o; },
            [](Object* o) { return *o; }
            }, val_or_ref);
    }

    [[nodiscard]] Object* ref(int line) const {
        if (std::holds_alternative<Object*>(val_or_ref))
            return std::get<Object*>(val_or_ref);
        throw RuntimeError("rvalue cannot be used as lvalue" + at(line));
    }
};

// 0 io, parameterized jump, assignment, ternary
// 1 logical or
// 2 logical and
// 3 bit or
// 4 bit xor
// 5 bit and
// 6 equality
// 7 inequality
// 8 shift
// 9 addition
// 10 multiplication
// 11 unary prefix
// 12 postfix
// 13 primary expression

constexpr int LEVEL_ROOT = 0;
constexpr int LEVEL_UNARY_PREFIX = 11;
constexpr int LEVEL_UNARY_POSTFIX = 12;
constexpr int LEVEL_PRIMARY = 13;

struct Operator {
    using Unary = std::function<void(ExprNode*)>;
    using Binary = std::function<void(ExprNode*, ExprNode*)>;
    using Fn = std::variant<Unary, Binary>;

    std::string_view literal;
    Fn fn;

    [[nodiscard]] Unary const& asUnary() const { return std::get<Unary>(fn); }
    [[nodiscard]] Binary const& asBinary() const { return std::get<Binary>(fn); }
};

extern const std::vector<Operator> OPERATORS[13];

inline Operator const* findOperator(std::string const& literal, int level) {
    auto first = OPERATORS[level].begin(), last = OPERATORS[level].end();
    auto result = std::find_if(first, last, [&literal](Operator const& op) { return op.literal == literal; });
    return result != last ? &*result : nullptr;
}

static thread_local std::vector<std::string_view> OP_TOKENS;

inline std::vector<std::string_view> const& opTokens() {
    if (OP_TOKENS.empty()) {
        OP_TOKENS = {"?", ":", ","};
        for (auto&& ops : OPERATORS) {
            for (auto&& op : ops) {
                OP_TOKENS.push_back(op.literal);
            }
        }
    }
    return OP_TOKENS;
}

namespace Keyword {
const std::string_view KW_TOKENS[] =
        {"while", "do", "if", "else", "try", "catch", "break", "continue", "for", "function"};
enum {
    WHILE, DO, IF, ELSE, TRY, CATCH, BREAK, CONTINUE, FOR, FUNCTION,

    NAK // not a keyword
};

inline int parse(std::string const& kw) {
    using namespace Keyword;
    auto first = std::begin(KW_TOKENS), last = std::end(KW_TOKENS);
    return std::find(first, last, kw) - first;
}
}

template<typename R, typename... Args>
std::function<func_t(ScriptEngine*)> external(std::function<R(Args...)> function);

void installEnvironment(ScriptEngine* engine);

enum class JumpTarget {
    NONE, CONTINUE, BREAK, RETURN, THROW
};

struct ScriptEngine {
    using Scope = std::map<std::string, Object>;
    std::deque<Scope> scopes{{}, {}};
    FILE *out, *in;
    JumpTarget jumpTarget = JumpTarget::NONE;
    int jumpFrom = 0;
    Object target;

    std::stack<Operand> stack;

    void push(Operand const& operand) {
        stack.push(operand);
    }

    Operand& top() {
        return stack.top();
    }

    Operand pop() {
        auto operand = top();
        stack.pop();
        return operand;
    }

    ScriptEngine(FILE* out = stdout, FILE* in = stdin): out(out), in(in) {
        installEnvironment(this);
    }

    Scope& global() { return scopes.front(); }
    Scope& local() { return scopes.back(); }

    template<typename Fn>
    void installExternalFunction(std::string const& name, Fn fn) {
        Object wrapped{external(std::function{fn})(this)};
        if (!global().contains(name)) {
            global()[name] = wrapped;
        } else {
            switch (global()[name].type()) {
                case Type::FUNC:
                    global()[name] = {std::make_shared<std::vector<Object>>(
                            std::vector<Object>{global()[name], wrapped})};
                    break;
                case Type::LIST:
                    get<list_t>(global()[name].object)->push_back(wrapped);
                    break;
                default:
                    throw RuntimeError("Assertion failed");
            }
        }
    }

    Operand findOperand(std::string const& name, int line);

    [[nodiscard]] std::unique_ptr<ExprNode> compileExpression(Token*& current, int level);
    [[nodiscard]] std::unique_ptr<ExprNode> compileWhile(Token*& current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileDoWhile(Token*& current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFor(Token*& current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileIfElse(Token*& current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileTryCatch(Token*& current);
    [[nodiscard]] std::unique_ptr<ExprNode> compileFunction(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtsNode> compileStatements(Token*& current);

    [[nodiscard]] std::unique_ptr<StmtsNode> compile(char const* script);
    void exec(char const* script, FILE* err = stderr);
};

struct ScriptScope {
    ScriptEngine* engine;
    ScriptScope(ScriptEngine* engine): engine(engine) {
        engine->scopes.emplace_back();
    }
    ~ScriptScope() {
        if (engine->jumpTarget == JumpTarget::NONE && std::uncaught_exceptions() == 0)
            engine->push(engine->pop().val());
        engine->scopes.pop_back();
    }
};

struct ExprNode {
    ScriptEngine* engine;
    int line;
    ExprNode(ScriptEngine* engine, int line): engine(engine), line(line) {}

    virtual void push() const = 0;

    [[nodiscard]] std::string walk() const {
        int id = 0;
        std::string buf;
        walk(buf, id);
        return buf;
    }

    [[nodiscard]] virtual std::vector<ExprNode*> children() const {
        return {};
    }

    [[nodiscard]] virtual std::string descriptor() const = 0;

    [[nodiscard]] virtual std::string toString() const {
        return descriptor();
    }

    virtual ~ExprNode() = default;

private:
    void walk(std::string& buf, int& id) const {
        std::string pid = std::to_string(id);
        buf += "id_" + pid + "[\"" + descriptor() + "\"]\n";
        for (auto&& child : children()) {
            ++id;
            buf += "id_" + pid + "-->" + "id_" + std::to_string(id) + "\n";
            child->walk(buf, id);
        }
    }
};

struct ValNode : ExprNode {
    Object val;
    ValNode(ScriptEngine* engine, int line, Object val): ExprNode(engine, line), val(std::move(val)) {}

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

    [[nodiscard]] std::string descriptor() const override {
        if (val.type() == Type::FUNC) {
            return get<func_t>(val.object)->descriptor();
        }
        return val.toString();
    }
};

struct RefNode : ExprNode {
    std::string name;
    RefNode(ScriptEngine* engine, int line, std::string name): ExprNode(engine, line), name(std::move(name)) {}

    void push() const override {
        engine->push(engine->findOperand(name, line));
    }

    [[nodiscard]] std::string descriptor() const override {
        return name;
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
    OpUnaryNode(ScriptEngine* engine, int line,
                std::unique_ptr<ExprNode> operand,
                Operator const* op): ExprNode(engine, line),
                operand(std::move(operand)), op(op) {}

    void push() const override {
        op->asUnary()(operand.get());
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {operand.get()};
    }

    [[nodiscard]] std::string descriptor() const override {
        return std::string(op->literal);
    }

    [[nodiscard]] std::string toString() const override {
        auto first = &*begin(OPERATORS[LEVEL_UNARY_POSTFIX]), last = &*end(OPERATORS[LEVEL_UNARY_POSTFIX]);
        if (first <= op && op < last) // postfix
            return "(" + operand->toString() + " " + std::string(op->literal) + ")";
        else // prefix
            return "(" + std::string(op->literal) + " " + operand->toString() + ")";
    }
};

struct OpBinaryNode : ExprNode {
    std::unique_ptr<ExprNode> lhs, rhs;
    Operator const* op;
    OpBinaryNode(ScriptEngine* engine, int line,
                 std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs,
                 Operator const* op): ExprNode(engine, line),
                 lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

    void push() const override {
        op->asBinary()(lhs.get(), rhs.get());
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string descriptor() const override {
        return std::string(op->literal);
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
                  std::unique_ptr<ExprNode> rhs): ExprNode(engine, line),
                  cond(std::move(cond)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    void push() const override {
        cond->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().asBool(line) ? lhs->push() : rhs->push();
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), lhs.get(), rhs.get()};
    }

    [[nodiscard]] std::string descriptor() const override {
        return "?:";
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
                std::unique_ptr<ExprNode> index): ExprNode(engine, line),
                list(std::move(list)), index(std::move(index)) {}

    void push() const override {
        list->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto t = engine->pop();
        auto a = std::get<list_t>(t.val().object);
        index->push();
        if (engine->jumpTarget != JumpTarget::NONE) { engine->pop(); return; }
        auto b = engine->pop().val().asInt(line);
        if (b < 0 || b >= a->size()) throw RuntimeError("list index out of bound" + at(line));
        if (t.val_or_ref.index())
            engine->push(&a->at(b));
        else
            engine->push(a->at(b));
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {list.get(), index.get()};
    }

    [[nodiscard]] std::string descriptor() const override {
        return "[]";
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
                 std::vector<std::unique_ptr<ExprNode>> args): ExprNode(engine, line),
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

    [[nodiscard]] std::string descriptor() const override {
        return "()";
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

struct OpListNode : ExprNode {
    std::vector<std::unique_ptr<ExprNode>> objs;
    OpListNode(ScriptEngine* engine, int line,
               std::vector<std::unique_ptr<ExprNode>> objs): ExprNode(engine, line),
               objs(std::move(objs)) {}

    void push() const override {
        std::vector<Object> objects;
        for (auto&& obj : objs) {
            obj->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            objects.push_back(engine->pop().val());
        }
        engine->push(Object{std::make_shared<std::vector<Object>>(std::move(objects))});
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        std::vector<ExprNode*> ret;
        for (auto&& obj : objs) {
            ret.push_back(obj.get());
        }
        return ret;
    }

    [[nodiscard]] std::string descriptor() const override {
        return "[]";
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
    NoopNode(ScriptEngine* engine, int line): ExprNode(engine, line) {}

    void push() const override {
        engine->push({});
    }
    [[nodiscard]] std::string descriptor() const override {
        return "{}";
    }
};

struct StmtsNode : ExprNode {
    std::vector<std::unique_ptr<ExprNode>> stmts;
    StmtsNode(ScriptEngine* engine, int line, std::vector<std::unique_ptr<ExprNode>> stmts)
        : ExprNode(engine, line), stmts(std::move(stmts)) {}

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

    [[nodiscard]] std::string descriptor() const override {
        return "{}";
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
            : ExprNode(engine, line), jumpTarget(jumpTarget) {}

    void push() const override {
        engine->target = {};
        engine->jumpTarget = jumpTarget;
        engine->jumpFrom = line;
    }

    [[nodiscard]] std::string descriptor() const override {
        switch (jumpTarget) {
            case JumpTarget::BREAK:
                return "break";
            case JumpTarget::CONTINUE:
                return "continue";
        }
        throw RuntimeError("Assertion failed");
    }
};

struct WhileNode : ExprNode {
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> loop;
    WhileNode(ScriptEngine* engine, int line,
              std::unique_ptr<ExprNode> cond,
              std::unique_ptr<ExprNode> loop)
            : ExprNode(engine, line), cond(std::move(cond)), loop(std::move(loop)) {}

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

    [[nodiscard]] std::string descriptor() const override {
        return "while";
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
            : ExprNode(engine, line), loop(std::move(loop)), cond(std::move(cond)) {}

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

    [[nodiscard]] std::string descriptor() const override {
        return "do-while";
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
            : ExprNode(engine, line), init(std::move(init)), cond(std::move(cond)), iter(std::move(iter)), loop(std::move(loop)) {}

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

    [[nodiscard]] std::string descriptor() const override {
        return "for";
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
            : ExprNode(engine, line), cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) {}

    void push() const override {
        ScriptScope scope(engine);
        cond->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        engine->pop().val().asBool(line) ? then->push() : else_->push();
    }

    [[nodiscard]] std::vector<ExprNode*> children() const override {
        return {cond.get(), then.get(), else_.get()};
    }

    [[nodiscard]] std::string descriptor() const override {
        return "if-else";
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
            : ExprNode(engine, line), try_(std::move(try_)), name(std::move(name)), catch_(std::move(catch_)) {}

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

    [[nodiscard]] std::string descriptor() const override {
        return "try-catch";
    }

    [[nodiscard]] std::string toString() const override {
        return "try" + try_->toString() + "catch " + name + catch_->toString();
    }
};

enum class TokenType {
    PUNCTUATOR, IDENTIFIER, KEYWORD, LINEBREAK,
    LITERAL_BOOL, LITERAL_INT, LITERAL_REAL, BRACE,

    TERMINATOR
};

struct Token {
    TokenType type;
    std::variant<int, std::string, int_t, real_t> parameter;
    int line = 0;
    bool operator ==(Token const& other) const {
        return type == other.type && parameter == other.parameter;
    }
    Token& at(int where) {
        line = where;
        return *this;
    }
    [[nodiscard]] std::string at() const {
        return SauScript::at(line);
    }
    [[nodiscard]] int keyword() const {
        return std::get<int>(parameter);
    }
    [[nodiscard]] std::string identifier() const {
        return std::get<std::string>(parameter);
    }
    [[nodiscard]] std::string punctuator() const {
        return std::get<std::string>(parameter);
    }
    [[nodiscard]] bool literal_bool() const {
        return std::get<int>(parameter);
    }
    [[nodiscard]] int_t literal_int() const {
        return std::get<int_t>(parameter);
    }
    [[nodiscard]] real_t literal_real() const {
        return std::get<real_t>(parameter);
    }
    static Token punctuator(std::string p) {
        return {TokenType::PUNCTUATOR, p};
    }
    static Token identifier(std::string id) {
        return {TokenType::IDENTIFIER, id};
    }
    static Token literal_bool(bool x) {
        return {TokenType::LITERAL_BOOL, x};
    }
    static Token literal_int(int_t x) {
        return {TokenType::LITERAL_INT, x};
    }
    static Token literal_real(real_t x) {
        return {TokenType::LITERAL_REAL, x};
    }
    static Token keyword(int keyword) {
        return {TokenType::KEYWORD, keyword};
    }
    static Token braceLeft() {
        return {TokenType::BRACE, 0};
    }
    static Token braceRight() {
        return {TokenType::BRACE, 1};
    }
    static Token parenLeft() {
        return {TokenType::BRACE, 2};
    }
    static Token parenRight() {
        return {TokenType::BRACE, 3};
    }
    static Token bracketLeft() {
        return {TokenType::BRACE, 4};
    }
    static Token bracketRight() {
        return {TokenType::BRACE, 5};
    }
    static Token linebreak() {
        return {TokenType::LINEBREAK, 0};
    }
    static Token terminator() {
        return {TokenType::TERMINATOR, 0};
    }
};

inline Type parseType(Token const& token) {
    if (token.type != TokenType::IDENTIFIER) throw SyntaxError("expected type name" + token.at());
    std::string name = token.identifier();
    auto first = std::begin(TYPE_NAMES), last = std::end(TYPE_NAMES);
    Type type = Type(std::find(first, last, name) - first);
    if (type == Type::NAT)
        throw SyntaxError("invalid type name" + token.at());
    return type;
}

template<size_t I>
std::string externalParameterName() {
    return "$" + std::to_string(I);
}

template<typename... Args, size_t... I>
std::vector<Parameter> externalParameters(std::index_sequence<I...>) {
    return {{parseType<Args>(), externalParameterName<I>()}...};
}

template<typename R, typename... Args>
struct ExternalFunctionInvocationNode : ExprNode {
    std::function<R(Args...)> function;
    ExternalFunctionInvocationNode(ScriptEngine* engine, std::function<R(Args...)> function)
            : ExprNode(engine, 0), function(function) {}
    void push() const override {
        engine->push(eval(std::index_sequence_for<Args...>()));
    }

    template<size_t... I>
    Object eval(std::index_sequence<I...>) const;

    [[nodiscard]] std::string descriptor() const override {
        return "<external function invocation>";
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

template<typename R, typename... Args>
template<size_t... I>
Object ExternalFunctionInvocationNode<R, Args...>::eval(std::index_sequence<I...>) const {
    auto functor = std::bind(function, engine->findOperand(externalParameterName<I>(), 0).val().template as<Args>()...);
    if constexpr(std::is_void_v<R>) {
        functor();
        return {};
    } else {
        return {functor()};
    }
}

}