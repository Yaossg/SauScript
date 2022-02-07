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
struct StmtNode;

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

inline bool skipLineBreak(char const*& str, bool strict, int& line) {
    switch (*str) {
        case '#':
            while (*str && *str != '\n' && *str != '\r') ++str;
            if (*str == '\n') ++str;
            ++line;
            return true;
        case ';':
            if (strict) return false;
            skipLineBreak(++str, strict, line);
            return true;
        case '\n':
            ++str;
            ++line;
            return true;
        case '\r':
            ++str;
            if (*str == '\n') ++str;
            ++line;
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

static std::string_view TYPE_NAMES[] = {"int", "real", "func", "void"};

enum class Type {
    INT, REAL, FUNC, VOID,

    NAT // not a type
};

using int_t = long long;
using real_t = double;
using FuncPtr = std::shared_ptr<Function>;

template<typename T>
constexpr Type parseType() {
    if constexpr(std::is_integral_v<T>) {
        return Type::INT;
    } else if constexpr(std::is_floating_point_v<T>) {
        return Type::REAL;
    } else if constexpr(std::is_void_v<T>) {
        return Type::VOID;
    }
    throw SyntaxError("unsupported external type");
}

struct Parameter {
    Type type;
    std::string name;
};

struct Function {
    Type returnType;
    std::vector<Parameter> parameters;
    std::unique_ptr<StmtNode> stmt;
};

struct Object {
    std::variant<int_t, real_t, FuncPtr, std::monostate> object;

    [[nodiscard]] bool asBool(int line) {
        if (std::holds_alternative<int_t>(object))
            return std::get<int_t>(object) != 0;
        throw RuntimeError("expected int as bool" + at(line));
    }

    int_t& asIntOp(char const* op, int line) {
        if (std::holds_alternative<int_t>(object))
            return std::get<int_t>(object);
        throw RuntimeError(std::string("expected int operand for '") + op + "'" + at(line));
    }

    [[nodiscard]] Object promote(int line) const {
        if (std::holds_alternative<int_t>(object))
            return Object{(real_t)std::get<int_t>(object)};
        throw RuntimeError("invalid promotion" + at(line));
    }

    Object invoke(int line, ScriptEngine* engine, std::vector<Object> const& arguments);

    [[nodiscard]] Type type() const {
        return (Type) object.index();
    }

    std::variant<int_t*, real_t*> asNumber(int line) {
        switch (object.index()) {
            case 0: return &std::get<0>(object);
            case 1: return &std::get<1>(object);
            case 2: throw RuntimeError("expected number but got a function" + at(line));
            default:
            case 3: throw RuntimeError("expected number but got void" + at(line));
        }
    }

    [[nodiscard]] std::string toString() const {
        return std::visit(overloaded {
            [](auto x) { return std::to_string(x); },
            [](std::monostate) { return std::string("<void>"); },
            [](FuncPtr const& ptr) {
                std::string ret = "<function(";
                bool first = true;
                for (auto&& [type, name] : ptr->parameters) {
                    if (first) { first = false; } else { ret += ", "; }
                    ret += name;
                    ret += ": ";
                    ret += TYPE_NAMES[(size_t)type];
                }
                ret += "): ";
                ret += TYPE_NAMES[(size_t)ptr->returnType];
                ret += ">";
                return ret;
            }
        }, object);
    }

    template<typename T>
    T as() {
        return std::get<(size_t)parseType<T>()>(object);
    }
};

struct Operand {
    std::variant<Object, Object*> val_or_ref;
    Operand(Operand const&) = default;
    Operand(Object val): val_or_ref(val) {}
    Operand(Object* ref): val_or_ref(ref) {}

    [[nodiscard]] Object val() const {
        static const struct {
            Object operator()(Object i) const { return i; }
            Object operator()(Object* i) const { return *i; }
        } ValueVisitor;
        return std::visit(ValueVisitor, val_or_ref);
    }
    [[nodiscard]] Object* ref(int line) const {
        if (std::holds_alternative<Object*>(val_or_ref))
            return std::get<Object*>(val_or_ref);
        throw RuntimeError("rvalue cannot be used as lvalue" + at(line));
    }
};

enum Associativity: bool {
    LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

enum class OperandType: int {
    INFIX, PREFIX, POSTFIX,
};

struct OperatorLevel {
    Associativity associativity;
    OperandType operandType;
};

inline const OperatorLevel OP_LEVELS[13] = {
        {RIGHT_TO_LEFT, OperandType::INFIX}, // assignment, ternary
        {LEFT_TO_RIGHT, OperandType::INFIX}, // logic or
        {LEFT_TO_RIGHT, OperandType::INFIX}, // logic and
        {LEFT_TO_RIGHT, OperandType::INFIX}, // bit or
        {LEFT_TO_RIGHT, OperandType::INFIX}, // bit xor
        {LEFT_TO_RIGHT, OperandType::INFIX}, // bit and
        {LEFT_TO_RIGHT, OperandType::INFIX}, // equality
        {LEFT_TO_RIGHT, OperandType::INFIX}, // inequality
        {LEFT_TO_RIGHT, OperandType::INFIX}, // shift
        {LEFT_TO_RIGHT, OperandType::INFIX}, // addition
        {LEFT_TO_RIGHT, OperandType::INFIX}, // multiplication
        {RIGHT_TO_LEFT, OperandType::PREFIX}, // unary prefix
        {LEFT_TO_RIGHT, OperandType::POSTFIX}, // unary postfix
};

struct Operator {
    using Unary = std::function<Operand(ExprNode*)>;
    using Binary = std::function<Operand(ExprNode*, ExprNode*)>;
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
        {"let", "while", "do", "end", "if", "else", "elif", "then", "try", "catch",
         "until", "repeat", "break", "continue", "for", "throw", "input", "print", "return", "function"};
enum {
    LET, WHILE, DO, END, IF, ELSE, ELIF, THEN, TRY, CATCH,
    UNTIL, REPEAT, BREAK, CONTINUE, FOR, THROW, INPUT, PRINT, RETURN, FUNCTION,

    NAK // not a keyword
};

inline int parse(std::string const& kw) {
    using namespace Keyword;
    auto first = std::begin(KW_TOKENS), last = std::end(KW_TOKENS);
    return std::find(first, last, kw) - first;
}
}

template<typename R, typename... Args>
std::function<FuncPtr(ScriptEngine*)> external(std::function<R(Args...)> function);

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

    ScriptEngine(FILE* out = stdout, FILE* in = stdin): out(out), in(in) {
        installEnvironment(this);
    }

    Scope& global() { return scopes.front(); }
    Scope& local() { return scopes.back(); }

    template<typename Fn>
    void installExternalFunction(std::string const& name, Fn fn) {
        global()[name] = Object{external(std::function{fn})(this)};
    }

    Operand findOperand(std::string const& name, int line);

    [[nodiscard]] std::unique_ptr<ExprNode> compileExpression(Token*& current, int level);
    [[nodiscard]] std::unique_ptr<ExprNode> compileExpression(std::vector<Token> tokens);
    [[nodiscard]] std::unique_ptr<StmtNode> compileStatement(std::vector<Token> tokens);
    [[nodiscard]] std::unique_ptr<StmtNode> compileWhile(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtNode> compileRepeat(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtNode> compileFor(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtNode> compileDo(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtNode> compileIf(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtNode> compileTry(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtNode> compileFunction(Token*& current);
    [[nodiscard]] std::unique_ptr<StmtNode> compileStatements(Token*& current);

    [[nodiscard]] std::unique_ptr<StmtNode> compile(char const* script);
    void exec(char const* script, FILE* err = stderr);
};

struct ScriptScope {
    ScriptEngine* engine;
    ScriptScope(ScriptEngine* engine): engine(engine) {
        engine->scopes.emplace_back();
    }
    ~ScriptScope() {
        engine->scopes.pop_back();
    }
};

struct ExprNode {
    int line;
    mutable std::optional<Operand> cache;
    ExprNode(int line, std::optional<Operand> cache = std::nullopt): line(line), cache(std::move(cache)) {}
    Operand eval() const {
        if (cache.has_value()) return cache.value();
        return do_eval();
    }
    virtual Operand do_eval() const = 0;
    virtual ~ExprNode() = default;
};

struct ValNode : ExprNode {
    ValNode(int line, Object val): ExprNode(line, val) {}
    Operand do_eval() const override {
        throw RuntimeError("Assertion failed");
    }
};

struct RefNode : ExprNode {
    ScriptEngine* engine;
    std::string name;
    RefNode(ScriptEngine* engine, int line, std::string name): engine(engine), ExprNode(line), name(std::move(name)) {}

    Operand do_eval() const override {
        cache = engine->findOperand(name, line);
        return cache.value();
    }
};

struct OpUnaryNode : ExprNode {
    std::unique_ptr<ExprNode> operand;
    Operator const* op;
    OpUnaryNode(int line,
                std::unique_ptr<ExprNode> lhs,
                Operator const* op): ExprNode(line),
                        operand(std::move(lhs)), op(op) {}

    Operand do_eval() const override {
        return op->asUnary()(operand.get());
    }
};

struct OpBinaryNode : ExprNode {
    std::unique_ptr<ExprNode> lhs, rhs;
    Operator const* op;
    OpBinaryNode(int line,
                 std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs,
                 Operator const* op): ExprNode(line),
                 lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

    Operand do_eval() const override {
        return op->asBinary()(lhs.get(), rhs.get());
    }
};

struct OpTernaryNode : ExprNode {
    std::unique_ptr<ExprNode> cond, lhs, rhs;
    OpTernaryNode(int line,
                  std::unique_ptr<ExprNode> cond,
                  std::unique_ptr<ExprNode> lhs,
                  std::unique_ptr<ExprNode> rhs): ExprNode(line),
                  cond(std::move(cond)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    Operand do_eval() const override {
        return cond->eval().val().asBool(line) ? lhs->eval() : rhs->eval();
    }
};

struct OpInvokeNode : ExprNode {
    ScriptEngine* engine;
    std::unique_ptr<ExprNode> func;
    std::vector<std::unique_ptr<ExprNode>> args;
    OpInvokeNode(int line,
                 ScriptEngine* engine,
                 std::unique_ptr<ExprNode> func,
                 std::vector<std::unique_ptr<ExprNode>> args): ExprNode(line),
                 func(std::move(func)), engine(engine), args(std::move(args)) {}

    Operand do_eval() const override {
        std::vector<Object> objects;
        for (auto&& arg : args) {
            objects.push_back(arg->eval().val());
        }
        return func->eval().val().invoke(line, engine, objects);
    }
};

struct EvalInterrupted {};

struct StmtNode {
    ScriptEngine* engine;
    StmtNode(ScriptEngine* engine): engine(engine) {}
    void exec() const try {
        if (engine->jumpTarget == JumpTarget::NONE)
            do_exec();
    } catch (EvalInterrupted) {}
    virtual void do_exec() const = 0;
    virtual ~StmtNode() = default;
};

struct StmtNoopNode : StmtNode {
    StmtNoopNode(ScriptEngine* engine): StmtNode(engine) {}
    void do_exec() const override {}
};

struct StmtSeqNode : StmtNode {
    std::vector<std::unique_ptr<StmtNode>> stmts;
    StmtSeqNode(ScriptEngine* engine, std::vector<std::unique_ptr<StmtNode>> stmts)
        : StmtNode(engine), stmts(std::move(stmts)) {}
    void do_exec() const override {
        for (auto&& stmt : stmts) {
            stmt->exec();
            if (engine->jumpTarget != JumpTarget::NONE) return;
        }
    }
};

struct StmtExprNode : StmtNode {
    std::unique_ptr<ExprNode> expr;
    StmtExprNode(ScriptEngine* engine, std::unique_ptr<ExprNode> expr)
            : StmtNode(engine), expr(std::move(expr)) {}
    void do_exec() const override { expr->eval(); }
};

struct LetNode : StmtNode {
    int line;
    std::unique_ptr<ExprNode> initializer;
    std::string name;
    LetNode(ScriptEngine* engine, int line, std::string name, std::unique_ptr<ExprNode> initializer)
            : StmtNode(engine), line(line), name(std::move(name)), initializer(std::move(initializer)) {}
    void do_exec() const override {
        if (engine->local().contains(name))
            throw RuntimeError(name + " already exists in the local scope" + at(line));
        Object val = initializer->eval().val();
        engine->local()[name] = val;
    }
};

struct JumpNode : StmtNode {
    int line;
    JumpTarget jumpTarget;
    JumpNode(ScriptEngine* engine, int line, JumpTarget jumpTarget)
            : StmtNode(engine), line(line), jumpTarget(jumpTarget) {}
    void do_exec() const override {
        engine->jumpTarget = jumpTarget;
        engine->jumpFrom = line;
    }
};

struct ReturnNode : StmtNode {
    int line;
    std::unique_ptr<ExprNode> returned;
    ReturnNode(ScriptEngine* engine, int line, std::unique_ptr<ExprNode> returned)
            : StmtNode(engine), line(line), returned(std::move(returned)) {}
    void do_exec() const override {
        engine->target = returned->eval().val();
        engine->jumpTarget = JumpTarget::RETURN;
        engine->jumpFrom = line;
    }
};

struct ThrowNode : StmtNode {
    int line;
    std::unique_ptr<ExprNode> thrown;
    ThrowNode(ScriptEngine* engine, int line, std::unique_ptr<ExprNode> thrown)
            : StmtNode(engine), line(line), thrown(std::move(thrown)) {}
    void do_exec() const override {
        engine->target = thrown->eval().val();
        engine->jumpTarget = JumpTarget::THROW;
        engine->jumpFrom = line;
    }
};

struct PrintNode : StmtNode {
    std::unique_ptr<ExprNode> print;
    PrintNode(ScriptEngine* engine, std::unique_ptr<ExprNode> print)
            : StmtNode(engine), print(std::move(print)) {}
    void do_exec() const override {
        fprintf(engine->out, "%s\n", print->eval().val().toString().c_str());
    }
};

struct InputNode : StmtNode {
    int line;
    std::string name;
    InputNode(ScriptEngine* engine, int line, std::string name)
            : StmtNode(engine), line(line), name(std::move(name)) {}
    void do_exec() const override {
        std::visit(overloaded {
            [this](int_t& a) { if (!fscanf(engine->in, "%lld", &a)) throw RuntimeError("illegal input as int"); },
            [this](real_t& a) { if (!fscanf(engine->in, "%lf", &a)) throw RuntimeError("illegal input as real"); },
            [this](FuncPtr& a) { throw RuntimeError("attempt to input a function" + at(line)); },
            [this](std::monostate& a) { throw RuntimeError("attempt to input void" + at(line)); }
        }, engine->findOperand(name, line).ref(line)->object);
    }
};


struct WhileNode : StmtNode {
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StmtNode> loop;
    WhileNode(ScriptEngine* engine,
              std::unique_ptr<ExprNode> cond,
              std::unique_ptr<StmtNode> loop)
            : StmtNode(engine), cond(std::move(cond)), loop(std::move(loop)) {}
    void do_exec() const override {
        ScriptScope scope(engine);
        while (cond->eval().val().asBool(cond->line)) {
            loop->exec();
            if (engine->jumpTarget == JumpTarget::CONTINUE)
                engine->jumpTarget = JumpTarget::NONE;
            if (engine->jumpTarget != JumpTarget::NONE) {
                if (engine->jumpTarget == JumpTarget::BREAK)
                    engine->jumpTarget = JumpTarget::NONE;
                break;
            }
        }
    }
};

struct RepeatNode : StmtNode {
    std::unique_ptr<StmtNode> loop;
    std::unique_ptr<ExprNode> cond;
    RepeatNode(ScriptEngine* engine,
               std::unique_ptr<StmtNode> loop,
               std::unique_ptr<ExprNode> cond = std::make_unique<ValNode>(0, Object{0}))
            : StmtNode(engine), loop(std::move(loop)), cond(std::move(cond)) {}
    void do_exec() const override {
        ScriptScope scope(engine);
        do {
            loop->exec();
            if (engine->jumpTarget == JumpTarget::CONTINUE)
                engine->jumpTarget = JumpTarget::NONE;
            if (engine->jumpTarget != JumpTarget::NONE) {
                if (engine->jumpTarget == JumpTarget::BREAK)
                    engine->jumpTarget = JumpTarget::NONE;
                break;
            }
        } while (!cond->eval().val().asBool(cond->line));

    }
};

struct ForNode : StmtNode {
    std::unique_ptr<StmtNode> init;
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> iter;
    std::unique_ptr<StmtNode> loop;
    ForNode(ScriptEngine* engine,
            std::unique_ptr<StmtNode> init,
            std::unique_ptr<ExprNode> cond,
            std::unique_ptr<ExprNode> iter,
            std::unique_ptr<StmtNode> loop)
            : StmtNode(engine), init(std::move(init)), cond(std::move(cond)), iter(std::move(iter)), loop(std::move(loop)) {}
    void do_exec() const override {
        ScriptScope scope(engine);
        for(init->exec(); cond->eval().val().asBool(cond->line); iter->eval()) {
            loop->exec();
            if (engine->jumpTarget == JumpTarget::CONTINUE)
                engine->jumpTarget = JumpTarget::NONE;
            if (engine->jumpTarget != JumpTarget::NONE) {
                if (engine->jumpTarget == JumpTarget::BREAK)
                    engine->jumpTarget = JumpTarget::NONE;
                break;
            }
        }
    }
};

struct DoNode : StmtNode {
    std::unique_ptr<StmtNode> stmt;
    DoNode(ScriptEngine* engine,
           std::unique_ptr<StmtNode> stmt)
            : StmtNode(engine), stmt(std::move(stmt)) {}
    void do_exec() const override {
        ScriptScope scope(engine);
        stmt->exec();
    }
};

struct IfNode : StmtNode {
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StmtNode> then;
    std::unique_ptr<StmtNode> else_;
    IfNode(ScriptEngine* engine,
           std::unique_ptr<ExprNode> cond,
           std::unique_ptr<StmtNode> then,
           std::unique_ptr<StmtNode> else_)
            : StmtNode(engine), cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) {}
    void do_exec() const override {
        ScriptScope scope(engine);
        if (cond->eval().val().asBool(cond->line)) then->exec(); else else_->exec();
    }
};

struct TryNode : StmtNode {
    std::unique_ptr<StmtNode> try_;
    std::string name;
    std::unique_ptr<StmtNode> catch_;
    TryNode(ScriptEngine* engine,
            std::unique_ptr<StmtNode> try_,
            std::string name,
            std::unique_ptr<StmtNode> catch_)
            : StmtNode(engine), try_(std::move(try_)), name(std::move(name)), catch_(std::move(catch_)) {}
    void do_exec() const override {
        {
            ScriptScope scope(engine);
            try_->exec();
        }
        if (engine->jumpTarget == JumpTarget::THROW) {
            engine->jumpTarget = JumpTarget::NONE;
            ScriptScope scope(engine);
            engine->local()[name] = engine->target;
            catch_->exec();
        }
    }
};

enum class TokenType {
    PUNCTUATION, IDENTIFIER, KEYWORD, PAREN, LINEBREAK,
    LITERAL_BOOL, LITERAL_INT, LITERAL_REAL,

    EOT // end of token
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
    [[nodiscard]] std::string punctuation() const {
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
    static Token punctuation(std::string p) {
        return {TokenType::PUNCTUATION, p};
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
    static Token parenLeft() {
        return {TokenType::PAREN, 0};
    }
    static Token parenRight() {
        return {TokenType::PAREN, 1};
    }
    static Token linebreak() {
        return {TokenType::LINEBREAK, 0};
    }
    static Token eot() {
        return {TokenType::EOT, 0};
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
    ScriptEngine* engine;
    std::function<R(Args...)> function;
    ExternalFunctionInvocationNode(ScriptEngine* engine, std::function<R(Args...)> function)
            : ExprNode(0), engine(engine), function(function) {}
    Operand do_eval() const override {
        return eval(std::index_sequence_for<Args...>());
    }

    template<size_t... I>
    Operand eval(std::index_sequence<I...>) const;
};

template<typename R, typename... Args>
std::function<FuncPtr(ScriptEngine*)> external(std::function<R(Args...)> function) {
    static_assert(!std::is_reference_v<R>);
    static_assert((!sizeof...(Args) || ... || !std::is_reference_v<Args>));
    return [function](ScriptEngine* engine) {
        return std::make_shared<Function>(Function{parseType<R>(), externalParameters<Args...>(std::index_sequence_for<Args...>()),
                std::make_unique<ReturnNode>(engine, 0, std::make_unique<ExternalFunctionInvocationNode<R, Args...>>(engine, function))});
    };
}

template<typename R, typename... Args>
template<size_t... I>
Operand ExternalFunctionInvocationNode<R, Args...>::eval(std::index_sequence<I...>) const {
    if constexpr(std::is_void_v<R>) {
        function(engine->findOperand(externalParameterName<I>(), 0).val().template as<Args>()...);
        return Object{std::monostate()};
    } else {
        return Object{function(engine->findOperand(externalParameterName<I>(), 0).val().template as<Args>()...)};
    }
}

}