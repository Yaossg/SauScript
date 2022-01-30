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
struct ScriptBreak final { int line = 0; };
struct ScriptContinue final { int line = 0; };

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

static std::string_view TYPE_NAMES[3] = {"int", "real", "func"};

enum class Type {
    INT, REAL, FUNC
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
    }
    throw SyntaxError("unsupported type currently");
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
    std::variant<int_t, real_t, FuncPtr> object;

    Object() = default;
    Object(std::variant<int_t, real_t, FuncPtr> object): object(std::move(object)) {}

    [[nodiscard]] bool asBool(int line) {
        if (std::holds_alternative<int_t>(object))
            return std::get<int_t>(object) != 0;
        throw RuntimeError("expected int as bool" + at(line));
    }

    int_t& asIntOp(char const* op, int line) {
        if (std::holds_alternative<int_t>(object))
            return std::get<int_t>(object);
        std::string
            msg  = "expected int operand for ";
            msg += op;
            msg += at(line);
        throw RuntimeError(msg);
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
            default: throw RuntimeError("expected number but got a function" + at(line));
        }
    }

    template<typename Fn>
    auto visit(Fn&& fn) {
        return std::visit(std::forward<Fn>(fn), object);
    }

    [[nodiscard]] std::string toString() const {
        return std::visit(overloaded {
            [](auto x) { return std::to_string(x); },
            [](FuncPtr const& ptr) {
                std::string ret = "function(";
                bool first = true;
                for (auto&& [type, name] : ptr->parameters) {
                    if (first) { first = false; } else { ret += ", "; }
                    ret += name;
                    ret += ": ";
                    ret += TYPE_NAMES[(size_t)type];
                }
                ret += "): ";
                ret += TYPE_NAMES[(size_t)ptr->returnType];
                return ret;
            }
        }, object);
    }

    template<typename T>
    T as() {
        return std::get<(size_t)parseType<T>()>(object);
    }
};

struct ScriptReturn final {
    Object returned;
    int line = 0;
};

struct ScriptException final : std::exception {
    Object thrown;
    std::string msg;

    ScriptException(Object const& thrown, int line): thrown(thrown),
        msg   ("Unhandled script exception '") {
        msg += thrown.toString();
        msg += "' thrown";
        msg += at(line);
    }

    [[nodiscard]] char const* what() const noexcept override {
        return msg.c_str();
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

    [[nodiscard]] Unary asUnary() const { return std::get<Unary>(fn); }
    [[nodiscard]] Binary asBinary() const { return std::get<Binary>(fn); }
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
const std::string_view KW_TOKENS[] = {"let", "del", "while", "do", "end", "if", "else", "elif", "then", "try", "catch",
                                      "until", "repeat", "break", "continue", "for", "throw", "input", "print", "return", "function"};
enum {
    LET, DEL, WHILE, DO, END, IF, ELSE, ELIF, THEN, TRY, CATCH, UNTIL, REPEAT, BREAK, CONTINUE, FOR, THROW, INPUT, PRINT, RETURN, FUNCTION,

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

struct ScriptEngine {
    using Scope = std::map<std::string, Object>;
    std::deque<Scope> scopes;
    FILE *out, *in;

    ScriptEngine(FILE* out = stdout, FILE* in = stdin): out(out), in(in), scopes{{}, {}} {
        installEnvironment(this);
    }

    Scope& global() { return scopes.front(); }
    Scope& local() { return scopes.back(); }

    template<typename Fn>
    void installExternalFunction(std::string const& name, Fn fn) {
        global()[name] = Object{external(std::function{fn})(this)};
    }

    std::optional<Operand> findOperand(std::string const& name);

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
    ExprNode(int line): line(line) {}
    virtual Operand eval() const = 0;
    virtual ~ExprNode() = default;
};

struct ObjNode : virtual ExprNode {};

struct ValNode : ObjNode {
    Object val;
    ValNode(int line, Object val): ExprNode(line), val(std::move(val)) {}
    Operand eval() const override {
        return Object{val};
    }
};

struct RefNode : ObjNode {
    ScriptEngine* engine;
    std::string name;
    RefNode(ScriptEngine* engine, int line, std::string name): engine(engine), ExprNode(line), name(std::move(name)) {}
    Operand eval() const override {
        if (auto operand = engine->findOperand(name))
            return operand.value();
        throw RuntimeError("reference to undefined variable '" + name + "'" + at(line));
    }
};

struct OpNode : virtual ExprNode {};

struct OpUnaryNode : OpNode {
    std::unique_ptr<ExprNode> operand;
    Operator const* op;
    OpUnaryNode(int line,
                std::unique_ptr<ExprNode> lhs,
                Operator const* op): ExprNode(line),
                        operand(std::move(lhs)), op(op) {}

    Operand eval() const override {
        return op->asUnary()(operand.get());
    }
};

struct OpBinaryNode : OpNode {
    std::unique_ptr<ExprNode> lhs, rhs;
    Operator const* op;
    OpBinaryNode(int line,
                 std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs,
                 Operator const* op): ExprNode(line),
                 lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

    Operand eval() const override {
        return op->asBinary()(lhs.get(), rhs.get());
    }
};

struct OpTernaryNode : OpNode {
    std::unique_ptr<ExprNode> cond, lhs, rhs;
    OpTernaryNode(int line,
                  std::unique_ptr<ExprNode> cond,
                  std::unique_ptr<ExprNode> lhs,
                  std::unique_ptr<ExprNode> rhs): ExprNode(line),
                  cond(std::move(cond)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    Operand eval() const override {
        return cond->eval().val().asBool(line) ? lhs->eval() : rhs->eval();
    }
};

struct OpInvokeNode : OpNode {
    ScriptEngine* engine;
    std::unique_ptr<ExprNode> func;
    std::vector<std::unique_ptr<ExprNode>> args;
    OpInvokeNode(int line,
                 ScriptEngine* engine,
                 std::unique_ptr<ExprNode> func,
                 std::vector<std::unique_ptr<ExprNode>> args): ExprNode(line),
                 func(std::move(func)), engine(engine), args(std::move(args)) {}

    Operand eval() const override {
        std::vector<Object> objects;
        for (auto&& arg : args) {
            objects.push_back(arg->eval().val());
        }
        return func->eval().val().invoke(line, engine, objects);
    }
};

struct StmtNode {
    virtual void exec() const = 0;
    virtual ~StmtNode() = default;
};

struct StmtNoopNode : StmtNode {
    void exec() const override {}
};

struct StmtSeqNode : StmtNode {
    std::vector<std::unique_ptr<StmtNode>> stmts;
    StmtSeqNode(std::vector<std::unique_ptr<StmtNode>> stmts): stmts(std::move(stmts)) {}
    void exec() const override {
        for (auto&& stmt : stmts) {
            stmt->exec();
        }
    }
};

struct StmtExprNode : StmtNode {
    std::unique_ptr<ExprNode> expr;
    StmtExprNode(std::unique_ptr<ExprNode> expr): expr(std::move(expr)) {}
    void exec() const override {
        expr->eval();
    }
};

struct LetNode : StmtNode {
    ScriptEngine* engine;
    int line;
    std::unique_ptr<ExprNode> initializer;
    std::string name;
    LetNode(ScriptEngine* engine, int line, std::string name, std::unique_ptr<ExprNode> initializer)
            : engine(engine), line(line), name(std::move(name)), initializer(std::move(initializer)) {}
    void exec() const override {
        if (engine->local().contains(name))
            throw RuntimeError(name + " already exists in the local scope" + at(line));
        Object val = initializer->eval().val();
        engine->local()[name] = val;
    }
};

struct DelNode : StmtNode {
    ScriptEngine* engine;
    int line;
    std::string name;
    DelNode(ScriptEngine* engine, int line, std::string name)
            : engine(engine), line(line), name(std::move(name)) {}
    void exec() const override {
        if (!engine->local().contains(name))
            throw RuntimeError(name + " is not found in the local scope" + at(line));
        engine->local().erase(name);
    }
};

struct BreakNode : StmtNode {
    int line;
    BreakNode(int line): line(line) {}
    void exec() const override {
        throw ScriptBreak{line};
    }
};

struct ContinueNode : StmtNode {
    int line;
    ContinueNode(int line): line(line) {}
    void exec() const override {
        throw ScriptContinue{line};
    }
};

struct ThrowNode : StmtNode {
    int line;
    std::unique_ptr<ExprNode> thrown;
    ThrowNode(int line, std::unique_ptr<ExprNode> thrown)
            : line(line), thrown(std::move(thrown)) {}
    void exec() const override {
        throw ScriptException{thrown->eval().val(), line};
    }
};

struct PrintNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<ExprNode> print;
    PrintNode(ScriptEngine* engine, std::unique_ptr<ExprNode> print)
            : engine(engine), print(std::move(print)) {}
    void exec() const override {
        fprintf(engine->out, "%s\n", print->eval().val().toString().c_str());
    }
};

struct InputNode : StmtNode {
    ScriptEngine* engine;
    int line;
    std::string name;
    InputNode(ScriptEngine* engine, int line, std::string name)
            : engine(engine), line(line), name(std::move(name)) {}
    void exec() const override {
        engine->findOperand(name)->ref(line)->visit(overloaded {
            [this](int_t& a) { if (!fscanf(engine->in, "%lld", &a)) throw RuntimeError("illegal input as int"); },
            [this](real_t& a) { if (!fscanf(engine->in, "%lf", &a)) throw RuntimeError("illegal input as real"); },
            [this](FuncPtr& a) { throw RuntimeError("attempt to input a function" + at(line)); }
        });
    }
};

struct ReturnNode : StmtNode {
    std::unique_ptr<ExprNode> returned;
    int line;
    ReturnNode(int line, std::unique_ptr<ExprNode> returned): line(line), returned(std::move(returned)) {}
    void exec() const override {
        throw ScriptReturn{returned->eval().val(), line};
    }
};

struct WhileNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StmtNode> loop;
    WhileNode(ScriptEngine* engine,
              std::unique_ptr<ExprNode> cond,
              std::unique_ptr<StmtNode> loop)
            : engine(engine), cond(std::move(cond)), loop(std::move(loop)) {}
    void exec() const override {
        ScriptScope scope(engine);
        try {
            while (cond->eval().val().asBool(cond->line))
                try { loop->exec(); } catch (ScriptContinue&) {}
        } catch (ScriptBreak&) {}
    }
};

struct RepeatNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<StmtNode> loop;
    std::unique_ptr<ExprNode> cond;
    RepeatNode(ScriptEngine* engine,
               std::unique_ptr<StmtNode> loop,
               std::unique_ptr<ExprNode> cond = std::make_unique<ValNode>(0, Object{0}))
            : engine(engine), loop(std::move(loop)), cond(std::move(cond)) {}
    void exec() const override {
        ScriptScope scope(engine);
        try {
            do try { loop->exec(); } catch (ScriptContinue&) {}
            while (!cond->eval().val().asBool(cond->line));
        } catch (ScriptBreak&) {}
    }
};

struct ForNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<StmtNode> init;
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<ExprNode> iter;
    std::unique_ptr<StmtNode> loop;
    ForNode(ScriptEngine* engine,
            std::unique_ptr<StmtNode> init,
            std::unique_ptr<ExprNode> cond,
            std::unique_ptr<ExprNode> iter,
            std::unique_ptr<StmtNode> loop)
            : engine(engine), init(std::move(init)), cond(std::move(cond)), iter(std::move(iter)), loop(std::move(loop)) {}
    void exec() const override {
        ScriptScope scope(engine);
        try {
            for(init->exec(); cond->eval().val().asBool(cond->line); iter->eval())
                try { loop->exec(); } catch (ScriptContinue&) {}
        } catch (ScriptBreak&) {}
    }
};

struct DoNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<StmtNode> stmt;
    DoNode(ScriptEngine* engine,
           std::unique_ptr<StmtNode> stmt)
            : engine(engine), stmt(std::move(stmt)) {}
    void exec() const override {
        ScriptScope scope(engine);
        stmt->exec();
    }
};

struct IfNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StmtNode> then;
    std::unique_ptr<StmtNode> else_;
    IfNode(ScriptEngine* engine,
           std::unique_ptr<ExprNode> cond,
           std::unique_ptr<StmtNode> then,
           std::unique_ptr<StmtNode> else_ = std::make_unique<StmtNoopNode>())
            : engine(engine), cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) {}
    void exec() const override {
        ScriptScope scope(engine);
        if (cond->eval().val().asBool(cond->line)) then->exec(); else else_->exec();
    }
};

struct TryNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<StmtNode> try_;
    std::string name;
    std::unique_ptr<StmtNode> catch_;
    TryNode(ScriptEngine* engine,
            std::unique_ptr<StmtNode> try_,
            std::string name,
            std::unique_ptr<StmtNode> catch_)
            : engine(engine), try_(std::move(try_)), name(std::move(name)), catch_(std::move(catch_)) {}
    void exec() const override try {
        ScriptScope scope(engine);
        try_->exec();
    } catch (ScriptException& e) {
        ScriptScope scope(engine);
        engine->local()[name] = {e.thrown};
        catch_->exec();
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
    if (name == "int") return Type::INT;
    else if (name == "real") return Type::REAL;
    else if (name == "func") return Type::FUNC;
    throw SyntaxError("invalid type name" + token.at());
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
struct ExternalFunctionNode : ExprNode {
    ScriptEngine* engine;
    std::function<R(Args...)> function;
    ExternalFunctionNode(ScriptEngine* engine, std::function<R(Args...)> function)
            : ExprNode(0), engine(engine), function(function) {}
    Operand eval() const override {
        return eval(std::index_sequence_for<Args...>());
    }

    template<size_t... I>
    Operand eval(std::index_sequence<I...>) const;
};

template<typename R, typename... Args>
std::function<FuncPtr(ScriptEngine*)> external(std::function<R(Args...)> function) {
    static_assert(!std::is_reference_v<R>);
    static_assert((!std::is_reference_v<Args> || ...));
    static_assert(!std::is_void_v<R>);
    return [function](ScriptEngine* engine) {
        return std::make_shared<Function>(Function{parseType<R>(), externalParameters<Args...>(std::index_sequence_for<Args...>()),
                std::make_unique<ReturnNode>(0, std::make_unique<ExternalFunctionNode<R, Args...>>(engine, function))});
    };
}

template<typename R, typename... Args>
template<size_t... I>
Operand ExternalFunctionNode<R, Args...>::eval(std::index_sequence<I...>) const {
    return Object{function(engine->findOperand(externalParameterName<I>())->val().template as<Args>()...)};
}

}