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

struct Number;
struct Token;
struct Operand;
struct Operator;
struct ScriptEngine;
struct ExprNode;
struct StmtNode;
struct ScriptBreak { int line; };
struct ScriptContinue { int line; };

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

inline std::string toLineString(int line) {
    return line > 0 ? std::to_string(line) : "unknown";
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

using int_t = std::int64_t;
using real_t = double;

struct Number {
    std::variant<int_t, real_t> object;

    Number() = default;
    Number(std::variant<int_t, real_t> object): object(object) {}

    [[nodiscard]] bool asBool(int line) {
        if (std::holds_alternative<int_t>(object))
            return std::get<int_t>(object) != 0;
        throw RuntimeError("expected int as bool at line " + toLineString(line));
    }

    int_t& asIntOp(char const* op, int line) {
        if (std::holds_alternative<int_t>(object))
            return std::get<int_t>(object);
        std::string
            msg  = "expected int operand for ";
            msg += op;
            msg += " at line ";
            msg += toLineString(line);
        throw RuntimeError(msg);
    }

    template<typename Fn>
    auto visit(Fn&& fn) {
        return std::visit(std::forward<Fn>(fn), object);
    }
};

struct ScriptException : std::exception {
    Number thrown;
    std::string msg;

    ScriptException(Number thrown, int line): thrown(thrown),
                                              msg("Unhandled script exception thrown at line " + toLineString(line)) {}

    [[nodiscard]] char const* what() const noexcept override {
        return msg.c_str();
    }
};

struct Operand {
    std::variant<Number, Number*> val_or_ref;
    Operand(Operand const&) = default;
    Operand(Number val): val_or_ref(val) {}
    Operand(Number* ref): val_or_ref(ref) {}

    Number val() const {
        static const struct {
            Number operator()(Number i) const { return i; }
            Number operator()(Number* i) const { return *i; }
        } ValueVisitor;
        return std::visit(ValueVisitor, val_or_ref);
    }
    Number* ref(int line) const {
        if (std::holds_alternative<Number*>(val_or_ref))
            return std::get<Number*>(val_or_ref);
        throw RuntimeError("rvalue cannot be used as lvalue at line " + toLineString(line));
    }
};

namespace Keyword {
const std::string_view KW_TOKENS[] = {"let", "del", "while", "do", "end", "if", "else", "elif", "then", "try", "catch",
                                      "until", "repeat", "break", "continue", "for", "throw", "input", "print"};
enum {
    LET, DEL, WHILE, DO, END, IF, ELSE, ELIF, THEN, TRY, CATCH, UNTIL, REPEAT, BREAK, CONTINUE, FOR, THROW, INPUT, PRINT,

    NAK // not a keyword
};

inline int parse(std::string const& kw) {
    using namespace Keyword;
    auto first = std::begin(KW_TOKENS), last = std::end(KW_TOKENS);
    return std::find(first, last, kw) - first;
}
}

struct ScriptEngine {
    std::vector<std::map<std::string, Number>> scopes{{{"__cplusplus", {__cplusplus}}}};
    FILE *out, *in;
    ScriptEngine(FILE* out = stdout, FILE* in = stdin): out(out), in(in) {}

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
    [[nodiscard]] std::unique_ptr<StmtNode> compileStatements(Token*& current);

    [[nodiscard]] std::unique_ptr<StmtNode> compile(char const* script);
    void exec(char const* script, FILE* err = stderr);
};

struct ExprNode {
    int line;
    ExprNode(int line): line(line) {}
    virtual Operand eval() const = 0;
    virtual ~ExprNode() = default;
};

struct ObjNode : virtual ExprNode {};

struct ValNode : ObjNode {
    Number val;
    ValNode(int line, Number val): ExprNode(line), val(val) {}
    Operand eval() const override {
        return val;
    }
};

struct RefNode : ObjNode {
    ScriptEngine* engine;
    std::string name;
    RefNode(ScriptEngine* engine, int line, std::string name): engine(engine), ExprNode(line), name(std::move(name)) {}
    Operand eval() const override {
        if (auto operand = engine->findOperand(name))
            return operand.value();
        throw RuntimeError("reference to undefined variable '" + name + "' at line " + toLineString(line));
    }
};

struct OpNode : virtual ExprNode {};

struct OpUnaryNode : OpNode {
    std::unique_ptr<ExprNode> operand;
    using Fn = std::function<Operand(ExprNode*)>;
    Fn fn;
    OpUnaryNode(int line,
                std::unique_ptr<ExprNode> lhs,
                Fn fn): ExprNode(line),
                        operand(std::move(lhs)), fn(std::move(fn)) {}

    Operand eval() const override {
        return fn(operand.get());
    }
};

struct OpBinaryNode : OpNode {
    std::unique_ptr<ExprNode> lhs, rhs;
    using Fn = std::function<Operand(ExprNode*, ExprNode*)>;
    Fn fn;
    OpBinaryNode(int line,
                 std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs,
                 Fn fn): ExprNode(line),
                 lhs(std::move(lhs)), rhs(std::move(rhs)), fn(std::move(fn)) {}

    Operand eval() const override {
        return fn(lhs.get(), rhs.get());
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
        if (engine->scopes.back().contains(name))
            throw RuntimeError(name + " already exists in the current scope at line " + toLineString(line));
        Number val = initializer->eval().val();
        engine->scopes.back()[name] = val;
    }
};

struct DelNode : StmtNode {
    ScriptEngine* engine;
    int line;
    std::string name;
    DelNode(ScriptEngine* engine, int line, std::string name)
            : engine(engine), line(line), name(std::move(name)) {}
    void exec() const override {
        if (!engine->scopes.back().contains(name))
            throw RuntimeError(name + " is unavailable in the current scope at line " + toLineString(line));
        engine->scopes.back().erase(name);
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
        print->eval().val().visit(overloaded {
            [this](int_t a) { fprintf(engine->out, "%lld\n", a); },
            [this](real_t a) { fprintf(engine->out, "%lg\n", a); }
        });
    }
};

struct InputNode : StmtNode {
    ScriptEngine* engine;
    int line;
    std::string name;
    InputNode(ScriptEngine* engine, int line, std::string name)
            : engine(engine), line(line), name(std::move(name)) {}
    void exec() const override {
        if (!engine->scopes.back().contains(name))
            throw RuntimeError(name + " is unavailable in the current scope at line " + toLineString(line));
        engine->scopes.back()[name].visit(overloaded {
            [this](int_t& a) { fscanf(engine->in, "%lld", &a); },
            [this](real_t& a) { fscanf(engine->in, "%lf", &a); }
        });
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
        engine->scopes.emplace_back();
        try {
            while (cond->eval().val().asBool(cond->line))
                try { loop->exec(); } catch (ScriptContinue) {}
        } catch (ScriptBreak) {}
        engine->scopes.pop_back();
    }
};

struct RepeatNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<StmtNode> loop;
    std::unique_ptr<ExprNode> cond;
    RepeatNode(ScriptEngine* engine,
               std::unique_ptr<StmtNode> loop,
               std::unique_ptr<ExprNode> cond = std::make_unique<ValNode>(0, Number{0}))
            : engine(engine), loop(std::move(loop)), cond(std::move(cond)) {}
    void exec() const override {
        engine->scopes.emplace_back();
        try {
            do try { loop->exec(); } catch (ScriptContinue) {}
            while (!cond->eval().val().asBool(cond->line));
        } catch (ScriptBreak) {}
        engine->scopes.pop_back();
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
        engine->scopes.emplace_back();
        try {
            for(init->exec(); cond->eval().val().asBool(cond->line); iter->eval())
                try { loop->exec(); } catch (ScriptContinue) {}
        } catch (ScriptBreak) {}
        engine->scopes.pop_back();
    }
};

struct DoNode : StmtNode {
    ScriptEngine* engine;
    std::unique_ptr<StmtNode> stmt;
    DoNode(ScriptEngine* engine,
           std::unique_ptr<StmtNode> stmt)
            : engine(engine), stmt(std::move(stmt)) {}
    void exec() const override {
        engine->scopes.emplace_back();
        stmt->exec();
        engine->scopes.pop_back();
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
        engine->scopes.emplace_back();
        if (cond->eval().val().asBool(cond->line)) then->exec(); else else_->exec();
        engine->scopes.pop_back();
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
        engine->scopes.emplace_back();
        try_->exec();
        engine->scopes.pop_back();
    } catch (ScriptException& e) {
        engine->scopes.push_back({{name, e.thrown}});
        catch_->exec();
        engine->scopes.pop_back();
    }
};

enum Associativity: bool {
    LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

enum class OperandType: int {
    INFIX, PREFIX, POSTFIX,
};

struct Operator;

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
    using Unary = OpUnaryNode::Fn;
    using Binary = OpBinaryNode::Fn;
    using Fn = std::variant<Unary, Binary>;

    std::string_view literal;
    Fn fn;

    Unary asUnary() const {
        return std::get<Unary>(fn);
    }

    Binary asBinary() const {
        return std::get<Binary>(fn);
    }
};

extern const std::vector<Operator> OPERATORS[13];

inline Operator const* findOperator(std::string const& literal, int level) {
    auto first = OPERATORS[level].begin(), last = OPERATORS[level].end();
    auto result = std::find_if(first, last, [&literal](Operator const& op) { return op.literal == literal; });
    return result != last ? &*result : nullptr;
}

static std::vector<std::string_view> OP_TOKENS;

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
        return toLineString(line);
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

using Assertion = void(int_t, int);

inline void noop_assert(int_t, int) {}

inline void division_assert(int_t b, int line) {
    if (b == 0) throw RuntimeError("divided by zero at line " + toLineString(line));
}

inline void shift_assert(int_t b, int line) {
    if (b < 0) throw RuntimeError("negative shift count at line " + toLineString(line));
}

}