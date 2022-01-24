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

namespace SauScript {

struct ScriptException { int thrown, line; };
struct ScriptBreak { int line; };
struct ScriptContinue { int line; };

struct Token;
struct Object;
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

inline bool isNumberContinue(char ch) {
    return std::isalnum(ch) || ch == '_';
}

inline int parseNumber(int line, std::string x) try {
    if (x == "false") return 0;
    if (x == "true") return 1;
    std::erase(x, '_');
    return std::stoi(x, nullptr, 0);
} catch(...) {
    throw SyntaxError("invalid number literal at line " + toLineString(line));
}

inline int assertNonZero(int line, int x) {
    if (x == 0) throw RuntimeError("divided by zero at line " + toLineString(line));
    return x;
}

inline int assertNonNeg(int line, int x) {
    if (x < 0) throw RuntimeError("negative shift count at line " + toLineString(line));
    return x;
}

struct Object {
    std::variant<int, int*> val_or_ref;
    Object(Object const&) = default;
    Object(int val): val_or_ref(val) {}
    Object(int* ref): val_or_ref(ref) {}

    int val() const {
        static const struct {
            int operator()(int i) const { return i; }
            int operator()(int* i) const { return *i; }
        } ValueVisitor;
        return std::visit(ValueVisitor, val_or_ref);
    }
    int& ref() const {
        if (std::holds_alternative<int>(val_or_ref)) throw RuntimeError("rvalue cannot be used as lvalue");
        return *std::get<int*>(val_or_ref);
    }
};

namespace Keyword {
const std::string_view KW_TOKENS[] = {"let", "del", "while", "do", "end", "if", "else", "elif", "then", "try", "catch", "until", "repeat", "break", "continue", "for"};
enum {
    LET, DEL, WHILE, DO, END, IF, ELSE, ELIF, THEN, TRY, CATCH, UNTIL, REPEAT, BREAK, CONTINUE, FOR,

    NAK // not a keyword
};

inline int parse(std::string const& kw) {
    using namespace Keyword;
    auto first = std::begin(KW_TOKENS), last = std::end(KW_TOKENS);
    return std::find(first, last, kw) - first;
}
}

struct ScriptEngine {
    std::vector<std::map<std::string, int>> scopes{{{"__cplusplus", __cplusplus}}};
    FILE *out, *in;
    ScriptEngine(FILE* out = stdout, FILE* in = stdin): out(out), in(in) {}

    std::optional<Object> findObject(std::string const& name);
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
    virtual Object eval() const = 0;
    virtual ~ExprNode() = default;
};

struct ObjNode : virtual ExprNode {};

struct ValNode : ObjNode {
    int val;
    ValNode(int line, int val): ExprNode(line), val(val) {}
    Object eval() const override {
        return val;
    }
};

struct RefNode : ObjNode {
    ScriptEngine* engine;
    std::string name;
    RefNode(ScriptEngine* engine, int line, std::string name): engine(engine), ExprNode(line), name(std::move(name)) {}
    Object eval() const override {
        if (auto object = engine->findObject(name))
            return object.value();
        throw RuntimeError("reference to undefined variable '" + name + "' at line " + toLineString(line));
    }
};

struct OpNode : virtual ExprNode {};

struct OpUnaryNode : OpNode {
    std::unique_ptr<ExprNode> operand;
    using Fn = std::function<Object(ExprNode*)>;
    Fn fn;
    OpUnaryNode(int line,
                std::unique_ptr<ExprNode> lhs,
                Fn fn): ExprNode(line),
                        operand(std::move(lhs)), fn(std::move(fn)) {}

    Object eval() const override {
        return fn(operand.get());
    }
};

struct OpBinaryNode : OpNode {
    std::unique_ptr<ExprNode> lhs, rhs;
    using Fn = std::function<Object(ExprNode*, ExprNode*)>;
    Fn fn;
    OpBinaryNode(int line,
                 std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs,
                 Fn fn): ExprNode(line),
                         lhs(std::move(lhs)), rhs(std::move(rhs)), fn(std::move(fn)) {}

    Object eval() const override {
        return fn(lhs.get(), rhs.get());
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
        if (engine->scopes.back().contains(name)) throw RuntimeError(name + " already exists in the current scope at line " + toLineString(line));
        int val = initializer->eval().val();
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
        if (!engine->scopes.back().contains(name)) throw RuntimeError(name + " is unavailable in the current scope at line " + toLineString(line));
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
            while (cond->eval().val())
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
               std::unique_ptr<ExprNode> cond = std::make_unique<ValNode>(0, 0))
            : engine(engine), loop(std::move(loop)), cond(std::move(cond)) {}
    void exec() const override {
        engine->scopes.emplace_back();
        try {
            do try { loop->exec(); } catch (ScriptContinue) {}
            while (!cond->eval().val());
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
            for(init->exec(); cond->eval().val(); iter->eval())
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
        if (cond->eval().val()) then->exec(); else else_->exec();
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
    } catch (ScriptException e) {
        engine->scopes.push_back({{name, e.thrown}});
        catch_->exec();
        engine->scopes.pop_back();
    }
};

enum Associativity: bool {
    LEFT_TO_RIGHT, RIGHT_TO_LEFT
};

enum OperandType: int {
    INFIX, PREFIX, POSTFIX,
};

struct Operator {
    int precedence;
    Associativity associativity;
    OperandType operandType;
    std::string_view literal;
    using Unary = OpUnaryNode::Fn;
    using EUnary = std::function<Object(ScriptEngine*, ExprNode*)>;
    using Binary = OpBinaryNode::Fn;
    using EBinary = std::function<Object(ScriptEngine*, ExprNode*, ExprNode*)>;
    using Fn = std::variant<Unary, EUnary, Binary, EBinary>;
    Fn fn;
    [[nodiscard]] std::unique_ptr<ExprNode> apply(ScriptEngine* engine, int line, std::stack<std::unique_ptr<ExprNode>>& operands) const {
        auto pop = [line, &operands] {
            if (operands.empty()) throw SyntaxError("operand expected at line " + toLineString(line));
            std::unique_ptr<ExprNode> operand = std::move(operands.top());
            operands.pop();
            return operand;
        };
        if (operandType == INFIX) {
            std::unique_ptr<ExprNode> rhs = pop();
            std::unique_ptr<ExprNode> lhs = pop();
            Binary binary = std::holds_alternative<EBinary>(fn) ? std::bind_front(std::get<EBinary>(fn), engine) : std::get<Binary>(fn);
            return std::make_unique<OpBinaryNode>(line, std::move(lhs), std::move(rhs), binary);
        }
        std::unique_ptr<ExprNode> operand = pop();
        Unary unary = std::holds_alternative<EUnary>(fn) ? std::bind_front(std::get<EUnary>(fn), engine) : std::get<Unary>(fn);
        return std::make_unique<OpUnaryNode>(line, std::move(operand), unary);
    }
};

inline const Operator OPERATORS[] = {
        {15, LEFT_TO_RIGHT, POSTFIX, "++",  [](ExprNode* op) { return op->eval().ref()++; }},
        {15, LEFT_TO_RIGHT, POSTFIX, "--",  [](ExprNode* op) { return op->eval().ref()--; }},
        {14, RIGHT_TO_LEFT, PREFIX, "++",   [](ExprNode* op) { return &++op->eval().ref(); }},
        {14, RIGHT_TO_LEFT, PREFIX, "--",   [](ExprNode* op) { return &--op->eval().ref(); }},
        {14, RIGHT_TO_LEFT, PREFIX, "+",    [](ExprNode* op) { return +op->eval().val(); }},
        {14, RIGHT_TO_LEFT, PREFIX, "-",    [](ExprNode* op) { return -op->eval().val(); }},
        {14, RIGHT_TO_LEFT, PREFIX, "!",    [](ExprNode* op) { return !op->eval().val(); }},
        {14, RIGHT_TO_LEFT, PREFIX, "~",    [](ExprNode* op) { return ~op->eval().val(); }},
        {14, RIGHT_TO_LEFT, PREFIX, "input",[](ScriptEngine* engine, ExprNode* op) { return fscanf(engine->in, "%d", &op->eval().ref()); }},
        {12, LEFT_TO_RIGHT, INFIX, "*",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() * rhs->eval().val(); }},
        {12, LEFT_TO_RIGHT, INFIX, "/",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() / assertNonZero(rhs->line, rhs->eval().val()); }},
        {12, LEFT_TO_RIGHT, INFIX, "%",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() % assertNonZero(rhs->line, rhs->eval().val()); }},
        {11, LEFT_TO_RIGHT, INFIX, "+",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() + rhs->eval().val(); }},
        {11, LEFT_TO_RIGHT, INFIX, "-",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() - rhs->eval().val(); }},
        {10, LEFT_TO_RIGHT, INFIX, "<<",    [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() << assertNonNeg(rhs->line, rhs->eval().val()); }},
        {10, LEFT_TO_RIGHT, INFIX, ">>",    [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() >> assertNonNeg(rhs->line, rhs->eval().val()); }},
        {8, LEFT_TO_RIGHT, INFIX, "<",      [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() < rhs->eval().val(); }},
        {8, LEFT_TO_RIGHT, INFIX, ">",      [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() > rhs->eval().val(); }},
        {8, LEFT_TO_RIGHT, INFIX, "<=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() <= rhs->eval().val(); }},
        {8, LEFT_TO_RIGHT, INFIX, ">=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() >= rhs->eval().val(); }},
        {7, LEFT_TO_RIGHT, INFIX, "==",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() == rhs->eval().val(); }},
        {7, LEFT_TO_RIGHT, INFIX, "!=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() != rhs->eval().val(); }},
        {6, LEFT_TO_RIGHT, INFIX, "&",      [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() & rhs->eval().val(); }},
        {5, LEFT_TO_RIGHT, INFIX, "^",      [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() ^ rhs->eval().val(); }},
        {4, LEFT_TO_RIGHT, INFIX, "|",      [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() | rhs->eval().val(); }},
        {3, LEFT_TO_RIGHT, INFIX, "&&",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() && rhs->eval().val(); }},
        {2, LEFT_TO_RIGHT, INFIX, "||",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().val() || rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, INFIX, "=",      [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() = rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, INFIX, "+=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() += rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, INFIX, "-=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() -= rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, INFIX, "*=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() *= rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, INFIX, "/=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() /= assertNonZero(rhs->line, rhs->eval().val()); }},
        {1, RIGHT_TO_LEFT, INFIX, "%=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() %= assertNonZero(rhs->line, rhs->eval().val()); }},
        {1, RIGHT_TO_LEFT, INFIX, "<<=",    [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() <<= assertNonNeg(rhs->line, rhs->eval().val()); }},
        {1, RIGHT_TO_LEFT, INFIX, ">>=",    [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() >>= assertNonNeg(rhs->line, rhs->eval().val()); }},
        {1, RIGHT_TO_LEFT, INFIX, "&=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() &= rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, INFIX, "^=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() ^= rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, INFIX, "|=",     [](ExprNode* lhs, ExprNode* rhs) { return lhs->eval().ref() |= rhs->eval().val(); }},
        {1, RIGHT_TO_LEFT, PREFIX, "print", [](ScriptEngine* engine, ExprNode* op) { return std::fprintf(engine->out, "%d\n", op->eval().val()) - 1; }},
        {1, RIGHT_TO_LEFT, PREFIX, "throw", [](ExprNode* op)->Object { throw ScriptException{op->eval().val(), op->line}; }},
};

[[nodiscard]] inline int parseOp(std::string const& token, OperandType operandType) {
    auto first = std::begin(OPERATORS), last = std::end(OPERATORS);
    auto result = std::find_if(first, last, [&] (Operator const& op) { return op.literal == token && op.operandType == operandType; });
    return result != last ? result - first : -1;
}

enum class TokenType {
    PUNCTUATION, OPERATOR, IDENTIFIER, LITERAL, KEYWORD, PAREN, LINEBREAK,

    EOT // end of token
};

struct Token {
    TokenType type;
    std::variant<int, std::string> parameter;
    int line = 0;
    bool operator ==(Token const& other) const {
        return type == other.type && parameter == other.parameter;
    }
    Token& at(int line) {
        this->line = line;
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
    [[nodiscard]] int parseOp(OperandType operandType) const {
        return SauScript::parseOp(punctuation(), operandType);
    }
    [[nodiscard]] std::string literal() const {
        return std::get<std::string>(parameter);
    }
    [[nodiscard]] int paren() const {
        return std::get<int>(parameter);
    }
    void asOperator(int index) {
        type = TokenType::OPERATOR;
        parameter = index;
    }
    Operator const& operator()() const {
        return OPERATORS[std::get<int>(parameter)];
    }
    static Token punctuation(std::string p) {
        return {TokenType::PUNCTUATION, p};
    }
    static Token identifier(std::string id) {
        return {TokenType::IDENTIFIER, id};
    }
    static Token literal(std::string id) {
        return {TokenType::LITERAL, id};
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

}