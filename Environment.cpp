#include "SauScript.hpp"

#include <cmath>
#include <numeric>
#include <numbers>
#include <chrono>
#include <cstdlib>

namespace SauScript {

using Assertion = void(int_t, int);

void noop_assert(int_t, int) {}

void division_assert(int_t b, int line) {
    if (b == 0) throw RuntimeError("divided by zero" + at(line));
}

void shift_assert(int_t b, int line) {
    if (b < 0) throw RuntimeError("negative shift count" + at(line));
}

int_t ushr(int_t lhs, int_t rhs) {
    return int_t(std::make_unsigned_t<int_t>(lhs) >> rhs);
}

template<typename Fn>
auto simpleBinary(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        auto* engine = lhs->engine;
        lhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto a = engine->pop();
        rhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto b = engine->pop();
        engine->push(std::visit([fn](auto lhs, auto rhs) { return Object{fn(*lhs, *rhs)}; },
                          a.val().asNumber(lhs->line), b.val().asNumber(rhs->line)));
    };
}

template<typename Fn>
auto intBinary(Fn fn, Assertion* an = noop_assert) {
    return [fn, an](ExprNode* lhs, ExprNode* rhs) {
        auto* engine = lhs->engine;
        lhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto a = lhs->engine->pop().val().asInt(lhs->line);
        rhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto b = rhs->engine->pop().val().asInt(rhs->line);
        an(b, rhs->line);
        engine->push(Object{fn(a, b)});
    };
}

template<typename Fn>
auto simpleAssignment(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        auto* engine = lhs->engine;
        lhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto a = engine->pop();
        rhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto b = engine->pop();
        std::visit(fn, a.ref(lhs->line)->asNumber(lhs->line), b.val().asNumber(rhs->line));
        engine->push(a);
    };
}

template<typename Fn>
auto intAssignment(Fn fn, Assertion* an = noop_assert) {
    return [fn, an](ExprNode* lhs, ExprNode* rhs) {
        auto* engine = lhs->engine;
        lhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto a = engine->pop();
        rhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        int_t b = engine->pop().val().asInt(lhs->line);
        an(b, rhs->line);
        fn(a.ref(lhs->line)->asInt(lhs->line), b);
        engine->push(a);
    };
}

const std::vector<Operator> OPERATORS[13] = {
        {
                {"print", [](ExprNode* rhs) {
                    auto* engine = rhs->engine;
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    fprintf(rhs->engine->out, "%s\n", engine->pop().val().toString().c_str());
                    engine->push({});
                }},
                {"throw", [](ExprNode* rhs) {
                    auto* engine = rhs->engine;
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->target = engine->pop().val();
                    engine->jumpTarget = JumpTarget::THROW;
                    engine->jumpFrom = rhs->line;
                }},
                {"yield", [](ExprNode* rhs) {
                    auto* engine = rhs->engine;
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->target = engine->pop().val();
                    engine->jumpTarget = JumpTarget::BREAK;
                    engine->jumpFrom = rhs->line;
                }},
                {"return", [](ExprNode* rhs) {
                    auto* engine = rhs->engine;
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->target = engine->pop().val();
                    engine->jumpTarget = JumpTarget::RETURN;
                    engine->jumpFrom = rhs->line;
                }},
                {":=", [](ExprNode* lhs, ExprNode* rhs) {
                    auto* engine = rhs->engine;
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    if (auto* ref = dynamic_cast<RefNode*>(lhs))
                        ref->initialize();
                    else
                        throw RuntimeError("initialization cannot be applied to an expression" + at(lhs->line));
                }},
                {"=",  simpleAssignment([](auto* lhs, auto* rhs) { *lhs = *rhs; })},
                {"+=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs += *rhs; })},
                {"-=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs -= *rhs; })},
                {"*=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs *= *rhs; })},
                {"/=", [](ExprNode* lhs, ExprNode* rhs) {
                    auto* engine = lhs->engine;
                    lhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    auto a  = engine->pop();
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    auto b = engine->pop();
                    std::visit(overloaded {
                            [line = rhs->line](int_t* lhs, auto* rhs) { division_assert(*rhs, line); *lhs /= *rhs; },
                            [](auto* lhs, auto* rhs) { *lhs /= *rhs; }
                    }, a.ref(lhs->line)->asNumber(lhs->line), b.val().asNumber(rhs->line));
                    engine->push(a);
                }},
                {"%=", intAssignment([](int_t& lhs, int_t rhs) { lhs %= rhs; }, division_assert)},
                {"<<=",intAssignment([](int_t& lhs, int_t rhs) { lhs <<= rhs; }, shift_assert)},
                {">>=",intAssignment([](int_t& lhs, int_t rhs) { lhs >>= rhs; }, shift_assert)},
                {">>>=",intAssignment([](int_t& lhs, int_t rhs) { lhs = ushr(lhs, rhs); }, shift_assert)},
                {"&=", intAssignment([](int_t& lhs, int_t rhs) { lhs &= rhs; })},
                {"^=", intAssignment([](int_t& lhs, int_t rhs) { lhs ^= rhs; })},
                {"|=", intAssignment([](int_t& lhs, int_t rhs) { lhs |= rhs; })}
        },
        {{"||", [](ExprNode *lhs, ExprNode *rhs) {
            auto* engine = lhs->engine;
            lhs->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            if (engine->top().val().asBool(lhs->line)) return;
            engine->pop();
            rhs->push();
        }}},
        {{"&&", [](ExprNode *lhs, ExprNode *rhs) {
            auto* engine = lhs->engine;
            lhs->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            if (!engine->top().val().asBool(lhs->line)) return;
            engine->pop();
            rhs->push();
        }}},
        {{"|", intBinary(std::bit_or<int_t>{})}},
        {{"^", intBinary(std::bit_xor<int_t>{})}},
        {{"&", intBinary(std::bit_and<int_t>{})}},
        {
                {"==", simpleBinary(std::equal_to<>{})},
                {"!=", simpleBinary(std::not_equal_to<>{})}
        },
        {
                {"<",  simpleBinary(std::less<>{})},
                {">",  simpleBinary(std::greater<>{})},
                {"<=", simpleBinary(std::less_equal<>{})},
                {">=", simpleBinary(std::greater_equal<>{})}
        },
        {
                {"<<", intBinary([](int_t lhs, int_t rhs) { return lhs << rhs; }, shift_assert)},
                {">>", intBinary([](int_t lhs, int_t rhs) { return lhs >> rhs; }, shift_assert)},
                {">>>",intBinary(ushr, shift_assert)}
        },
        {
                {"+", simpleBinary(std::plus<>{})},
                {"-", simpleBinary(std::minus<>{})}
        },
        {
                {"*", simpleBinary(std::multiplies<>{})},
                {"/", [](ExprNode* lhs, ExprNode* rhs) {
                    auto* engine = lhs->engine;
                    lhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    auto a = engine->pop();
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    auto b = engine->pop();
                    engine->push(std::visit(overloaded {
                            [line = rhs->line](int_t* lhs, int_t* rhs) { division_assert(*rhs, line); return Object{*lhs / *rhs}; },
                            [](auto* lhs, auto* rhs) { return Object{*lhs / *rhs}; }
                    }, a.val().asNumber(lhs->line), b.val().asNumber(rhs->line)));
                }},
                {"%", intBinary(std::modulus<int_t>{}, division_assert)}
        },
        {
                {"++", [](ExprNode* op) {
                    auto* engine = op->engine;
                    op->push();
                    auto a = engine->pop();
                    ++a.ref(op->line)->asInt(op->line);
                    engine->push(a);
                }},
                {"--", [](ExprNode *op) {
                    auto* engine = op->engine;
                    op->push();
                    auto a = engine->pop();
                    --a.ref(op->line)->asInt(op->line);
                    engine->push(a);
                }},
                {"+", [](ExprNode *op) { op->push(); }},
                {"-", [](ExprNode *op) {
                    auto* engine = op->engine;
                    op->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->push(std::visit([](auto* a) { return Object{-*a}; }, engine->pop().val().asNumber(op->line)));
                }},
                {"!", [](ExprNode *op) {
                    auto* engine = op->engine;
                    op->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->push(Object{!engine->pop().val().asBool(op->line)});
                }},
                {"~", [](ExprNode *op) {
                    auto* engine = op->engine;
                    op->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->push(Object{~engine->top().val().asInt(op->line)});
                }}
        },
        {
                {"++", [](ExprNode *op) {
                    auto* engine = op->engine;
                    op->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->push(Object{engine->pop().ref(op->line)->asInt(op->line)++});
                }},
                {"--", [](ExprNode *op) {
                    auto* engine = op->engine;
                    op->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    engine->push(Object{engine->pop().ref(op->line)->asInt(op->line)--});
                }}
        }
};

void installEnvironment(ScriptEngine* engine) {
    engine->installExternalFunction("int",      [](real_t x) { return int_t(x); });
    engine->installExternalFunction("real",     [](int_t x) { return real_t(x); });

    engine->installExternalFunction("readInt",  []() { int_t x; std::scanf("%lld", &x); return x; });
    engine->installExternalFunction("readReal", []() { real_t x; std::scanf("%lf", &x); return x; });

    engine->installExternalFunction("len",      [](list_t x) { return (int_t)x->size(); });

    engine->installExternalFunction("ceil",     ::ceil);
    engine->installExternalFunction("floor",    ::floor);
    engine->installExternalFunction("trunc",    ::trunc);
    engine->installExternalFunction("round",    ::llround);
    engine->installExternalFunction("abs",      ::llabs);
    engine->installExternalFunction("fabs",     ::fabs);
    engine->installExternalFunction("fmod",     ::fmod);
    engine->installExternalFunction("remainder",::remainder);
    engine->installExternalFunction("fma",      ::fma);
    engine->installExternalFunction("min",      [](int_t a, int_t b) { return a < b ? a : b; });
    engine->installExternalFunction("max",      [](int_t a, int_t b) { return a > b ? a : b; });
    engine->installExternalFunction("fmin",     ::fmin);
    engine->installExternalFunction("fmax",     ::fmax);

    engine->installExternalFunction("exp",      ::exp);
    engine->installExternalFunction("exp2",     ::exp2);
    engine->installExternalFunction("expm1",    ::expm1);
    engine->installExternalFunction("log",      ::log);
    engine->installExternalFunction("log2",     ::log2);
    engine->installExternalFunction("log10",    ::log10);
    engine->installExternalFunction("log1p",    ::log1p);
    engine->installExternalFunction("pow",      ::pow);
    engine->installExternalFunction("sqrt",     ::sqrt);
    engine->installExternalFunction("cbrt",     ::cbrt);
    engine->installExternalFunction("hypot",    ::hypot);
    engine->installExternalFunction("hypot3",   (real_t(*)(real_t, real_t, real_t))std::hypot);

    engine->installExternalFunction("sin",      ::sin);
    engine->installExternalFunction("cos",      ::cos);
    engine->installExternalFunction("tan",      ::tan);
    engine->installExternalFunction("asin",     ::asin);
    engine->installExternalFunction("acos",     ::acos);
    engine->installExternalFunction("atan",     ::atan);
    engine->installExternalFunction("atan2",    ::atan2);
    engine->installExternalFunction("sinh",     ::sinh);
    engine->installExternalFunction("cosh",     ::cosh);
    engine->installExternalFunction("tanh",     ::tanh);
    engine->installExternalFunction("asinh",    ::asinh);
    engine->installExternalFunction("acosh",    ::acosh);
    engine->installExternalFunction("atanh",    ::atanh);

    engine->installExternalFunction("erf",      ::erf);
    engine->installExternalFunction("erfc",     ::erfc);
    engine->installExternalFunction("tgamma",   ::tgamma);
    engine->installExternalFunction("lgamma",   ::lgamma);
    engine->installExternalFunction("beta",     std::beta<real_t, real_t>);

    engine->installExternalFunction("gcd",      std::gcd<int_t, int_t>);
    engine->installExternalFunction("lcm",      std::lcm<int_t, int_t>);
    engine->installExternalFunction("midpoint", (int_t(*)(int_t, int_t))std::midpoint);
    engine->installExternalFunction("fmidpoint",(real_t(*)(real_t, real_t))std::midpoint);
    engine->installExternalFunction("lerp",     (real_t(*)(real_t, real_t, real_t))std::lerp);

    engine->installExternalFunction("nanos",    [] { return std::chrono::system_clock::now().time_since_epoch().count(); });
    engine->installExternalFunction("micros",   [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000; });
    engine->installExternalFunction("millis",   [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000'000; });

    {
        using namespace std::numbers;
        engine->global()["e"]           = {e};
        engine->global()["log2e"]       = {log2e};
        engine->global()["log10e"]      = {log10e};
        engine->global()["pi"]          = {pi};
        engine->global()["inv_pi"]      = {inv_pi};
        engine->global()["inv_sqrtpi"]  = {inv_sqrtpi};
        engine->global()["sqrt2"]       = {sqrt2};
        engine->global()["sqrt3"]       = {sqrt3};
        engine->global()["inv_sqrt3"]   = {inv_sqrt3};
        engine->global()["egamma"]      = {egamma};
        engine->global()["phi"]         = {phi};
    }

    engine->installExternalFunction("counter",  [i = int_t()]() mutable { return i++; });

    engine->global()["__cplusplus"]     = {__cplusplus};
}

}

