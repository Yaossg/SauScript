#include "SauScript.hpp"

#include <cmath>
#include <numeric>
#include <numbers>
#include <ctime>
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

template<typename Fn>
auto simpleBinary(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        return std::visit([fn](auto lhs, auto rhs) { return Object{fn(*lhs, *rhs)}; },
                          lhs->eval().val().asNumber(lhs->line), rhs->eval().val().asNumber(rhs->line));
    };
}

template<typename Fn>
auto intBinary(char const* name, Fn fn, Assertion* an = noop_assert) {
    return [name, fn, an](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval().val().asIntOp(name, lhs->line);
        auto b = rhs->eval().val().asIntOp(name, rhs->line);
        an(b, rhs->line);
        return Object{fn(a, b)};
    };
}

template<typename Fn>
auto simpleAssignment(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval();
        std::visit(fn, a.ref(lhs->line)->asNumber(lhs->line), rhs->eval().val().asNumber(rhs->line));
        return a;
    };
}

template<typename Fn>
auto intAssignment(char const* name, Fn fn, Assertion* an = noop_assert) {
    return [name, fn, an](ExprNode* lhs, ExprNode* rhs) {
        auto a = lhs->eval();
        int_t& r = a.ref(lhs->line)->asIntOp(name, lhs->line);
        int_t b = rhs->eval().val().asIntOp(name, lhs->line);
        an(b, rhs->line);
        fn(r, b);
        return a;
    };
}

const std::vector<Operator> OPERATORS[13] = {
        {
                {"=",  simpleAssignment([](auto* lhs, auto* rhs) { *lhs = *rhs; })},
                {"+=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs += *rhs; })},
                {"-=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs -= *rhs; })},
                {"*=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs *= *rhs; })},
                {"/=", [](ExprNode* lhs, ExprNode* rhs) {
                    auto a = lhs->eval();
                    std::visit(overloaded {
                            [line = rhs->line](int_t* lhs, auto* rhs) { division_assert(*rhs, line); *lhs /= *rhs; },
                            [](auto* lhs, auto* rhs) { *lhs /= *rhs; }
                    }, a.ref(lhs->line)->asNumber(lhs->line), rhs->eval().val().asNumber(rhs->line));
                    return a;
                }},
                {"%=", intAssignment("%=", [](int_t& lhs, int_t rhs) { lhs %= rhs; }, division_assert)},
                {"<<=",intAssignment("<<=", [](int_t& lhs, int_t rhs) { lhs <<= rhs; }, shift_assert)},
                {">>=",intAssignment(">>=", [](int_t& lhs, int_t rhs) { lhs >>= rhs; }, shift_assert)},
                {"&=", intAssignment("&=", [](int_t& lhs, int_t rhs) { lhs &= rhs; })},
                {"^=", intAssignment("^=", [](int_t& lhs, int_t rhs) { lhs ^= rhs; })},
                {"|=", intAssignment("|=", [](int_t& lhs, int_t rhs) { lhs |= rhs; })}
        },
        {{"||", [](ExprNode *lhs, ExprNode *rhs) {
            return Object{lhs->eval().val().asBool(lhs->line) || rhs->eval().val().asBool(rhs->line)};
        }}},
        {{"&&", [](ExprNode *lhs, ExprNode *rhs) {
            return Object{lhs->eval().val().asBool(lhs->line) && rhs->eval().val().asBool(rhs->line)};
        }}},
        {{"|", intBinary("|", std::bit_or<int_t>{})}},
        {{"^", intBinary("^", std::bit_xor<int_t>{})}},
        {{"&", intBinary("&", std::bit_and<int_t>{})}},
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
                {"<<", intBinary("<<", [](int_t a, int_t b) { return a << b; }, shift_assert)},
                {">>", intBinary(">>", [](int_t a, int_t b) { return a >> b; }, shift_assert)}
        },
        {
                {"+", simpleBinary(std::plus<>{})},
                {"-", simpleBinary(std::minus<>{})}
        },
        {
                {"*", simpleBinary(std::multiplies<>{})},
                {"/", [](ExprNode* lhs, ExprNode* rhs) {
                    return Object{std::visit(overloaded {
                            [line = rhs->line](int_t* lhs, int_t* rhs) { division_assert(*rhs, line); return Object{*lhs / *rhs}; },
                            [](auto* lhs, auto* rhs) { return Object{*lhs / *rhs}; }
                    }, lhs->eval().val().asNumber(lhs->line), rhs->eval().val().asNumber(rhs->line))};
                }},
                {"%", intBinary("%", std::modulus<int_t>{}, division_assert)}
        },
        {
                {"++", [](ExprNode* op) {
                    auto a = op->eval();
                    ++a.ref(op->line)->asIntOp("++", op->line);
                    return a;
                }},
                {"--", [](ExprNode *op) {
                    auto a = op->eval();
                    --a.ref(op->line)->asIntOp("--", op->line);
                    return a;
                }},
                {"+", [](ExprNode *op) { return op->eval().val(); }},
                {"-", [](ExprNode *op) {
                    return std::visit([](auto* a) { return Object{-*a}; }, op->eval().val().asNumber(op->line));
                }},
                {"!", [](ExprNode *op) { return Object{!op->eval().val().asBool(op->line)}; }},
                {"~", [](ExprNode *op) { return Object{~op->eval().val().asIntOp("~", op->line)}; }}
        },
        {
                {"++", [](ExprNode *op) {
                    return Object{op->eval().ref(op->line)->asIntOp("++", op->line)++};
                }},
                {"--", [](ExprNode *op) {
                    return Object{op->eval().ref(op->line)->asIntOp("--", op->line)--};
                }}
        }
};

void installEnvironment(ScriptEngine* engine) {
    engine->installExternalFunction("int",      [](real_t x) { return int_t(x); });
    engine->installExternalFunction("real",     [](int_t x) { return real_t(x); });
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
    engine->installExternalFunction("micros",    [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000; });
    engine->installExternalFunction("millis",    [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000'000; });

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

    ::srand(1);
    engine->installExternalFunction("srand",    ::srand);
    engine->installExternalFunction("rand",     ::rand);
    engine->global()["RAND_MAX"]        = {RAND_MAX};

    engine->installExternalFunction("counter",  [i = int_t()]() mutable { return i++; });

    engine->installExternalFunction("void",     []{});

    engine->global()["__cplusplus"]     = {__cplusplus};
}

}

