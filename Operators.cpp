#include "Node.hpp"

namespace SauScript::Operators {


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


}