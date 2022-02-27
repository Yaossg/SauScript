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
auto unary(Fn fn) {
    return [fn](ExprNode* node) {
        auto* engine = node->engine;
        node->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        fn(engine, node->line);
    };
}

template<typename Fn>
auto binary(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        auto* engine = lhs->engine;
        lhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto a = engine->pop();
        rhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto b = engine->pop();
        engine->push(Object{fn(a.val(), b.val())});
    };
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
auto assignment(Fn fn) {
    return [fn](ExprNode* lhs, ExprNode* rhs) {
        auto* engine = lhs->engine;
        lhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto a = engine->pop();
        rhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto b = engine->pop();
        fn(a.ref(lhs->line), b.val(), rhs->line);
        engine->push(a);
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

const std::vector<Operator> OPERATORS[14] = {
        {
                {"throw", unary([] (ScriptEngine* engine, int line) {
                    engine->jump(JumpTarget::THROW, line, engine->pop().val());
                })},
                {"break", unary([] (ScriptEngine* engine, int line) {
                    engine->jump(JumpTarget::BREAK, line, engine->pop().val());
                })},
                {"continue", unary([] (ScriptEngine* engine, int line) {
                    engine->jump(JumpTarget::CONTINUE, line, engine->pop().val());
                })},
                {"return", unary([] (ScriptEngine* engine, int line) {
                    engine->jump(JumpTarget::RETURN, line, engine->pop().val());
                })},
                {":=", [](ExprNode* lhs, ExprNode* rhs) {
                    auto* engine = rhs->engine;
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    if (auto* ref = dynamic_cast<RefNode*>(lhs))
                        ref->initialize();
                    else
                        throw RuntimeError("initialization must be directly applied to id-expression" + at(lhs->line));
                }},
                {"=",  assignment([](Object* lhs, Object rhs, int line) {
                    *lhs = rhs.cast(lhs->type(), line);
                })},
                {"+=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs += *rhs; })},
                {"-=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs -= *rhs; })},
                {"*=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs *= *rhs; })},
                {"/=", assignment([](Object* lhs, Object rhs, int line) {
                    std::visit(overloaded {
                            [line](int_t* lhs, auto* rhs) { division_assert(*rhs, line); *lhs /= *rhs; },
                            [](auto* lhs, auto* rhs) { *lhs /= *rhs; }
                    }, lhs->asNumber(line), rhs.asNumber(line));
                })},
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
                {"==", binary(std::equal_to<>{})},
                {"!=", binary(std::not_equal_to<>{})}
        },
        {
                {"<",  binary(std::less<>{})},
                {">",  binary(std::greater<>{})},
                {"<=", binary(std::less_equal<>{})},
                {">=", binary(std::greater_equal<>{})}
        },
        {{"<=>", binary([](auto lhs, auto rhs) { return lhs == rhs ? 0 : lhs < rhs ? -1 : 1; })}},
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
                {"++", unary([](ScriptEngine* engine, int line) {
                    auto a = engine->pop();
                    ++a.ref(line)->asInt(line);
                    engine->push(a);
                })},
                {"--", unary([](ScriptEngine* engine, int line) {
                    auto a = engine->pop();
                    --a.ref(line)->asInt(line);
                    engine->push(a);
                })},
                {"+", unary([](ScriptEngine*, int){})},
                {"-", unary([](ScriptEngine* engine, int line) {
                    engine->push(std::visit([](auto* a) { return Object{-*a}; }, engine->pop().val().asNumber(line)));
                })},
                {"!", unary([](ScriptEngine* engine, int line) {
                    engine->push(Object{!engine->pop().val().asBool(line)});
                })},
                {"~", unary([](ScriptEngine* engine, int line) {
                    engine->push(Object{~engine->top().val().asInt(line)});
                })}
        },
        {
                {"++", unary([](ScriptEngine* engine, int line) {
                    engine->push(Object{engine->pop().ref(line)->asInt(line)++});
                })},
                {"--", unary([](ScriptEngine* engine, int line) {
                    engine->push(Object{engine->pop().ref(line)->asInt(line)--});
                })}
        }
};


}