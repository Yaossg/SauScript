#include "Node.hpp"

namespace SauScript::Operators {

using Assertion = void(int_t);

void noop_assert(int_t) {}

void division_assert(int_t b) {
    if (b == 0) runtime("divided by zero");
}

void shift_assert(int_t b) {
    if (b < 0) runtime("negative shift count");
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
        fn(engine);
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
                                a.val().asNumber(), b.val().asNumber()));
    };
}

template<typename Fn>
auto intBinary(Fn fn, Assertion* an = noop_assert) {
    return [fn, an](ExprNode* lhs, ExprNode* rhs) {
        auto* engine = lhs->engine;
        lhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto a = lhs->engine->pop().val().asInt();
        rhs->push();
        if (engine->jumpTarget != JumpTarget::NONE) return;
        auto b = rhs->engine->pop().val().asInt();
        an(b);
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
        fn(a.ref(), b.val());
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
        std::visit(fn, a.ref()->asNumber(), b.val().asNumber());
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
        int_t b = engine->pop().val().asInt();
        an(b);
        fn(a.ref()->asInt(), b);
        engine->push(a);
    };
}

const std::vector<Operator> OPERATORS[14] {
        {
                {"throw", unary([] (ScriptEngine* engine) {
                    engine->jump(JumpTarget::THROW, engine->pop().val());
                })},
                {"break", unary([] (ScriptEngine* engine) {
                    engine->jump(JumpTarget::BREAK, engine->pop().val());
                })},
                {"continue", unary([] (ScriptEngine* engine) {
                    engine->jump(JumpTarget::CONTINUE, engine->pop().val());
                })},
                {"return", unary([] (ScriptEngine* engine) {
                    engine->jump(JumpTarget::RETURN, engine->pop().val());
                })},
                {":=", [](ExprNode* lhs, ExprNode* rhs) {
                    auto* engine = rhs->engine;
                    rhs->push();
                    if (engine->jumpTarget != JumpTarget::NONE) return;
                    if (auto* ref = dynamic_cast<RefNode*>(lhs))
                        ref->initialize();
                    else
                        runtime("initialization must be directly applied to id-expression", lhs->location);
                }},
                {"=",  assignment([](Object* lhs, Object const& rhs) {
                    *lhs = rhs.cast(lhs->type());
                })},
                {"+=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs += *rhs; })},
                {"-=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs -= *rhs; })},
                {"*=", simpleAssignment([](auto* lhs, auto* rhs) { *lhs *= *rhs; })},
                {"/=", assignment([](Object* lhs, Object rhs) {
                    std::visit(overloaded {
                            [](int_t* lhs, auto* rhs) { division_assert(*rhs); *lhs /= *rhs; },
                            [](auto* lhs, auto* rhs) { *lhs /= *rhs; }
                    }, lhs->asNumber(), rhs.asNumber());
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
            if (engine->top().val().asBool()) return;
            engine->pop();
            rhs->push();
        }}},
        {{"&&", [](ExprNode *lhs, ExprNode *rhs) {
            auto* engine = lhs->engine;
            lhs->push();
            if (engine->jumpTarget != JumpTarget::NONE) return;
            if (!engine->top().val().asBool()) return;
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
                            [](int_t* lhs, int_t* rhs) { division_assert(*rhs); return Object{*lhs / *rhs}; },
                            [](auto* lhs, auto* rhs) { return Object{*lhs / *rhs}; }
                    }, a.val().asNumber(), b.val().asNumber()));
                }},
                {"%", intBinary(std::modulus<int_t>{}, division_assert)}
        },
        {
                {"++", unary([](ScriptEngine* engine) {
                    auto a = engine->pop();
                    ++a.ref()->asInt();
                    engine->push(a);
                })},
                {"--", unary([](ScriptEngine* engine) {
                    auto a = engine->pop();
                    --a.ref()->asInt();
                    engine->push(a);
                })},
                {"+", unary([](ScriptEngine*){})},
                {"-", unary([](ScriptEngine* engine) {
                    engine->push(std::visit([](auto* a) { return Object{-*a}; }, engine->pop().val().asNumber()));
                })},
                {"!", unary([](ScriptEngine* engine) {
                    engine->push(Object{!engine->pop().val().asBool()});
                })},
                {"~", unary([](ScriptEngine* engine) {
                    engine->push(Object{~engine->top().val().asInt()});
                })}
        },
        {
                {"++", unary([](ScriptEngine* engine) {
                    engine->push(Object{engine->pop().ref()->asInt()++});
                })},
                {"--", unary([](ScriptEngine* engine) {
                    engine->push(Object{engine->pop().ref()->asInt()--});
                })}
        }
};


}