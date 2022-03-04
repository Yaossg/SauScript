#include "Node.hpp"

namespace SauScript {

std::string Function::descriptor() const {
    std::string ret = "fn(";
    bool first = true;
    for (auto&& parameter : parameters) {
        if (first) { first = false; } else { ret += ", "; }
        ret += parameter.toString();
    }
    ret += "): ";
    ret += nameOf(returnType);
    return ret;
}

std::string Function::toString() const {
    std::string ret = "(";
    ret += descriptor();
    ret += "=";
    ret += expr->toString();
    ret += ")";
    return ret;
}

void Function::invoke(ScriptEngine *engine, const std::vector<Object> &arguments) const {
    if (parameters.size() != arguments.size())
        runtime("expected " + std::to_string(parameters.size()) + " argument(s) but got " + std::to_string(arguments.size()));
    ScriptScope scope(engine);
    for (size_t i = 0; i < arguments.size(); ++i) {
        auto&& parameter = parameters[i];
        auto&& argument = arguments[i];
        engine->local()[parameter.name] = argument.cast(parameter.type);
    }
    expr->push();
    switch (engine->jumpTarget) {
        default:
        case JumpTarget::NONE:
            engine->yield = engine->pop().val();
            if (auto* stmts = dynamic_cast<StmtsNode*>(expr.get()); stmts && !stmts->stmts.empty()) {
                engine->jumpFrom = stmts->stmts.back()->location;
            } else {
                engine->jumpFrom = expr->location;
            }
        case JumpTarget::RETURN:
            try {
                engine->jumpTarget = JumpTarget::NONE;
                engine->push(engine->yield.cast(returnType));
            } catch (RawError& re) {
                re.rethrow(engine->jumpFrom);
            }
        case JumpTarget::THROW:
            return;
        case JumpTarget::BREAK:
            runtime("Wild break jump", engine->jumpFrom);
        case JumpTarget::CONTINUE:
            runtime("Wild continue jump", engine->jumpFrom);
    }
}

Object Function::invokeExternally(ScriptEngine *engine, const std::vector<Object> &arguments) const {
    invoke(engine, arguments);
    if (engine->jumpTarget != JumpTarget::NONE) runtime("internal throw in external callback is forbidden");
    return engine->pop().val();
}

void List::invoke(ScriptEngine *engine, const std::vector<Object> &arguments) const {
    std::vector<func_t> candidates;
    size_t least_promoted = arguments.size();
    for (auto&& obj : elements) {
        if (obj.type() != Type::FUNC) goto mismatch;
        if (func_t candidate = get<func_t>(obj.object); candidate->parameters.size() == arguments.size()) {
            size_t promoted = 0;
            for (size_t i = 0; i < arguments.size(); ++i) {
                auto&& parameter = candidate->parameters[i];
                auto&& argument = arguments[i];
                if (parameter.type == Type::ANY || argument.type() == Type::INT && parameter.type == Type::REAL) {
                    ++promoted;
                } else if (parameter.type != argument.type()) {
                    goto mismatch;
                }
            }
            if (promoted < least_promoted) {
                least_promoted = promoted;
                candidates.clear();
                candidates.push_back(candidate);
            } else if (promoted == least_promoted) {
                candidates.push_back(candidate);
            }
        }
        mismatch: ;
    }
    if (candidates.empty())
        runtime("no function is matched in the set of overloads");
    if (candidates.size() > 1)
        runtime("multiple functions are matched in the set of overloads");
    candidates.front()->invoke(engine, arguments);
}

void Object::invoke(ScriptEngine* engine, std::vector<Object> const& arguments) const {
    std::visit(overloaded {
        [] (auto x) { runtime("not invocable"); },
        [&] (func_t const& func) { func->invoke(engine, arguments); },
        [&] (list_t const& func) { func->invoke(engine, arguments); }
    }, object);
}

std::string Object::toString() const {
    return std::visit(overloaded {
            [](std::monostate) { return std::string("<void>"); },
            [](int_t x) { return std::to_string(x); },
            [](real_t x) {
                size_t len = std::snprintf(nullptr, 0, "%g", x);
                std::string ret(len, '\0');
                std::sprintf(ret.data(), "%g", x);
                if (ret == std::to_string((int_t)x)) ret += ".0";
                return ret;
            },
            [](func_t const& ptr) { return ptr->toString(); },
            [](list_t const& ptr) { return ptr->toString(); }
    }, object);
}

std::string List::toString() const {
    ToStringLockGuard guard(this);
    bool first = true;
    std::string ret = "[";
    for (auto&& obj : elements) {
        if (first) { first = false; } else { ret += ", "; }
        if (obj.type() == Type::LIST && get<list_t>(obj.object)->toStringLock)
            runtime("recursive list cannot be serialized");
        ret += obj.toString();
    }
    return ret + "]";
}

}