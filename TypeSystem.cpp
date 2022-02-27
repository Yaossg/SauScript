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
    std::string ret = descriptor();
    ret += " = {";
    ret += stmt->toString();
    ret += "}";
    return ret;
}

void Function::invoke(ScriptEngine *engine, int line, const std::vector<Object> &arguments) const {
    if (parameters.size() != arguments.size())
        throw RuntimeError("expected " + std::to_string(parameters.size()) + " argument(s) but got "
                           + std::to_string(arguments.size()) + at(line));
    ScriptScope scope(engine);
    for (int i = 0; i < arguments.size(); ++i) {
        auto&& parameter = parameters[i];
        auto&& argument = arguments[i];
        engine->local()[parameter.name] = argument.cast(parameter.type, line);
    }
    stmt->push();
    switch (engine->jumpTarget) {
        case JumpTarget::RETURN: {
            engine->jumpTarget = JumpTarget::NONE;
            engine->push(engine->yield.cast(returnType, engine->jumpFrom));
        }
        case JumpTarget::NONE:
        default:
        case JumpTarget::THROW:
            return;
        case JumpTarget::BREAK:
            throw RuntimeError("Wild break jump" + at(engine->jumpFrom));
        case JumpTarget::CONTINUE:
            throw RuntimeError("Wild continue jump" + at(engine->jumpFrom));
    }
}

void List::invoke(ScriptEngine *engine, int line, const std::vector<Object> &arguments) const {
    std::vector<func_t> candidates;
    int least_promoted = arguments.size();
    for (auto&& obj : objs) {
        if (obj.type() != Type::FUNC) goto mismatch;
        if (func_t candidate = get<func_t>(obj.object); candidate->parameters.size() == arguments.size()) {
            int promoted = 0;
            for (int i = 0; i < arguments.size(); ++i) {
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
        throw RuntimeError("no function is matched in the set of overloads" + at(line));
    if (candidates.size() > 1)
        throw RuntimeError("multiple functions are matched in the set of overloads" + at(line));
    candidates.front()->invoke(engine, line, arguments);
}

void Object::invoke(ScriptEngine* engine, int line, std::vector<Object> const& arguments) const {
    std::visit(overloaded {
        [line] (auto x) { throw RuntimeError("not invocable" + at(line)); },
        [&] (func_t const& func) { func->invoke(engine, line, arguments); },
        [&] (list_t const& func) { func->invoke(engine, line, arguments); }
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
    Guard guard(this);
    bool first = true;
    std::string ret = "[";
    for (auto&& obj : objs) {
        if (first) { first = false; } else { ret += ", "; }
        if (obj.type() == Type::LIST && get<list_t>(obj.object)->mark)
            throw RuntimeError("[List::toString]: fatal recursive list");
        ret += obj.toString();
    }
    return ret + "]";
}

}