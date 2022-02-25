#include "Node.hpp"

namespace SauScript {

std::string Function::descriptor() const {
    std::string ret = "function(";
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

void Object::invoke(ScriptEngine* engine, int line, std::vector<Object> const& arguments) const {
    func_t fn;
    switch (type()) {
        case Type::FUNC: {
            fn = std::get<func_t>(object);
            if (fn->parameters.size() != arguments.size())
                throw RuntimeError("expected " + std::to_string(fn->parameters.size()) + "argument(s) but got "
                                   + std::to_string(arguments.size()) + at(line));
            for (int i = 0; i < arguments.size(); ++i) {
                auto&& parameter = fn->parameters[i];
                auto&& argument = arguments[i];
                if (!(parameter.type == argument.type() || argument.type() == Type::INT && parameter.type == Type::REAL))
                    throw RuntimeError("mismatched type of the " + std::to_string(i + 1) + "th argument, expected "
                                       + parameter.type_name() + " but got " + argument.type_name() + at(line));
            }
        } break;
        case Type::LIST: {
            std::vector<func_t> candidates;
            int least_promoted = arguments.size();
            for (auto&& obj : *std::get<list_t>(object)) {
                if (obj.type() != Type::FUNC) goto mismatch;
                if (func_t candidate = get<func_t>(obj.object); candidate->parameters.size() == arguments.size()) {
                    int promoted = 0;
                    for (int i = 0; i < arguments.size(); ++i) {
                        auto&& parameter = candidate->parameters[i];
                        auto&& argument = arguments[i];
                        if (argument.type() == Type::INT && parameter.type == Type::REAL) {
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
            fn = candidates.front();
        } break;
        default:
            throw RuntimeError("not invocable" + at(line));
    }
    ScriptScope scope(engine);
    for (int i = 0; i < arguments.size(); ++i) {
        auto&& parameter = fn->parameters[i];
        auto&& argument = arguments[i];
        if (argument.type() == Type::INT && parameter.type == Type::REAL) {
            engine->local()[parameter.name] = argument.promote(line);
        } else if (parameter.type == argument.type()) {
            engine->local()[parameter.name] = argument;
        } else {
            throw RuntimeError("Assertion failed");
        }
    }
    fn->stmt->push();
    switch (engine->jumpTarget) {
        case JumpTarget::RETURN: {
            engine->jumpTarget = JumpTarget::NONE;
            int line = engine->jumpFrom;
            Object returned = engine->target;
            if (returned.type() == Type::INT && fn->returnType == Type::REAL) {
                engine->push(returned.promote(line));
            } else if (returned.type() == fn->returnType) {
                engine->push(returned);
            } else {
                throw RuntimeError("invalid return type" + at(line));
            }
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
            [](list_t const& ptr) {
                bool first = true;
                std::string ret = "[";
                for (auto&& obj : *ptr) {
                    if (first) { first = false; } else { ret += ", "; }
                    ret += obj.toString();
                }
                return ret + "]";
            }
    }, object);
}
}