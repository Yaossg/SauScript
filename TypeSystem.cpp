#include "Node.hpp"
#include "Unicode.hpp"

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
    ret += expr->dump();
    ret += ")";
    return ret;
}

Object Function::invoke(ScriptEngine *engine, const std::vector<Object> &arguments) const {
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
            return engine->pop().val();
        case JumpTarget::BREAK:
            runtime("Wild break jump", engine->jumpFrom);
        case JumpTarget::CONTINUE:
            runtime("Wild continue jump", engine->jumpFrom);
    }
}

Object List::invoke(ScriptEngine *engine, const std::vector<Object> &arguments) const {
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
    return candidates.front()->invoke(engine, arguments);
}

Object Object::invoke(ScriptEngine* engine, std::vector<Object> const& arguments) const {
    return std::visit(overloaded {
        [] (auto x) -> Object { runtime("not invocable"); },
        [&] (func_t const& func) { return func->invoke(engine, arguments); },
        [&] (list_t const& func) { return func->invoke(engine, arguments); }
    }, object);
}

std::string toString(real_t x) {
    size_t len = std::snprintf(nullptr, 0, "%g", x);
    std::string ret(len, '\0');
    std::sprintf(ret.data(), "%g", x);
    if (ret == std::to_string((int_t)x)) ret += ".0";
    return ret;
}

std::string Object::toString(StringifyScheme scheme) const {
    return std::visit(overloaded {
            [](std::monostate) { return std::string("{}"); },
            [](int_t x) { return std::to_string(x); },
            [](real_t x) { return SauScript::toString(x); },
            [scheme](func_t const& ptr) { return scheme == StringifyScheme::TREE_NODE ? ptr->descriptor() : ptr->toString(); },
            [](list_t const& ptr) { return ptr->toString(StringifyScheme::DUMP); },
            [scheme](str_t const& ptr) { return scheme == StringifyScheme::RUNTIME ? ptr->bytes : Unicode::quote(ptr->bytes); },
            [](dict_t const& ptr) { return ptr->toString(StringifyScheme::DUMP); },
    }, object);
}

int_t Object::hashCode() const {
    return std::visit(overloaded {
            [](std::monostate) { return 0LL; },
            [](int_t x) { return x; },
            [](real_t x) { return *(int_t*)&x; },
            [](func_t const& ptr) { return (int_t)ptr.get(); },
            [](list_t const& ptr) { return ptr->hashCode(); },
            [](str_t const& ptr) { return int_t(std::hash<std::string>{}(ptr->bytes)); },
            [](dict_t const& ptr) { return ptr->hashCode(); }
    }, object);
}

bool Object::operator==(const Object &other) const {
    return std::visit(overloaded {
            [](auto const& a, auto const& b) { return false; },
            [](std::monostate, std::monostate) { return true; },
            [](int_t a, int_t b) { return a == b; },
            [](real_t a, real_t b) { return a == b; },
            [](int_t a, real_t b) { return a == b; },
            [](real_t a, int_t b) { return a == b; },
            [](func_t const& a, func_t const& b) { return a.get() == b.get(); },
            [](list_t const& a, list_t const& b) { return a->elements == b->elements; },
            [](str_t const& a, str_t const& b) { return a->bytes == b->bytes; },
            [](dict_t const& a, dict_t const& b) { return a->elements == b->elements; }
    }, object, other.object);
}

std::partial_ordering Object::operator<=>(const Object &other) const {
    return std::visit(overloaded {
            [](auto const& a, auto const& b) { return std::partial_ordering::unordered; },
            [](std::monostate, std::monostate) { return std::partial_ordering::equivalent; },
            [](int_t a, int_t b) -> std::partial_ordering { return a <=> b; },
            [](real_t a, real_t b) { return a <=> b; },
            [](int_t a, real_t b) { return a <=> b; },
            [](real_t a, int_t b) { return a <=> b; },
            [](list_t const& a, list_t const& b) { return a->elements <=> b->elements; },
            [](str_t const& a, str_t const& b) -> std::partial_ordering { return a->bytes <=> b->bytes; }
    }, object, other.object);
}

list_t Object::asIterable() const {
    if (type() != Type::LIST) runtime("not iterable");
    return std::get<list_t>(object);
}

bool Object::isMutLocked() const {
    return std::visit(overloaded {
            [](auto const&) { return false; },
            [](list_t const& ptr) { return ptr->mutLock; },
            [](dict_t const& ptr) { return ptr->mutLock; }
    }, object);
}

bool Object::isToStringLocked() const {
    return std::visit(overloaded {
        [](auto const&) { return false; },
        [](list_t const& ptr) { return ptr->toStringLock; },
        [](dict_t const& ptr) { return ptr->toStringLock; }
    }, object);
}

Operand Object::member(Object const& index, bool mut) const {
    return std::visit(overloaded {
            [this](auto const&) -> Operand { runtime("member access is unsupported for " + type_name()); },
            [&index, mut](list_t const& ptr) -> Operand {
                auto index_ = index.asInt();
                if (index_ < 0 || index_ >= ptr->elements.size()) runtime("list index access out of bound");
                if (mut) return &ptr->mut()[index_]; else return ptr->elements[index_];
            },
            [&index, mut](dict_t const& ptr) -> Operand {
                if (!ptr->elements.contains(index)) runtime("dict index access out of bound");
                if (mut) return &ptr->mut().at(index); else return ptr->elements.at(index);
            }
    }, object);
}

std::string List::toString(StringifyScheme scheme) const {
    ToStringLockGuard guard(this);
    bool first = true;
    std::string ret = "[";
    for (auto&& obj : elements) {
        if (first) { first = false; } else { ret += ", "; }
        if (obj.isToStringLocked())
            runtime("recursive object cannot be serialized");
        ret += obj.toString(scheme);
    }
    return ret + "]";
}

int_t List::hashCode() const {
    int_t ret = 0;
    for (auto&& element : elements) {
        ret <<= 1;
        ret ^= element.hashCode();
    }
    return ret;
}

std::string Dictionary::toString(StringifyScheme scheme) const {
    ToStringLockGuard guard(this);
    bool first = true;
    std::string ret = "@[";
    for (auto&& [key, value] : elements) {
        if (first) { first = false; } else { ret += ", "; }
        if (key.isToStringLocked() || value.isToStringLocked())
            runtime("recursive object cannot be serialized");
        ret += key.toString(scheme);
        ret += ": ";
        ret += value.toString(scheme);
    }
    return ret + "]";
}

int_t Dictionary::hashCode() const {
    int_t ret = 0;
    for (auto&& [key, value] : elements) {
        ret <<= 1;
        ret ^= key.hashCode();
        ret <<= 1;
        ret ^= value.hashCode();
    }
    return ret;
}
}