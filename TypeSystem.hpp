#pragma once

#include <vector>
#include <memory>
#include <variant>

#include "Lib.hpp"

namespace SauScript {

static std::string_view TYPE_NAMES[] = {"void", "int", "real", "func", "list", "any"};

enum class Type : size_t {
    VOID, INT, REAL, FUNC, LIST,
    ANY, // as parameter or return type
    NAT  // not a type
};

inline std::string_view nameOf(Type type) {
    return TYPE_NAMES[(size_t) type];
}

struct List {
    std::vector<Object> objs;
    mutable bool mark = false;
    List() = default;
    List(std::vector<Object> objs): objs(std::move(objs)) {}
    List(std::initializer_list<Object> objs): objs(objs) {}
    struct Guard {
        List const* list;
        explicit Guard(List const* list): list(list) { list->mark = true; }
        ~Guard() { list->mark = false; }
    };

    void invoke(ScriptEngine* engine, int line, std::vector<Object> const& arguments) const;

    [[nodiscard]] std::string toString() const;
};

using int_t = long long;
using real_t = double;
using func_t = std::shared_ptr<Function>;
using list_t = std::shared_ptr<List>;

template<typename T>
constexpr Type parseType() {
    if constexpr(std::is_void_v<T> || std::is_same_v<T, std::monostate>) {
        return Type::VOID;
    } else if constexpr(std::is_integral_v<T>) {
        return Type::INT;
    } else if constexpr(std::is_floating_point_v<T>) {
        return Type::REAL;
    } else if constexpr(std::is_same_v<T, func_t>) {
        return Type::FUNC;
    } else if constexpr(std::is_same_v<T, list_t>) {
        return Type::LIST;
    } else if constexpr(std::is_same_v<T, Object>) {
        return Type::ANY;
    } else throw SyntaxError("unsupported external type");
}

struct Parameter {
    Type type;
    std::string name;

    [[nodiscard]] std::string type_name() const {
        return std::string(nameOf(type));
    }

    [[nodiscard]] std::string toString() const {
        return name + ": " + type_name();
    }
};

struct Function {
    Type returnType;
    std::vector<Parameter> parameters;
    std::unique_ptr<ExprNode> stmt;

    [[nodiscard]] std::string descriptor() const;
    [[nodiscard]] std::string toString() const;
    void invoke(ScriptEngine* engine, int line, std::vector<Object> const& arguments) const;
};

template<size_t I>
std::string externalParameterName() {
    return "$" + std::to_string(I);
}

template<typename... Args, size_t... I>
std::vector<Parameter> externalParameters(std::index_sequence<I...>) {
    return {{parseType<Args>(), externalParameterName<I>()}...};
}

struct Object {
    std::variant<std::monostate, int_t, real_t, func_t, list_t> object;

    [[nodiscard]] Type type() const {
        return (Type) object.index();
    }

    [[nodiscard]] std::string type_name() const {
        return std::string(nameOf(type()));
    }

    [[nodiscard]] bool asBool(int line) const {
        if (type() == Type::INT)
            return std::get<int_t>(object) != 0;
        throw RuntimeError("expected int as bool but got " + type_name() + at(line));
    }

    [[nodiscard]] int_t& asInt(int line) {
        if (type() == Type::INT)
            return std::get<int_t>(object);
        throw RuntimeError("expected int but got " + type_name() + at(line));
    }

    [[nodiscard]] std::variant<int_t*, real_t*> asNumber(int line) {
        switch (type()) {
            case Type::INT: return &std::get<int_t>(object);
            case Type::REAL: return &std::get<real_t>(object);
        }
        throw RuntimeError("expected number but got " + type_name() + at(line));
    }

    [[nodiscard]] Object cast(Type type, int line) const {
        if (type == Type::ANY || this->type() == type) return *this;
        if (this->type() == Type::INT && type == Type::REAL) return {(real_t)std::get<int_t>(object)};
        throw RuntimeError("attempt to implicitly cast from " + type_name() + " to " + std::string(nameOf(type)) + at(line));
    }

    void invoke(ScriptEngine* engine, int line, std::vector<Object> const& arguments) const;

    [[nodiscard]] std::string toString() const;

    template<typename T>
    [[nodiscard]] T as() const {
        if constexpr(std::is_same_v<T, Object>) {
            return *this;
        } else {
            return std::get<(size_t) parseType<T>()>(object);
        }
    }
};

struct Operand {
    std::variant<Object, Object*> val_or_ref;
    Operand() = default;
    Operand(Operand const&) = default;
    Operand(Object val): val_or_ref(val) {}
    Operand(Object* ref): val_or_ref(ref) {}

    [[nodiscard]] Object val() const {
        return std::visit(overloaded {
                [](Object const& o) { return o; },
                [](Object* o) { return *o; }
        }, val_or_ref);
    }

    [[nodiscard]] Object* ref(int line) const {
        if (std::holds_alternative<Object*>(val_or_ref))
            return std::get<Object*>(val_or_ref);
        throw RuntimeError("rvalue cannot be used as lvalue" + at(line));
    }
};
}