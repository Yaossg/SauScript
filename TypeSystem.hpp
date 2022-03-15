#pragma once

#include <vector>
#include <memory>
#include <variant>
#include <unordered_map>

#include "Diagnostics.hpp"

namespace SauScript {

template<typename... Ts>
struct overloaded : Ts... {
    explicit overloaded(Ts... ts): Ts(ts)... {}
    using Ts::operator()...;
};

enum class Type {
    VOID, INT, REAL, FUNC, LIST, STR, DICT,
    ANY // as parameter or return type
};

inline std::unordered_map<std::string, Type> TYPES{
        {"void",    Type::VOID  },
        {"int",     Type::INT   },
        {"real",    Type::REAL  },
        {"func",    Type::FUNC  },
        {"list",    Type::LIST  },
        {"str",     Type::STR   },
        {"dict",    Type::DICT  },
        {"any",     Type::ANY   }
};

static std::string TYPE_NAMES[] = {"void", "int", "real", "func", "list", "str", "dict", "any"};

using int_t = long long;
using real_t = double;

inline std::string nameOf(Type type) {
    return TYPE_NAMES[(size_t) type];
}

enum class StringifyScheme {
    RUNTIME, DUMP, TREE_NODE
};

struct Parameter {
    Type type;
    std::string name;

    [[nodiscard]] std::string type_name() const {
        return nameOf(type);
    }

    [[nodiscard]] std::string toString() const {
        return name + ": " + type_name();
    }
};

struct Function {
    Type returnType;
    std::vector<Parameter> parameters;
    std::unique_ptr<ExprNode> expr;

    [[nodiscard]] std::string descriptor() const;
    [[nodiscard]] std::string toString() const;
    Object invoke(ScriptEngine* engine, std::vector<Object> const& arguments) const;
};

struct List;

struct String {
    const std::string bytes;

    explicit String(std::string bytes): bytes(std::move(bytes)) {}
};

struct Dictionary;

using func_t = std::shared_ptr<Function>;
using list_t = std::shared_ptr<List>;
using str_t = std::shared_ptr<String>;
using dict_t = std::shared_ptr<Dictionary>;

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
    } else if constexpr(std::is_same_v<T, str_t>) {
        return Type::STR;
    } else if constexpr(std::is_same_v<T, dict_t>) {
        return Type::DICT;
    } else if constexpr(std::is_same_v<T, Object>) {
        return Type::ANY;
    }
}

struct Object {
    std::variant<std::monostate, int_t, real_t, func_t, list_t, str_t, dict_t> object;

    [[nodiscard]] Type type() const {
        return (Type) object.index();
    }

    [[nodiscard]] std::string type_name() const {
        return nameOf(type());
    }

    [[nodiscard]] bool asBool() const {
        if (type() == Type::INT)
            return std::get<int_t>(object) != 0;
        runtime("expected int as bool but got " + type_name());
    }

    [[nodiscard]] int_t& asInt() {
        if (type() == Type::INT)
            return std::get<int_t>(object);
        runtime("expected int but got " + type_name());
    }

    [[nodiscard]] const int_t& asInt() const {
        if (type() == Type::INT)
            return std::get<int_t>(object);
        runtime("expected int but got " + type_name());
    }

    [[nodiscard]] std::variant<int_t*, real_t*> asNumber() {
        switch (type()) {
            case Type::INT: return &std::get<int_t>(object);
            case Type::REAL: return &std::get<real_t>(object);
        }
        runtime("expected number but got " + type_name());
    }

    [[nodiscard]] std::variant<int_t const*, real_t const*> asNumber() const {
        switch (type()) {
            case Type::INT: return &std::get<int_t>(object);
            case Type::REAL: return &std::get<real_t>(object);
        }
        runtime("expected number but got " + type_name());
    }

    [[nodiscard]] Object cast(Type type) const {
        if (type == Type::VOID) return {};
        if (type == Type::ANY || this->type() == type) return *this;
        if (this->type() == Type::INT && type == Type::REAL) return {(real_t)std::get<int_t>(object)};
        runtime("attempt to implicitly cast from " + type_name() + " to " + nameOf(type));
    }

    template<typename T>
    [[nodiscard]] T as() const {
        if constexpr(std::is_same_v<T, Object>) {
            return *this;
        } else {
            return std::get<(size_t) parseType<T>()>(object);
        }
    }

    void yield(std::vector<Object>& yield) const {
        if (type() != Type::VOID) yield.push_back(*this);
    }

    [[nodiscard]] bool isMutLocked() const;
    [[nodiscard]] bool isToStringLocked() const;
    [[nodiscard]] Operand member(Object const& index, bool mut) const;
    Object invoke(ScriptEngine* engine, std::vector<Object> const& arguments) const;
    [[nodiscard]] list_t asIterable() const;
    [[nodiscard]] std::string toString(StringifyScheme scheme) const;
    [[nodiscard]] int_t hashCode() const;
    bool operator==(Object const& other) const;
    std::partial_ordering operator<=>(Object const& other) const;
};

struct List {
    const std::vector<Object> elements;
    explicit List(std::vector<Object> elements): elements(std::move(elements)) {}

    mutable bool mutLock = false;
    struct MutLockGuard {
        List const* owner;
        explicit MutLockGuard(List const* owner): owner(owner) { owner->mutLock = true; }
        ~MutLockGuard() { owner->mutLock = false; }
    };
    std::vector<Object>& mut() {
        if (mutLock) runtime("attempt to modify a list locked in mutability");
        return const_cast<std::vector<Object>&>(elements);
    }
    mutable bool toStringLock = false;
    struct ToStringLockGuard {
        List const* owner;
        explicit ToStringLockGuard(List const* owner): owner(owner) { owner->toStringLock = true; }
        ~ToStringLockGuard() { owner->toStringLock = false; }
    };
    [[nodiscard]] std::string toString(StringifyScheme scheme) const;
    [[nodiscard]] int_t hashCode() const;
    Object invoke(ScriptEngine* engine, std::vector<Object> const& arguments) const;
};

}

template<>
struct std::hash<SauScript::Object> {
    size_t operator()(SauScript::Object const& rhs) const noexcept {
        return rhs.hashCode();
    }
};

namespace SauScript {

struct Dictionary {
    const std::unordered_map<Object, Object> elements;
    explicit Dictionary(std::unordered_map<Object, Object> elements): elements(std::move(elements)) {}
    mutable bool mutLock = false;
    struct MutLockGuard {
        Dictionary const* owner;
        explicit MutLockGuard(Dictionary const* owner): owner(owner) { owner->mutLock = true; }
        ~MutLockGuard() { owner->mutLock = false; }
    };
    std::unordered_map<Object, Object>& mut() {
        if (mutLock) runtime("attempt to modify a list locked in mutability");
        return const_cast<std::unordered_map<Object, Object>&>(elements);
    }
    mutable bool toStringLock = false;
    struct ToStringLockGuard {
        Dictionary const* owner;
        explicit ToStringLockGuard(Dictionary const* owner): owner(owner) { owner->toStringLock = true; }
        ~ToStringLockGuard() { owner->toStringLock = false; }
    };
    [[nodiscard]] std::string toString(StringifyScheme scheme) const;
    [[nodiscard]] int_t hashCode() const;
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

    [[nodiscard]] Object* ref() const {
        if (std::holds_alternative<Object*>(val_or_ref))
            return std::get<Object*>(val_or_ref);
        runtime("rvalue cannot be used as lvalue");
    }
};
}