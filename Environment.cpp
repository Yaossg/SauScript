#include "Node.hpp"
#include "Unicode.hpp"

#include <cmath>
#include <numeric>
#include <numbers>
#include <chrono>

namespace SauScript {

void ScriptEngine::initEnv() {
    // intrinsics
    install("eval",     [this](str_t script) { return eval(script->bytes); });
    install("repr",     [](Object object) { return std::make_shared<String>(object.toString(StringifyScheme::DUMP)); });
    install("toString", [](Object object) { return std::make_shared<String>(object.toString(StringifyScheme::RUNTIME)); });
    install("hashCode", [](Object object) { return object.hashCode(); });
    install("typeid",   [](Object object) { return (int_t) object.type(); });
    install("typename", [](Object object) { return std::make_shared<String>(object.type_name()); });
    install("int",      [](int_t x) { return x; });
    install("int",      [](real_t x) { return int_t(x); });
    install("real",     [](int_t x) { return real_t(x); });
    install("real",     [](real_t x) { return x; });
    install("isASCII",  [](int_t x) { return char32_t(x) == x && Unicode::isASCII(x); });
    install("isUnicode",[](int_t x) { return char32_t(x) == x && Unicode::isUnicode(x); });
    install("apply",    [this](Object func, list_t args) { return func.invoke(this, args->elements); });
    install("global",   [this] {
        std::unordered_map<Object, Object> elements;
        for (auto&& [key, value] : global()) {
            elements[{std::make_shared<String>(key)}] = value;
        }
        return std::make_shared<Dictionary>(elements);
    });
    install("local",   [this] {
        std::unordered_map<Object, Object> elements;
        for (auto&& [key, value] : scopes[scopes.size() - 2]) {
            elements[{std::make_shared<String>(key)}] = value;
        }
        return std::make_shared<Dictionary>(elements);
    });
    // io
    install("flush",    [this] { std::fflush(out); });
    install("output",   [this](str_t file) {
        if (!freopen(file->bytes.c_str(), "w", out)) runtime("failed to reopen output stream");
    });
    install("eof",      [this] { return std::feof(in); });
    install("input",    [this](str_t file) {
        if (!freopen(file->bytes.c_str(), "r", in)) runtime("failed to reopen input stream");
    });
    install("putchar",  [this](int_t ch) {
        std::fprintf(out, "%s", Unicode::encodeUnicode(ch).c_str());
    });
    install("print",    [this](Object obj) {
        std::fprintf(out, "%s", obj.toString(StringifyScheme::RUNTIME).c_str());
    });
    install("println",  [this] {
        std::fprintf(out, "\n");
    });
    install("println",  [this](Object obj) {
        std::fprintf(out, "%s\n", obj.toString(StringifyScheme::RUNTIME).c_str());
    });
    install("getchar",  [this] { return Unicode::fgetc(in); });
    install("readLine", [this] {
        return std::make_shared<String>(Unicode::fgets(in, [](char32_t ch) { return ch == '\n'; }));
    });
    install("readWord", [this] {
        return std::make_shared<String>(Unicode::fgets(in));
    });
    install("readInt",  [this] {
        return eval(Unicode::fgets(in)).asInt();
    });
    install("readReal", [this] {
        return eval(Unicode::fgets(in)).cast(Type::REAL);
    });
    install("readUntil",[this](func_t until) {
        return std::make_shared<String>(Unicode::fgets(in, [this, &until](char32_t ch) {
            return until->invoke(this, {{ch}}).asBool();
        }));
    });
    // list
    install("copy",     [](list_t list) { return std::make_shared<List>(list->elements); });
    install("empty",    [](list_t list) { return (int_t) list->elements.empty(); });
    install("size",     [](list_t list) { return (int_t) list->elements.size(); });
    install("push",     [](list_t list, Object obj) { list->mut().push_back(obj); });
    install("pop",      [](list_t list) { if (list->elements.empty()) runtime("empty list cannot be popped"); list->mut().pop_back(); });
    install("clear",    [](list_t list) { list->mut().clear(); });
    install("concat",   [](list_t list1, list_t list2) {
        std::vector<Object> result;
        result.reserve(list1->elements.size() + list2->elements.size());
        for (auto &&obj: list1->elements) {
            result.push_back(obj);
        }
        for (auto &&obj: list2->elements) {
            result.push_back(obj);
        }
        return std::make_shared<List>(std::move(result));
    });
    install("flat",     [](list_t list) {
        std::vector<Object> result;
        for (auto &&obj: list->elements) {
            for (auto iterable = obj.asIterable(); auto &&obj: iterable->elements) {
                result.push_back(obj);
            }
        }
        return std::make_shared<List>(std::move(result));
    });
    install("insert",   [](list_t list, int_t index, Object obj) {
        if (index < 0 || index > list->elements.size()) runtime("index out of bound");
        list->mut().insert(list->elements.begin() + index, obj);
    });
    install("slice",    [](list_t list, int_t from, int_t to) {
        if (from > to) runtime("from > to");
        if (from < 0 || to > list->elements.size()) runtime("index out of bound");
        std::vector<Object> result;
        for (int_t i = from; i < to; ++i) {
            result.push_back(list->elements[i]);
        }
        return std::make_shared<List>(result);
    });
    install("removeAt", [](list_t list, int_t index) {
        if (index < 0 || index >= list->elements.size()) runtime("index out of bound");
        list->mut().erase(list->elements.begin() + index);
    });
    install("remove",   [](list_t list, Object obj) {
        erase(list->mut(), obj);
    });
    install("find",     [](list_t list, Object obj) {
        int_t index = std::find(list->elements.begin(), list->elements.end(), obj) - list->elements.begin();
        return list->elements.size() == index ? -1 : index;
    });
    install("find_if", [this](list_t list, func_t predicate) {
        int_t index = std::find_if(list->elements.begin(), list->elements.end(), [this, &predicate](Object const &a) {
            return predicate->invoke(this, {a}).asBool();
        }) - list->elements.begin();
        return list->elements.size() == index ? -1 : index;
    });
    install("reverse",  [](list_t list) { std::reverse(list->mut().begin(), list->mut().end()); });
    install("sort",     [this](list_t list, func_t comparator) {
        std::sort(list->mut().begin(), list->mut().end(), [this, &comparator](Object const &a, Object const &b) {
            return comparator->invoke(this, {a, b}).asBool();
        });
    });
    install("iota",     [](int_t size) {
        std::vector<Object> result;
        result.reserve(size);
        for (int_t i = 0; i < size; ++i) {
            result.push_back({i});
        }
        return std::make_shared<List>(std::move(result));
    });
    install("list",     [](int_t size, Object object) {
        return std::make_shared<List>(std::vector<Object>(size, object));
    });
    install("map", [this](list_t list, func_t mapper) {
        std::vector<Object> result;
        result.reserve(list->elements.size());
        for (auto &&obj: list->elements) {
            result.push_back(mapper->invoke(this, {obj}));
        }
        return std::make_shared<List>(std::move(result));
    });
    install("filter", [this](list_t list, func_t filter) {
        std::vector<Object> result;
        result.reserve(list->elements.size());
        for (auto &&obj: list->elements) {
            if (filter->invoke(this, {obj}).asBool())
                result.push_back(obj);
        }
        return std::make_shared<List>(std::move(result));
    });
    install("reduce", [this](list_t list, func_t acc) {
        if (list->elements.empty()) runtime("empty list cannot be reduced without init");
        Object init = list->elements[0];
        bool first = true;
        for (auto &&obj: list->elements) {
            if (first) { first = false; }
            else { init = acc->invoke(this, {init, obj}); }
        }
        return init;
    });
    install("reduce", [this](list_t list, Object init, func_t acc) {
        for (auto &&obj: list->elements) {
            init = acc->invoke(this, {init, obj});
        }
        return init;
    });
    // string
    install("copy",     [](str_t str) { return std::make_shared<String>(str->bytes); });
    install("empty",    [](str_t str) { return (int_t) str->bytes.empty(); });
    install("concat",   [](str_t str1, str_t str2) { return std::make_shared<String>(str1->bytes + str2->bytes); });
    install("startsWith",[](str_t str1, str_t str2) { return str1->bytes.starts_with(str2->bytes); });
    install("endsWith", [](str_t str1, str_t str2) { return str1->bytes.ends_with(str2->bytes); });
    install("quote",    [](int_t ch) { return std::make_shared<String>(Unicode::quote(ch)); });
    install("quote",    [](str_t str) { return std::make_shared<String>(Unicode::quote(str->bytes)); });
    install("decode",   [](str_t str) {
        std::u32string utf32 = Unicode::decodeUnicode(str->bytes);
        std::vector<Object> result;
        for (char32_t ch: utf32) {
            result.push_back({int_t(ch)});
        }
        return std::make_shared<List>(result);
    });
    install("encode",   [](list_t list) {
        std::u32string utf32;
        for (auto &&obj: list->elements) {
            utf32 += char32_t(obj.asInt());
        }
        return std::make_shared<String>(Unicode::encodeUnicode(utf32));
    });
    install("substr",   [](str_t str, int_t from, int_t to) {
        if (from > to) runtime("from > to");
        std::u32string utf32 = Unicode::decodeUnicode(str->bytes);
        if (from < 0 || to > utf32.size()) runtime("index out of bound");
        return std::make_shared<String>(Unicode::encodeUnicode(utf32.substr(from, to - from)));
    });
    install("join",     [](list_t list) {
        std::string result;
        for (auto &&obj: list->elements) {
            result += obj.toString(StringifyScheme::RUNTIME);
        }
        return std::make_shared<String>(result);
    });
    install("str",      [](int_t size, int_t ch) {
        return std::make_shared<String>(Unicode::encodeUnicode(std::u32string(size, char32_t(ch))));
    });
    // dict
    install("copy",     [](dict_t dict) { return std::make_shared<Dictionary>(dict->elements); });
    install("empty",    [](dict_t dict) { return (int_t) dict->elements.empty(); });
    install("size",     [](dict_t dict) { return (int_t) dict->elements.size(); });
    install("put",      [](dict_t dict, Object key, Object value) { dict->mut()[key] = value; });
    install("clear",    [](dict_t dict) { dict->mut().clear(); });
    install("contains", [](dict_t dict, Object key) { return (int_t)dict->elements.contains(key); });
    install("keys",     [](dict_t dict) {
        std::vector<Object> result;
        result.reserve(dict->elements.size());
        for (auto&& [key, value] : dict->elements) {
            result.push_back(key);
        }
        return std::make_shared<List>(std::move(result));
    });
    install("values",   [](dict_t dict) {
        std::vector<Object> result;
        result.reserve(dict->elements.size());
        for (auto&& [key, value] : dict->elements) {
            result.push_back(value);
        }
        return std::make_shared<List>(std::move(result));
    });
    // math
    install("ceil",     ::ceil);
    install("floor",    ::floor);
    install("trunc",    ::trunc);
    install("round",    ::llround);
    install("abs",      ::llabs);
    install("abs",      ::fabs);
    install("fmod",     ::fmod);
    install("remainder",::remainder);
    install("fma",      ::fma);
    install("min",      [](int_t a, int_t b) { return a < b ? a : b; });
    install("max",      [](int_t a, int_t b) { return a > b ? a : b; });
    install("min",      ::fmin);
    install("max",      ::fmax);
    install("exp",      ::exp);
    install("exp2",     ::exp2);
    install("expm1",    ::expm1);
    install("log",      ::log);
    install("log2",     ::log2);
    install("log10",    ::log10);
    install("log1p",    ::log1p);
    install("pow",      ::pow);
    install("sqrt",     ::sqrt);
    install("cbrt",     ::cbrt);
    install("hypot",    ::hypot);
    install("hypot",    (real_t(*)(real_t, real_t, real_t)) std::hypot);
    install("sin",      ::sin);
    install("cos",      ::cos);
    install("tan",      ::tan);
    install("asin",     ::asin);
    install("acos",     ::acos);
    install("atan",     ::atan);
    install("atan2",    ::atan2);
    install("sinh",     ::sinh);
    install("cosh",     ::cosh);
    install("tanh",     ::tanh);
    install("asinh",    ::asinh);
    install("acosh",    ::acosh);
    install("atanh",    ::atanh);
    install("erf",      ::erf);
    install("erfc",     ::erfc);
    install("tgamma",   ::tgamma);
    install("lgamma",   ::lgamma);
    install("beta",     std::beta<real_t, real_t>);
    install("gcd",      std::gcd<int_t, int_t>);
    install("lcm",      std::lcm<int_t, int_t>);
    install("midpoint", (int_t(*)(int_t, int_t)) std::midpoint);
    install("midpoint", (real_t(*)(real_t, real_t)) std::midpoint);
    install("lerp",     (real_t(*)(real_t, real_t, real_t)) std::lerp);
    // chrono
    install("nanos",    [] { return std::chrono::system_clock::now().time_since_epoch().count(); });
    install("micros",   [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000LL; });
    install("millis",   [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000'000LL; });
    install("seconds",  [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000'000'000LL; });
    // constants
    {
        using namespace std::numbers;
        global()["e"]           = {e};
        global()["log2e"]       = {log2e};
        global()["log10e"]      = {log10e};
        global()["pi"]          = {pi};
        global()["inv_pi"]      = {inv_pi};
        global()["inv_sqrtpi"]  = {inv_sqrtpi};
        global()["sqrt2"]       = {sqrt2};
        global()["sqrt3"]       = {sqrt3};
        global()["inv_sqrt3"]   = {inv_sqrt3};
        global()["egamma"]      = {egamma};
        global()["phi"]         = {phi};
    }
}

}
