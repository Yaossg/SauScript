#include "Node.hpp"
#include "Unicode.hpp"

#include <cmath>
#include <numeric>
#include <numbers>
#include <chrono>

namespace SauScript {

void initEnv(ScriptEngine* engine) {
    // intrinsics
    engine->installExternalFunction("eval",     [engine](str_t script) { return engine->eval(script->bytes); });
    engine->installExternalFunction("repr",     [](Object object) { return std::make_shared<String>(object.toString(StringifyScheme::DUMP)); });
    engine->installExternalFunction("toString", [](Object object) { return std::make_shared<String>(object.toString(StringifyScheme::RUNTIME)); });
    engine->installExternalFunction("identity", [](Object object) { return object; });
    engine->installExternalFunction("typeid",   [](Object object) { return (int_t)object.type(); });
    engine->installExternalFunction("typename", [](Object object) { return std::make_shared<String>(object.type_name()); });
    engine->installExternalFunction("int",      [](int_t x) { return x; });
    engine->installExternalFunction("int",      [](real_t x) { return int_t(x); });
    engine->installExternalFunction("real",     [](int_t x) { return real_t(x); });
    engine->installExternalFunction("real",     [](real_t x) { return x; });
    engine->installExternalFunction("isASCII",  [](int_t x) { return char32_t(x) == x && Unicode::isASCII(x); });
    engine->installExternalFunction("isUnicode",[](int_t x) { return char32_t(x) == x && Unicode::isUnicode(x); });
    // io
    engine->installExternalFunction("flush",    [out = engine->out] { std::fflush(out); });
    engine->installExternalFunction("output",   [out = engine->out](str_t file) {
        if (!freopen(file->bytes.c_str(), "w", out)) runtime("Failed to reopen output stream");
    });
    engine->installExternalFunction("eof",      [in = engine->in] { return std::feof(in); });
    engine->installExternalFunction("input",    [in = engine->in](str_t file) {
        if (!freopen(file->bytes.c_str(), "r", in)) runtime("Failed to reopen input stream");
    });
    engine->installExternalFunction("putchar",  [out = engine->out](int_t ch) {
        std::fprintf(out, "%s", Unicode::encodeUnicode(ch).c_str());
    });
    engine->installExternalFunction("print",    [out = engine->out](Object obj) {
        std::fprintf(out, "%s", obj.toString(StringifyScheme::RUNTIME).c_str());
    });
    engine->installExternalFunction("println",  [out = engine->out] {
        std::fprintf(out, "\n");
    });
    engine->installExternalFunction("println",  [out = engine->out](Object obj) {
        std::fprintf(out, "%s\n", obj.toString(StringifyScheme::RUNTIME).c_str());
    });
    engine->installExternalFunction("getchar",  [in = engine->in] { return Unicode::fgetc(in); });
    engine->installExternalFunction("readLine", [in = engine->in] {
        return std::make_shared<String>(Unicode::fgets(in, [](char32_t ch) { return ch == '\n'; }));
    });
    engine->installExternalFunction("readWord", [in = engine->in] {
        return std::make_shared<String>(Unicode::fgets(in));
    });
    engine->installExternalFunction("readInt",  [engine] {
        return engine->eval(Unicode::fgets(engine->in)).asInt();
    });
    engine->installExternalFunction("readReal", [engine] {
        return engine->eval(Unicode::fgets(engine->in)).cast(Type::REAL);
    });
    engine->installExternalFunction("readUntil",[engine](func_t until) {
        return std::make_shared<String>(Unicode::fgets(engine->in, [engine, &until](char32_t ch){
            return until->invokeExternally(engine, {{ch}}).asBool();
        }));
    });
    // list
    engine->installExternalFunction("copy",     [](list_t list) { return std::make_shared<List>(list->elements); });
    engine->installExternalFunction("empty",    [](list_t list) { return (int_t)list->elements.empty(); });
    engine->installExternalFunction("size",     [](list_t list) { return (int_t)list->elements.size(); });
    engine->installExternalFunction("push",     [](list_t list, Object obj) { list->mut().push_back(obj); });
    engine->installExternalFunction("pop",      [](list_t list) { list->mut().pop_back(); });
    engine->installExternalFunction("front",    [](list_t list) { return list->elements.front(); });
    engine->installExternalFunction("back",     [](list_t list) { return list->elements.back(); });
    engine->installExternalFunction("clear",    [](list_t list) { list->mut().clear(); });
    engine->installExternalFunction("concat",   [](list_t list1, list_t list2) {
        std::vector<Object> result;
        for (auto&& obj : list1->elements) {
            result.push_back(obj);
        }
        for (auto&& obj : list2->elements) {
            result.push_back(obj);
        }
        return std::make_shared<List>(std::move(result));
    });
    engine->installExternalFunction("flat",     [](list_t list) {
        std::vector<Object> result;
        for (auto&& obj : list->elements) {
            for (auto iterable = obj.asIterable(); auto&& obj : iterable->elements) {
                result.push_back(obj);
            }
        }
        return std::make_shared<List>(std::move(result));
    });
    engine->installExternalFunction("insert",   [](list_t list, int_t index, Object obj) {
        if (index < 0 || index > list->elements.size()) runtime("index out of bound");
        list->mut().insert(list->elements.begin() + index, obj);
    });
    engine->installExternalFunction("slice",    [](list_t list, int_t from, int_t to) {
        if (from > to) runtime("from > to");
        if (from < 0 || to > list->elements.size()) runtime("index out of bound");
        std::vector<Object> result;
        for (int_t i = from; i < to; ++i) {
            result.push_back(list->elements[i]);
        }
        return std::make_shared<List>(result);
    });
    engine->installExternalFunction("removeAt", [](list_t list, int_t index) {
        if (index < 0 || index >= list->elements.size()) runtime("index out of bound");
        list->mut().erase(list->elements.begin() + index);
    });
    engine->installExternalFunction("remove",   [](list_t list, Object obj) {
        erase(list->mut(), obj);
    });
    engine->installExternalFunction("find",     [](list_t list, Object obj) {
        int_t index = std::find(list->elements.begin(), list->elements.end(), obj) - list->elements.begin();
        return list->elements.size() == index ? -1 : index;
    });
    engine->installExternalFunction("find_if",  [engine](list_t list, func_t predicate) {
        int_t index = std::find_if(list->elements.begin(), list->elements.end(), [engine, &predicate](Object const& a) {
            return predicate->invokeExternally(engine, {a}).asBool();
        }) - list->elements.begin();
        return list->elements.size() == index ? -1 : index;
    });
    engine->installExternalFunction("reverse",  [](list_t list) { std::reverse(list->mut().begin(), list->mut().end()); });
    engine->installExternalFunction("sort",     [engine](list_t list, func_t comparator) {
        std::sort(list->mut().begin(), list->mut().end(), [engine, &comparator](Object const& a, Object const& b) {
            return comparator->invokeExternally(engine, {a, b}).asBool();
        });
    });
    engine->installExternalFunction("iota",     [](int_t size) {
        std::vector<Object> result;
        for (int_t i = 0; i < size; ++i) {
            result.push_back({i});
        }
        return std::make_shared<List>(std::move(result));
    });
    engine->installExternalFunction("list",     [](int_t size, Object object) {
        return std::make_shared<List>(std::vector<Object>(size, object));
    });
    engine->installExternalFunction("map",      [engine](list_t list, func_t mapper) {
        std::vector<Object> result;
        for (auto&& obj : list->elements) {
            result.push_back(mapper->invokeExternally(engine, {obj}));
        }
        return std::make_shared<List>(std::move(result));
    });
    engine->installExternalFunction("filter",   [engine](list_t list, func_t filter) {
        std::vector<Object> result;
        for (auto&& obj : list->elements) {
            if (filter->invokeExternally(engine, {obj}).asBool())
                result.push_back(obj);
        }
        return std::make_shared<List>(std::move(result));
    });
    engine->installExternalFunction("reduce",   [engine](list_t list, func_t acc) {
        if (list->elements.empty()) runtime("empty list cannot be reduced without init");
        Object init = list->elements[0];
        bool first = true;
        for (auto&& obj : list->elements) {
            if (first) { first = false; } else {
                init = acc->invokeExternally(engine, {init, obj});
            }
        }
        return init;
    });
    engine->installExternalFunction("reduce",   [engine](list_t list, Object init, func_t acc) {
        for (auto&& obj : list->elements) {
            init = acc->invokeExternally(engine, {init, obj});
        }
        return init;
    });
    // string
    engine->installExternalFunction("copy",     [](str_t str) { return std::make_shared<String>(str->bytes); });
    engine->installExternalFunction("empty",    [](str_t str) { return (int_t)str->bytes.empty(); });
    engine->installExternalFunction("concat",   [](str_t str1, str_t str2) { return std::make_shared<String>(str1->bytes + str2->bytes); });
    engine->installExternalFunction("startsWith",[](str_t str1, str_t str2) { return str1->bytes.starts_with(str2->bytes); });
    engine->installExternalFunction("endsWith", [](str_t str1, str_t str2) { return str1->bytes.ends_with(str2->bytes); });
    engine->installExternalFunction("quote",    [](int_t ch) { return std::make_shared<String>(Unicode::quote(ch)); });
    engine->installExternalFunction("quote",    [](str_t str) { return std::make_shared<String>(Unicode::quote(str->bytes)); });
    engine->installExternalFunction("decode",   [](str_t str){
        std::u32string utf32 = Unicode::decodeUnicode(str->bytes);
        std::vector<Object> result;
        for (char32_t ch : utf32) {
            result.push_back({int_t(ch)});
        }
        return std::make_shared<List>(result);
    });
    engine->installExternalFunction("encode",   [](list_t list) {
        std::u32string utf32;
        for (auto&& obj : list->elements) {
            utf32 += char32_t(obj.asInt());
        }
        return std::make_shared<String>(Unicode::encodeUnicode(utf32));
    });
    engine->installExternalFunction("substr",   [](str_t str, int_t from, int_t to) {
        if (from > to) runtime("from > to");
        std::u32string utf32 = Unicode::decodeUnicode(str->bytes);
        if (from < 0 || to > utf32.size()) runtime("index out of bound");
        return std::make_shared<String>(Unicode::encodeUnicode(utf32.substr(from, to - from)));
    });
    engine->installExternalFunction("join",     [](list_t list) {
        std::string result;
        for (auto&& obj : list->elements) {
            result += obj.toString(StringifyScheme::RUNTIME);
        }
        return std::make_shared<String>(result);
    });
    engine->installExternalFunction("str",     [](int_t size, int_t ch) {
        return std::make_shared<String>(Unicode::encodeUnicode(std::u32string(size, char32_t(ch))));
    });
    // math
    engine->installExternalFunction("ceil",     ::ceil);
    engine->installExternalFunction("floor",    ::floor);
    engine->installExternalFunction("trunc",    ::trunc);
    engine->installExternalFunction("round",    ::llround);
    engine->installExternalFunction("abs",      ::llabs);
    engine->installExternalFunction("abs",      ::fabs);
    engine->installExternalFunction("fmod",     ::fmod);
    engine->installExternalFunction("remainder",::remainder);
    engine->installExternalFunction("fma",      ::fma);
    engine->installExternalFunction("min",      [](int_t a, int_t b) { return a < b ? a : b; });
    engine->installExternalFunction("max",      [](int_t a, int_t b) { return a > b ? a : b; });
    engine->installExternalFunction("min",      ::fmin);
    engine->installExternalFunction("max",      ::fmax);
    engine->installExternalFunction("exp",      ::exp);
    engine->installExternalFunction("exp2",     ::exp2);
    engine->installExternalFunction("expm1",    ::expm1);
    engine->installExternalFunction("log",      ::log);
    engine->installExternalFunction("log2",     ::log2);
    engine->installExternalFunction("log10",    ::log10);
    engine->installExternalFunction("log1p",    ::log1p);
    engine->installExternalFunction("pow",      ::pow);
    engine->installExternalFunction("sqrt",     ::sqrt);
    engine->installExternalFunction("cbrt",     ::cbrt);
    engine->installExternalFunction("hypot",    ::hypot);
    engine->installExternalFunction("hypot",    (real_t(*)(real_t, real_t, real_t))std::hypot);
    engine->installExternalFunction("sin",      ::sin);
    engine->installExternalFunction("cos",      ::cos);
    engine->installExternalFunction("tan",      ::tan);
    engine->installExternalFunction("asin",     ::asin);
    engine->installExternalFunction("acos",     ::acos);
    engine->installExternalFunction("atan",     ::atan);
    engine->installExternalFunction("atan2",    ::atan2);
    engine->installExternalFunction("sinh",     ::sinh);
    engine->installExternalFunction("cosh",     ::cosh);
    engine->installExternalFunction("tanh",     ::tanh);
    engine->installExternalFunction("asinh",    ::asinh);
    engine->installExternalFunction("acosh",    ::acosh);
    engine->installExternalFunction("atanh",    ::atanh);
    engine->installExternalFunction("erf",      ::erf);
    engine->installExternalFunction("erfc",     ::erfc);
    engine->installExternalFunction("tgamma",   ::tgamma);
    engine->installExternalFunction("lgamma",   ::lgamma);
    engine->installExternalFunction("beta",     std::beta<real_t, real_t>);
    engine->installExternalFunction("gcd",      std::gcd<int_t, int_t>);
    engine->installExternalFunction("lcm",      std::lcm<int_t, int_t>);
    engine->installExternalFunction("midpoint", (int_t(*)(int_t, int_t))std::midpoint);
    engine->installExternalFunction("midpoint", (real_t(*)(real_t, real_t))std::midpoint);
    engine->installExternalFunction("lerp",     (real_t(*)(real_t, real_t, real_t))std::lerp);
    // chrono
    engine->installExternalFunction("nanos",    [] { return std::chrono::system_clock::now().time_since_epoch().count(); });
    engine->installExternalFunction("micros",   [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000LL; });
    engine->installExternalFunction("millis",   [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000'000LL; });
    engine->installExternalFunction("seconds",  [] { return std::chrono::system_clock::now().time_since_epoch().count() / 1'000'000'000LL; });
    // constants
    {
        using namespace std::numbers;
        engine->global()["e"]           = {e};
        engine->global()["log2e"]       = {log2e};
        engine->global()["log10e"]      = {log10e};
        engine->global()["pi"]          = {pi};
        engine->global()["inv_pi"]      = {inv_pi};
        engine->global()["inv_sqrtpi"]  = {inv_sqrtpi};
        engine->global()["sqrt2"]       = {sqrt2};
        engine->global()["sqrt3"]       = {sqrt3};
        engine->global()["inv_sqrt3"]   = {inv_sqrt3};
        engine->global()["egamma"]      = {egamma};
        engine->global()["phi"]         = {phi};
    }
}

}
