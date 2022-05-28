#include <string>
#include <stack>
#include <vector>
#include <unordered_map>
#include <cstring>

#include "Diagnostics.hpp"
#include "TypeSystem.hpp"
#include "Unicode.hpp"

namespace SauScript::Bytecode {

using byte = unsigned char;

enum class OpCode : byte {
    nop, load, const_, store, pop, jmp, jif, call, list, dict,
    ic, rc, sc, fc, param, local, ret
};

inline const char* OpLiterals[] = {
        "nop", "load", "const", "store", "pop", "jmp", "jif", "call", "list", "dict",
        "ic", "rc", "sc", "fc", "param", "local", "ret"
};

struct ByteBuf {
    byte buf[4096] = {0};
    byte* cur = buf;
    size_t mark = 0;

    // reset before sequential use
    void reset() { cur = buf; }

    void seek(size_t pos) { cur = buf + pos; }
    size_t pos() const { return cur - buf; }
    bool eof() const { return pos() == mark; }
    void setmark() { mark = pos(); }
    void op(OpCode opcode) { *cur++ = (byte)opcode; }
    void u(unsigned u) {
        *cur++ = u & 0xFF;
        *cur++ = (u >> 8) & 0xFF;
        *cur++ = (u >> 16) & 0xFF;
        *cur++ = (u >> 24) & 0xFF;
    }
    void i(int i) { u(i); }
    void ull(unsigned long long ull) {
        u(ull);
        u(ull >> 32);
    }
    void ll(long long ll) { ull(ll); }
    void r(double r) { ull(*(unsigned long long*)&r); };
    void s(std::string const& s) {
        ull(s.length());
        memcpy(cur, s.data(), s.length());
        cur += s.length();
    }

    OpCode op() { return (OpCode)*cur++; }
    unsigned u() {
        unsigned u = *cur++;
        u |= *cur++ << 8;
        u |= *cur++ << 16;
        u |= *cur++ << 24;
        return u;
    }
    int i() { return u(); }
    unsigned long long ull() {
        return u() | (unsigned long long)u() << 32;
    }
    long long ll() { return ull(); }
    double r() {
        unsigned long long r = ull();
        return *(double*)&r;
    }
    std::string s() {
        std::string s(ull(), '\0');
        memcpy(s.data(), cur, s.length());
        cur += s.length();
        return s;
    }
};

inline std::string trim(std::string const& line) {
    if (line.empty()) return "";
    size_t start = 0;
    while (start < line.length() && std::isspace(line[start])) ++start;
    size_t stop = line.length();
    while (stop && std::isspace(line[--stop]));
    if (start > stop) return "";
    return line.substr(start, stop - start + 1);
}

inline ByteBuf assemble(std::vector<std::string> const& lines) {
    ByteBuf buf;
    std::unordered_map<std::string, size_t> labels;
    std::unordered_map<size_t, std::string> jumps;
    for (auto line : lines) {
        line = trim(line);
        if (line.empty() || line[0] == ';') continue;
        if (std::isupper(line[0])) {
            std::string label = line.substr(0, line.find(':'));
            labels[label] = buf.pos();
            continue;
        }
        for (int i = 0; i < std::size(OpLiterals); ++i) {
            auto literal = OpLiterals[i];
            if (line.starts_with(literal)) {
                line = line.substr(strlen(literal));
                auto opcode = (OpCode)i;
                buf.op(opcode);
                switch (opcode) {
                    case OpCode::nop:
                    case OpCode::pop:
                    case OpCode::ret:
                        break;
                    case OpCode::load:
                    case OpCode::const_:
                    case OpCode::store:
                    case OpCode::call:
                    case OpCode::list:
                    case OpCode::dict:
                    case OpCode::local:
                        buf.u(std::stoul(line));
                        break;
                    case OpCode::jmp:
                    case OpCode::jif:
                        if (size_t pos = line.find(';'); pos != std::string::npos) {
                            line = line.substr(0, pos);
                        }
                        line = trim(line);
                        if (std::isupper(line[0])) {
                            buf.i(0);
                            jumps[buf.pos()] = line;
                        } else {
                            buf.i(std::stol(line));
                        }
                        break;
                    case OpCode::ic:
                        buf.ll(std::stoll(line));
                        break;
                    case OpCode::rc:
                        buf.r(std::stod(line));
                        break;
                    case OpCode::sc: {
                        line = trim(line);
                        const char* s = line.c_str();
                        buf.s(Unicode::unquoteString(s));
                    } break;
                    case OpCode::fc:
                    case OpCode::param:
                        impossible();
                }
            }
        }
    }
    buf.setmark();
    for (auto [pos, target] : jumps) {
        buf.seek(pos - 4);
        int jump = (int)labels[target] - (int)pos;
        buf.i(jump);
    }
    return buf;
}


inline void interpret(ByteBuf buf) {
    buf.reset();
    std::stack<Object> stack;
    std::vector<Object> local;
    std::vector<Object> const_;
    while(!buf.eof()) {
        switch (buf.op()) {
            case OpCode::nop:
                break;
            case OpCode::load:
                stack.push(local[buf.u()]);
                break;
            case OpCode::const_:
                stack.push(const_[buf.u()]);
                break;
            case OpCode::store:
                local[buf.u()] = stack.top();
                stack.pop();
                break;
            case OpCode::pop:
                stack.pop();
                break;
            case OpCode::jmp: {
                int offset = buf.i();
                buf.cur += offset;
            } break;
            case OpCode::jif: {
                bool cond = stack.top().asInt();
                stack.pop();
                int offset = buf.i();
                if (cond) buf.cur += offset;
            } break;
            case OpCode::call:{
                unsigned params = buf.u();
                long long f = stack.top().asInt();
                stack.pop();
                std::vector<Object> parameters;
                parameters.reserve(params);
                while (params--) parameters.push_back(stack.top()), stack.pop();
                switch (f) {
                    case -1: {
                        stack.push({parameters[0].asInt() + parameters[1].asInt()});
                    } break;
                    case -2: {
                        puts(parameters[0].toString(StringifyScheme::RUNTIME).c_str());
                        stack.push({});
                    } break;
                    case -3: {
                        stack.push({parameters[0].asInt() % parameters[1].asInt()});
                    } break;
                    default:
                        runtime("invalid function");
                }
            } break;
            case OpCode::list: {
                unsigned params = buf.u();
                std::vector<Object> elements;
                elements.reserve(params);
                while (params--) elements.push_back(stack.top()), stack.pop();
                stack.push({std::make_shared<List>(std::move(elements))});
            } break;
            case OpCode::dict: {
                unsigned pairs = buf.u();
                unsigned params = pairs << 1;
                std::vector<Object> elements;
                elements.reserve(params);
                while (params--) elements.push_back(stack.top()), stack.pop();
                std::unordered_map<Object, Object> map;
                for (int i = 0; i < pairs; ++i) {
                    map[elements[i << 1]] = elements[(i << 1) | 1];
                }
                stack.push({std::make_shared<Dictionary>(std::move(map))});
            } break;
            case OpCode::ic:
                const_.push_back({buf.ll()});
                break;
            case OpCode::rc:
                const_.push_back({buf.r()});
                break;
            case OpCode::sc:
                const_.push_back({std::make_shared<String>(buf.s())});
                break;
            case OpCode::fc:
            case OpCode::param:
                break;
            case OpCode::local:
                local.clear();
                local.resize(buf.u());
                break;
            case OpCode::ret:
                printf("ret with stack top: %s\n", stack.top().toString(StringifyScheme::DUMP).c_str());
                return;
        }
    }
}

inline std::string disassemble(ByteBuf buf) {
    buf.reset();
    std::string result;
    while(!buf.eof()) {
        OpCode opcode = buf.op();
        result += OpLiterals[(size_t)opcode];
        switch (opcode) {
            case OpCode::nop:
            case OpCode::pop:
            case OpCode::ret:
                break;
            case OpCode::load:
            case OpCode::const_:
            case OpCode::store:
            case OpCode::call:
            case OpCode::list:
            case OpCode::dict:
            case OpCode::local:
                result += ' ';
                result += std::to_string(buf.u());
                break;
            case OpCode::jmp:
            case OpCode::jif:
                result += ' ';
                result += std::to_string(buf.i());
                break;
            case OpCode::ic:
                result += ' ';
                result += std::to_string(buf.ll());
                break;
            case OpCode::rc:
                result += ' ';
                result += std::to_string(buf.r());
                break;
            case OpCode::sc:
                result += ' ';
                result += Unicode::quote(buf.s());
                break;
            case OpCode::fc:
            case OpCode::param:
                impossible();
        }
        result += '\n';
    }
    return result;
}

inline void __main() {
    FILE* file_assembly = fopen("assembly.txt", "r");
    FILE* file_disassembly = fopen("disassembly.txt", "w");
    if (!(file_assembly && file_disassembly)) {
        printf("Failed to open files\n");
        return;
    }
    std::vector<std::string> lines;
    do {
        char line[1024];
        memset(line, 0, sizeof line);
        fgets(line, sizeof line, file_assembly);
        lines.emplace_back(line);
    } while (!feof(file_assembly));
    fclose(file_assembly);
    ByteBuf buf = assemble(lines);
    interpret(buf);
    fputs(disassemble(buf).c_str(), file_disassembly);
}

}