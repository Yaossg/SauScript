#include "SauScript.hpp"

#include <cstring>

using namespace std;
using SauScript::ScriptEngine;

void file() {
    FILE* file_script = fopen("script.txt", "r");
    FILE* file_output = fopen("output.txt", "w");
    FILE* file_input = fopen("input.txt", "r");
    if (!(file_script && file_input && file_output)) {
        printf("Failed to open files\n");
        return;
    }
    ScriptEngine engine(file_output, file_input);
    string script;
    do {
        char line[1024];
        memset(line, 0, sizeof line);
        fgets(line, sizeof line, file_script);
        script += line;
    } while (!feof(file_script));
    fclose(file_script);
//    puts("source code: ");
//    puts(script.c_str());
//    auto compiled = engine.compile(script.c_str());
//    puts("dump code: ");
//    puts(compiled->toString().c_str());
//    puts("syntax tree: ");
//    puts(compiled->walk().c_str());
    SauScript::Object ret = engine.eval(script);
    fclose(file_input);
    fclose(file_output);
    printf("Program finished and returned %s\n", ret.toString(SauScript::StringifyScheme::DUMP).c_str());
    fflush(stdout);
}

[[noreturn]] void repl() {
    ScriptEngine engine;

    while (true) {
        char line[1024];
        printf(">>>");
        fflush(stdout);
        fgets(line, sizeof line, stdin);
        SauScript::Object ret = engine.eval(line);
        if (ret.type() != SauScript::Type::VOID) {
            printf("%s\n", ret.toString(SauScript::StringifyScheme::DUMP).c_str());
            fflush(stdout);
        }
    }
}

#include "Bytecode.cpp"


void bytecode() {
    SauScript::Bytecode::__main();
}

int main() {
    //file();
    //repl();
    bytecode();


}