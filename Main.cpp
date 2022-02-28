#include "SauScript.hpp"

#include <iostream>
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
        char line[256];
        memset(line, 0, sizeof line);
        fgets(line, sizeof line, file_script);
        script += line;
    } while (!feof(file_script));
//    puts("source code: ");
//    puts(script.c_str());
//    auto compiled = engine.compile(script.c_str());
//    puts("dump code: ");
//    puts(compiled->toString().c_str());
//    puts("syntax tree: ");
//    puts(compiled->walk().c_str());
    engine.exec(script);
}

[[noreturn]] void repl() {
    ScriptEngine engine;

    while (true) {
        string line;
        printf(">>>");
        getline(cin, line);
        engine.exec(line);
    }
}

int main() {
    file();
    //repl();
}