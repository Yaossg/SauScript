#include "SauScript.hpp"

#include <iostream>

using namespace std;
using SauScript::ScriptEngine;

void file() {
    ScriptEngine engine;
    FILE* fp = fopen("script.txt", "r");
    if (fp == NULL) {
        printf("Failed to open script.txt\n");
        return;
    }
    string script;
    do {
        char line[256];
        memset(line, 0, sizeof line);
        fgets(line, sizeof line, fp);
        script += line;
    } while (!feof(fp));
    puts(script.c_str());
    engine.exec(script.c_str());
}

[[noreturn]] void repl() {
    ScriptEngine engine;

    while (true) {
        string line;
        printf(">>>");
        getline(cin, line);
        engine.exec(line.c_str());
    }
}

int main() {
    file();
    repl();
}