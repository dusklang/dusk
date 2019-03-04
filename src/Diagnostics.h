#pragma once

#include <optional>
#include <iostream>

#include "SourceInfo.h"

#define ERR(message, file) Diagnostic(Diagnostic::Error, file, message)

struct ErrorRange {
    SourceRange range;
    const char* message;
    bool isPrimary;

    ErrorRange(SourceRange range, bool isPrimary, const char* message = nullptr)
        : range(range), message(message), isPrimary(isPrimary) {};
};

class Diagnostic {
    Array<ErrorRange> ranges;

public:
    enum Kind {
        Error, Warning
    };
    Kind const kind;
    SourceFile const& file;
    StringSlice message;

    Diagnostic(Kind kind, SourceFile const& file, StringSlice message) : kind(kind), file(file), message(message) {}
    Diagnostic range(SourceRange range, const char* message = nullptr) {
        ranges.append(ErrorRange(range, false, message));
        return *this;
    }
    Diagnostic primaryRange(SourceRange range, const char* message = nullptr) {
        ranges.append(ErrorRange(range, true, message));
        return *this;
    }

    void print(std::ostream& stream);
    void report() {
        print(std::cout);
        switch(kind) {
            case Diagnostic::Error:
                exit(1);
            case Diagnostic::Warning:
                break;
        }
    }
};
