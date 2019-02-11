//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <optional>
#include <string>
#include <iostream>

#include "SourceInfo.h"

struct ErrorRange {
    SourceRange range;
    std::optional<std::string> message;
    bool isPrimary;

    ErrorRange(SourceRange range, bool isPrimary, std::optional<std::string> message = std::nullopt)
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
    std::string const message;

    Diagnostic(Kind kind, SourceFile const& file, std::string message) : kind(kind), file(file), message(message) {}
    Diagnostic range(SourceRange range, std::optional<std::string> message = std::nullopt) {
        ranges.append(ErrorRange(range, false, message));
        return *this;
    }
    Diagnostic primaryRange(SourceRange range, std::optional<std::string> message = std::nullopt) {
        ranges.append(ErrorRange(range, true, message));
        return *this;
    }

    void print(std::ostream& stream);
};
