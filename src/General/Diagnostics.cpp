//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Diagnostics.h"

void Diagnostic::print(std::ostream& stream)  {
    std::sort(ranges.begin(), ranges.end(), [](auto a, auto b) {
        return a.range.begin.pos < b.range.begin.pos;
    });
    switch(kind) {
        case Error:
            stream << "\033[31merror: ";
            break;
        case Warning:
            stream << "\033[33mwarning: ";
            break;
    }
    stream << message << "\033[0m\n";
    std::vector<std::vector<LineRange>> lineRangeLists;
    auto numDigits = [](uint32_t num) -> uint32_t {
        uint32_t digits = 0;
        while(num > 0) {
            num /= 10;
            digits++;
        }
        return digits;
    };
    auto printSequence = [&](auto seq, auto numTimes) {
        for(decltype(numTimes) i = 0; i < numTimes; i++) {
            stream << seq;
        }
    };
    auto printSpacing = [&](auto spacing) {
        printSequence(' ', spacing);
    };
    uint32_t maxLineNumberSize = 0;
    for(auto& range: ranges) {
        lineRangeLists.push_back(file.linesInRange(range.range));
        for(auto& range: lineRangeLists.back()) {
            maxLineNumberSize = std::max(maxLineNumberSize, numDigits(range.line));
        }
    }
    auto printLineNum = [&](auto num) {
        auto asString = std::to_string(num);
        stream << asString;
        printSpacing(maxLineNumberSize - asString.size());
    };
    auto printSourceLine = [&](auto lineNum) {
        printLineNum(lineNum);
        auto sub = file.substringFromLine(lineNum);
        stream << " | " << sub;
    };
    auto printEmptyLine = [&] {
        printSpacing(maxLineNumberSize);
        stream << " | ";
    };
    printEmptyLine();
    stream << '\n';
    for(uint32_t rnge = 0; rnge < ranges.size(); rnge++) {
        auto& range = ranges[rnge];
        auto& lines = lineRangeLists[rnge];
        std::optional<std::vector<LineRange>> nextLines;
        if((rnge + 1) < ranges.size()) {
            nextLines = lineRangeLists[rnge + 1];
        }
        bool first = true;
        for(LineRange line: lines) {
            if(!first) {
                stream << '\n';
            } else {
                first = false;
            }

            printSourceLine(line.line);
            printEmptyLine();
            printSpacing(line.startColumn.pos);
            auto numberOfHighlights = line.endColumn.pos - line.startColumn.pos;
            if(range.isPrimary) {
                printSequence('^', numberOfHighlights);
            } else {
                printSequence('-', numberOfHighlights);
            }
        }
        if(range.message) {
            // FIXME: Wrap the range message on to multiple lines if necessary.
            stream << ' ' << *range.message;
        }
        stream << '\n';
        if(nextLines && nextLines->front().line <= lines.back().line + 2) {
            for(auto i = lines.back().line + 1; i < nextLines->front().line; i++) {
                printSourceLine(i);
            }
        } else if(!nextLines) {
            stream << '\n';
        } else {
            stream << "...\n";
        }
    }
}
