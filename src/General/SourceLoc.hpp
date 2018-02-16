//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

typedef int SourceLoc;

class SourceRange {
private:
    SourceLoc _begin;
    int _length;
public:
    SourceRange(SourceLoc begin, int length) : _begin(begin), _length(length) {}
    SourceRange() {}
    SourceLoc begin() const { return _begin; }
    int length() const { return _length; }
};
