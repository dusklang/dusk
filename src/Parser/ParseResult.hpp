//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "llvm/ADT/Optional.h"
#include "General/Diagnostic.hpp"
#include <iostream>

template<typename Success>
struct ParseResult {
private:
    union {
        Success _success;
        Diagnostic _failure;
    };
    bool isSuccess;
public:
    ParseResult(Success success) : _success(success), isSuccess(true) {}
    ParseResult(Diagnostic failure) : _failure(failure), isSuccess(false) {}
    ParseResult(const ParseResult<Success>& other) : isSuccess(other.isSuccess) {
        if(other.isSuccess) _success = other._success;
        else _failure = other._failure;
    }
    ~ParseResult() {};
    void operator=(const ::ParseResult<Success>& other) {
        isSuccess = other.isSuccess;
        if(other.isSuccess) _success = other._success;
        else _failure = other._failure;
    }
    llvm::Optional<Success> maybe() const {
        if(isSuccess) return _success;
        return llvm::None;
    }
    Success certain() const {
        if(isSuccess) return _success;
        std::cout << _failure << '\n';
        assert(false);
    }
    Diagnostic failure() const { assert(!isSuccess); return _failure; }
    operator bool() const { return isSuccess; }
    Success& operator* () const { return _success; }
    Success* operator-> () { return _success; }
};

template<typename Success>
ParseResult<Success> make_success(Success success) { return success; }
template<typename Success>
ParseResult<Success> make_failure(Diagnostic failure) { return failure; }
