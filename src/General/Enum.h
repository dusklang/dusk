//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#define LIST_FIRST_CASE(enumName, caseName, ...) caseName
#define LIST_NORMAL_CASE(enumName, caseName, ...) , caseName
#define PRINT_STRUCT_CASE(enumName, caseName, ...) struct caseName { __VA_ARGS__; };
#define PRINT_CONSTRUCTOR(enumName, caseName, ...) enumName(caseName data) : data(data) {}
#define BEGIN_ENUM(name, cases) \
    cases(name, PRINT_STRUCT_CASE, PRINT_STRUCT_CASE) \
    struct name { \
        using Data = std::variant<cases(name, LIST_FIRST_CASE, LIST_NORMAL_CASE)>; \
        Data data; \
        \
        name(Data data): data(data) {} \
        cases(name, PRINT_CONSTRUCTOR, PRINT_CONSTRUCTOR)
#define END_ENUM() };
#define DECLARE_ENUM(name, cases) BEGIN_ENUM(name, cases) END_ENUM()
