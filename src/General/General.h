//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>

#if defined(_MSC_VER)
    #define never __declspec(noreturn)
    #define unreachable panic("Code at line number % in file % supposed to be unreachable.", __LINE__, __FILE__)
#elif defined(__GNUC__)
    #define never _Noreturn
    #define unreachable panic("Code at line number % in file % supposed to be unreachable.", __LINE__, __FILE__)
#elif defined(__clang__)
    #define never [[gcc::noreturn]]
    #define unreachable __builtin_unreachable()
#endif

constexpr int numberOfPercents(char const* str) {
	int num = 0;
	for (int i = 0; str[i] != '\0'; i++) {
		if (str[i] == '%') {
			num++;
		}
	}
	return num;
}
constexpr int numberOfArgs(char const* str) {
	if (str[0] == '\0') { return 0; }
	int numCommas = 0;
	int curlies = 0;
	int parens = 0;
	bool isInQuotes = false;
	for (int i = 0; str[i] != '\0'; i++) {
		if (str[i] == '{' && !isInQuotes) {
			curlies++;
		}
		else if (str[i] == '}' && !isInQuotes) {
			curlies--;
		}
		else if (str[i] == '(' && !isInQuotes) {
			parens++;
		}
		else if (str[i] == ')' && !isInQuotes) {
			parens--;
		}
		else if (str[i] == '"') {
			if (isInQuotes && (i == 0 || str[i - 1] != '\\')) {
				isInQuotes = false;
			}
			else {
				isInQuotes = true;
			}
		}
		else if (str[i] == ',' && parens == 0 && curlies == 0 && !isInQuotes) {
			numCommas++;
		}
	}
	return numCommas + 1;
}

constexpr void printlnImpl(char const* str) {
	for (int i = 0; str[i] != '\0'; i++) {
		std::cout << str[i];
	}
	std::cout << '\n';
}

template <typename Arg, typename... Args>
constexpr void printlnImpl(char const* str, Arg arg, Args... args) {
	for (int i = 0; str[i] != '\0'; i++) {
		if (str[i] == '%') {
			std::cout << arg;
			printlnImpl(str + i + 1, args...);
			break;
		}
		else {
			std::cout << str[i];
		}
	}
	return;
}

#define println(str, ...) { \
    static_assert((numberOfArgs(#__VA_ARGS__)) == numberOfPercents(str), "number of '%'s in format string doesn't match number of arguments passed"); \
    printlnImpl(str, ##__VA_ARGS__); \
}

#define panic(str, ...) { \
	std::cout << "PANIC: "; \
    println(str, ##__VA_ARGS__); \
	abort(); \
}

inline void assertImpl(bool expr, char const* msg, char const* str) {
	if (expr) return;
	panic("ASSERTION FAILED: %\n%", msg, str);
}
inline void assertImpl(bool expr, char const* msg) {
	if (expr) return;
	panic("ASSERTION FAILED: %", msg);
}

#define assertTrue(expr) assertImpl(expr, #expr " is false.")
#define assertEqual(lhs, rhs) assertImpl(lhs == rhs, #lhs " does not equal " #rhs ".")
#define assertNotEqual(lhs, rhs) assertImpl(lhs != rhs, #lhs " should not equal " #rhs ".")

#define assertTrueMessage(expr, str) assertImpl(expr, #expr " is false.", str)
#define assertEqualMessage(lhs, rhs, str) assertImpl(lhs == rhs, #lhs " does not equal " #rhs ".", str)
#define assertNotEqualMessage(lhs, rhs, str) assertImpl(lhs != rhs, #lhs " should not equal " #rhs ".", str)
