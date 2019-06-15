# Meda
Meda is a systems programming language. Right now all I've got is a bunch of disorganized ideas about how it should be and the beginnings of an implementation.

## Rich type system
Must-haves:
- Discriminated unions
- Pattern matching
- Traits
- Parametric polymorphism
- Optional type instead of null pointers or sentinels

## Arbitrary compile-time code execution
The more I program, the more convinced I become that this is an essential feature of any serious programming language. The idea is pretty simple: as programmers our job is to solve problems using computers. So why not apply our problem-solving skills to the problems we experience every day as programmers?

This isn't just a theoretical discussion, thankfully. Just look at what is possible with languages that have it, like D, Zig and Jai. Code introspection, code generation, and custom tools are great examples of things made way better with compile-time code execution. However, I'd like to take the concept even further, enabling users to write things like:
- optimizations
- backends
- calling conventions
- automated importers/exporters of APIs to/from other languages
- primitive data abstractions like struct, class, enum, etc., with fine-grained control over their layout

There may also be implementation-related benefits to thinking of types as mere compile-time evaluated expressions.

## Simple, stupid, modular compiler
Taking advantage of the previous thing (arbitrary compile-time code execution), I hope we will be able to keep the compiler itself unusually simple and stable for a language as complex as Meda. Most of the complexity should be in user-level libraries. This would have numerous benefits, such as:
- no dependencies
- minimal new features needed in the compiler over time, freeing up compiler developers to fix bugs and optimize
- easier to port the compiler to new host platforms
- easier to standardize the compiler
- easier to write new compiler implementations
- easier to prove correctness of a compiler implementation

As an example of a way in which complexity could be moved out of the compiler and into libraries, perhaps the base "compiler" won't actually know how to compile anything at all. Perhaps it will only be able to interpret bytecode (because it will already need to do that to enable the compile-time code execution stuff). Backends could then be implemented in libraries. For example, there might be an LLVM backend (for generating optimized code) and an x64 backend and an arm64 backend (for generating working code quickly), all implemented as their own libraries. For particularly important backends like those, they should be part of the official project as standard libraries.

Another example of the type of thing that could be in its own library is an importer/exporter of APIs to/from other languages. These would allow Meda to dethrone C as the new lingua franca of the programming world. Other than the modularity angle, Swift sets a great example with its ClangImporter, a part of its compiler that uses Clang to import C and Objective-C APIs. Something similar should be provided as a standard library for Meda. In addition, if Meda ends up supporting custom data abstractions, we could have an advantage over Swift in the sense that we wouldn't have to bolt the abstractions offered by Objective-C or C++ onto the similar, but distinct abstractions of a language like Swift (or vice versa). Instead, CStruct, CUnion, CxxClass, CxxEnumClass, ObjCClass, etc. could all be defined as metatypes in the ClangImporter library. These would then be free to work exactly as they do in the language from which they came.

## Refinement types and/or typestate
I'm not 100% sure about this yet, but I think it would be valuable for both expressivity and correctness to track statically-known information about values over time (at compile-time). For example, let's say we need to add 1 to a number, like so:
```
fn addOne(n: u8): u8 {
    n + 1
}
```

On first glance, it may seem like there's nothing worth talking about with regards this function. But what if you call it with the value `n = 255`? In C or C++, the result would overflow and the function would return 0. This is bad. In safer languages like Rust or Swift, your program would crash at runtime. This is arguably less bad, but it's still bad. Instead, what if we could statically guarantee that this condition will never happen? That's where refinement types come in. The above code wouldn't compile, hopefully with a helpful error message on the expression `n + 1` that says that overflow could occur if n == 255. To fix, we would have at least two options. The first option is to use control flow to guarantee that the expression is never executed in the exceptional case:
```
// Example #1
fn addOne(n: u8): u8 {
    if n == 'max {
        panic // explicit trap
    }

    // Ok, because we would've panicked if n + 1 didn't fit in u8
    n + 1
}

// Example #2
fn addOne(n: u8): u8 {
    if n < 'max {
        // Ok, because we know that n + 1 fits in u8
        n + 1
    } else {
        0 // explicit overflow
    }
}
```

In either case, the compiler should be smart enough to accept the code. Alternatively, we could choose to shift the responsibility of handling this case onto the caller of the function by adding a precondition to the interface:
```
fn addOne(n: u8): u8 where n < 'max {
    n + 1
}
```

Then, as you'd expect, when the caller tries to pass in a value of `n` that may be equal to `u8.max`, they would get an error message.

It's possible that this stuff will be too pedantic for general use. It's equally possible that it won't provide much tangible benefit due to limitations in what kinds of analysis are decidable at compile-time. I can't know until I've had the chance to use it for a real project.