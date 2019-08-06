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
The more I program, the more convinced I become that this is an essential feature of any serious programming language. The idea is pretty simple: as programmers our job is to solve problems using computers. So why shouldn't we be able to apply our problem-solving skills to the problems we experience every day as programmers?

This isn't just a theoretical discussion anymore, either. Just look at what is possible with languages that have it, like D, Zig and Jai. Code introspection, code generation, and custom tools are great examples of things made way better with compile-time code execution. However, I'd like to take the concept even further, enabling users to write things like:
- optimizations
- backends
- calling conventions
- automated importers/exporters of APIs to/from other languages
- primitive data abstractions like struct, class, enum, etc., with fine-grained control over their layout

## Simple, stupid, modular compiler
Taking arbitrary compile-time code execution to its logical conclusion, one could imagine an unusually simple and stable compiler for a language as complex as Meda. By allowing users to write code that fundamentally changes the way the compiler works, the burden of accommodating every imaginable use case of the language would be transferred onto the backs of library authors. Instead of stacking more and more features on to the language over time, the compiler would stay minimal. This would have numerous benefits, such as:
- a compiler with zero direct dependencies
- fewer new features needed to be added in the compiler over time, freeing up compiler developers to fix bugs and optimize
- easier to port the compiler to new host platforms
- easier to standardize the compiler
- easier to write new compiler implementations
- easier to prove correctness of a compiler implementation

As an example of a way in which complexity could be moved out of the compiler and into libraries, perhaps the base "compiler" won't actually know how to compile anything at all. Perhaps it will only be able to generate and interpret bytecode (because it will already need to do that to enable the compile-time code execution stuff). Backends could then be implemented in libraries. For example, there might be an LLVM backend (for generating optimized code) and an x64 backend and an arm64 backend (for generating working code quickly), all implemented as their own libraries. For backends as important as those, they should be part of the official project as "standard" libraries.

Another example of the type of thing that could be in its own library is an importer/exporter of APIs to/from other languages. These would allow Meda to dethrone C as the new lingua franca of the programming world. Other than the modularity angle, Swift sets a great example with its ClangImporter, a part of its compiler that uses Clang to import C and Objective-C APIs. Something similar should be provided as a standard library for Meda. In addition, if Meda ends up supporting custom data abstractions, we could have an advantage over Swift in the sense that we wouldn't have to bolt the abstractions offered by Objective-C or C++ onto the similar, but distinct abstractions of a language like Swift (or vice versa). Instead, CStruct, CUnion, CxxClass, CxxEnumClass, ObjCClass, etc. could all be defined as metatypes in the ClangImporter library. These would then be free to work exactly as they do in the language from which they came.

## Refinement types and typestate
I think this feature has the potential to be highly valuable for both expressivity and correctness. We should track statically-known information about values over time (at compile-time). For example, let's say we need to add 1 to a number, like so:
```
fn addOne(n: u8): u8 {
    n + 1
}
```

This function is so simple, there's no way it could have any bugs! Right? Well, think about what would happen if you were to call it with the value `n = 255`. That is the maximum value representable by `u8`, an 8-bit unsigned integer. In C or C++, the result would overflow and the function would return 0. This is unlikely to be what you intended, except in some specific cases. In safer languages like Rust or Swift, your program would crash at run-time. This is arguably a better default, but it's still pretty bad. Instead, what if we could statically guarantee that this condition will never happen? That's where refinement types come in. The above code wouldn't compile, hopefully with a helpful error message on the expression `n + 1` that says that overflow could occur if n == 255. To fix, we would have at least two options. The first option is to use control flow to guarantee that the expression is never executed in the exceptional case:
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

In either case, the compiler should be smart enough to accept the code. The second option is to shift the responsibility of handling this case onto the caller of the function by adding a precondition to the interface:
```
fn addOne(n: u8): u8 where n < 'max {
    n + 1
}
```

Then, as you'd expect, when the caller tries to pass in a value of `n` that may be equal to `u8.max`, they would get an error message.

In addition to refinement types, it should be possible to declare *typestate* for a type. You could think of typestate as extra data that will be added to every instance of a type *solely for the purpose of being tracked via refinement*. This data isn't stored at run-time, and can be mutated, even without a mutable binding to the run-time data.

One example where typestate would be useful is in enabling memory safety features like Rust's borrow checker to be implemented in user-level code:
```
// Strawman typestate declaration syntax, inspired by Rust's `impl` syntax. This adds the typestate
// enclosed in curly braces to every instance of the specified type (in this case, every type).
// Think of it like appending a struct field. These can be accessed on an instance using
// `instance.typestate.insert_field_name_here`
typestate[T] T {
    // Same rules as the Rust memory model: you can have aliasing or mutability, but not both.
    borrow_status: enum {
        borrowed { ref_count: uint }
        mutablyBorrowed
    } = 'borrowed { ref_count: 0 }
}

Ref[T] :: struct {
    ptr: T*
}

impl[T] ImplicitCast[T*] for Ref[T] {
    // Protect against borrows of mutably-borrowed data
    pub fn implicit_cast(ptr: T*): Self where (*ptr).typestate.borrow_status matches 'borrowed {
        (*ptr).typestate.borrow_status.ref_count += 1
        Ref { ptr }
    }
}

impl[T] Drop for Ref[T] {
    fn drop(&mut self) where 
        // It's a bug in this borrow checker implementation if a reference is in scope while
        // its referenced value is not tracking it
        (*self.ptr).typestate.borrow_status matches 'borrowed &&
        (*self.ptr).typestate.borrow_status.ref_count > 0
    {
        (*self.ptr).typestate.borrow_status.ref_count -= 1
    }
}

MutRef[T] :: struct {
    ptr: T *mut
}

impl[T] ImplicitCast[T *mut] for MutRef[T] {
    // Protect against mutable borrows of previously borrowed data
    pub fn implicit_cast(ptr: T *mut): Self where (*ptr).typestate.borrow_status == 'borrowed { ref_count: 0 } {
        (*ptr).typestate.borrow_status = 'mutablyBorrowed
        MutRef { ptr }
    }
}

impl[T] Drop for MutRef[T] {
    // It's a bug in this borrow checker implementation if a reference is in scope while its referenced
    // value is not tracking it
    fn drop(&mut self) where (*self.ptr).typestate.borrow_status == 'mutablyBorrowed {
        (*self.ptr).typestate.borrow_status = 'borrowed { ref_count: 0 }
    }
}

// Example usage:
// Strawman keyword for specifying language defaults. `pointer_type` and `mut_pointer_type` must
// implement ImplicitCast[T]
defaults {
    pointer_type[T]: Ref[T]
    mut_pointer_type[T]: MutRef[T]
}

foo := 5
foo_ref = &foo
foo_mut_ref = &mut foo // Error: "can't instantiate `Ref[int]` from pointer because it requires
                       //         typestate `borrow_status` to be 'borrowed { ref_count: 0 },
                       //         found 'borrowed { ref_count: 1 }"
                       // (Obviously there needs to be a way for the library author to provide
                       // specialized, helpful error messages)
```
Note: There are two really important omissions in the code above: dereferences, and what Rust calls non-lexical lifetimes. The former is trivial, while the latter could be imitated by allowing references to be invalidated/dropped as new, conflicting references are created. This differs from the Rust model in that it would be legal to create as many conflicting references as you want, but illegal to use the invalidated ones. The code examples below assume that model is implemented.

This system could in theory offer finer, more specialized control over ownership and borrowing than Rust's built-in checker can. For example, a custom array type could keep track of all the currently-held borrows of individual elements (or more accurately, ranges of possibly-borrowed elements), instead of an all-or-nothing approach. That way this code would be illegal:
```
foo := [1, 2, 3, 4, 5]
[a, b] = [&mut foo.0, &mut foo.0] // The second reference invalidates the first here, but that's ok so far
*a += 1 // Error: reference 'a' invalidated by the creation of reference 'b'
*b += 1
```
While this code would be legal:
```
foo := [1, 2, 3, 4, 5]
[a, b] = [&mut foo.0, &mut foo.2] // Indices 0 and 2 are different, so both are valid references to hold
*a += 1
*b += 1
```

Another exciting use case for typestate is the compile-time enforcement of invariants for APIs from other languages. For example, take the Vulkan C API. It has a detailed specification that tells you how to correctly use it. Incorrect usage of the API may result in a crash, data corruption, a silent failure, or virtually anything else the user's driver decides to do. Validation layers exist to point out invalid usage of the API, but they are limited by the fact that they operate at run-time, not compile-time. This increases the latency between which programmers can write code and discover bugs in it. It also means they can't check all possible paths of execution, only the ones that execute during testing. A Vulkan FFI binding for Meda using typestate could solve these problems and more, by checking all possible paths of control flow for invalid usage, and doing so at compile-time. Additionally, because the user's full source code would be available, error messages could include rich location information to help pinpoint the error, and could include specific suggestions on how to fix your code.

I'm extremely excited by the possibilities of this technology in theory, but I need to implement it and use it in a real project to know for sure what its strengths and weaknesses are. I have a lot of unanswered questions about both the design and implementation. The technology may prove to be too pedantic for general use. It may not provide much in the way of tangible benefits due to limitations in what kinds of analysis are decidable at compile-time. I'm also concerned about the feasibility of enabling library authors to provide easy-to-understand error messages. Only time (and a healthy dose of work) will tell!