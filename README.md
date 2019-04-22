# Meda
Meda is a systems programming language. Right now all I've got is a bunch of disorganized ideas about how it should be and the beginnings of an implementation.

## Rich type system
Must-haves:
- Discriminated unions
- Pattern matching
- Traits
- Parametric polymorphism
- Optional values in place of null pointers and sentinels

## Arbitrary compile-time code execution
I believe the benefits of this feature are obvious if you look at languages that have it, like D, Zig and Jai. Code introspection, code generation, and custom tools are examples of things made way better with compile-time code execution. However, I'd like to take the concept even further than those languages, by enabling users to write things like:
- code optimizations
- backends
- their own primitive metatypes, with fine-grained control over layout (like struct, class, enum, bitfield, etc.)
- importers/exporters of APIs to/from other languages, like (Objective-)C(++)

There may also be implementation-related benefits to thinking of types as mere compile-time evaluated expressions.

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

It's possible that this stuff will be too pedantic for general use. I honestly won't know until I have the chance to use it for a real project. I'm hopeful that perhaps something like Rust's borrow-checker could be expressed in terms of this system one day.