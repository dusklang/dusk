# Dire: Dusk Intermediate REpresentation
This is (or will be) an intermediate representation library inspired by [MLIR](https://mlir.llvm.org/), primarily for use in the Dusk programming language.

Goals:
- Fast: to generate, and to execute.
- Extensible: Dusk programmers should be able to define new dialects (in MLIR parlance) to open up new capabilities and/or optimizations.
- Flexible: should be able to represent everything from super high-level code, all the way down to assembly.
