# Dusk
[Dusk](https://dusklang.org/) is a work-in-progress systems programming language.

## Goals
- General-purpose and scalable: should be well-suited to a wide variety of domains, including kernels, game engines, and high-level GUI applications
- Fast: both in runtime performance and compile times
- Extensible: the core language should be as simple as possible, while enabling advanced tools and abstractions typically thought of as compiler features to be implemented in libraries instead
- Safe: should prevent common mistakes, potentially using refinement types and typestate (just an idea at this point)
- Delightful interop with other languages and runtimes: in the fullness of time, it should be possible to easily use any library from Dusk—or integrate Dusk into any codebase—regardless of language. In the short to medium term, important APIs such as those provided for Win32, COM, WinRT, Objective-C, the JVM, GObject, Vulkan and OpenXR (to name a few that interest me) should feel as though they were designed to be used from Dusk, while imposing zero overhead compared to the best-performing alternative. Higher-level convenience interfaces that impose a small amount of overhead are fine, as long as it’s possible to drop down to the lower-level version.
- Single source, multiple contexts: it should be possible (and encouraged) to mix code intended to be run in completely different contexts. For example, suppose we are writing an Android app. A single file should be allowed to contain all of the following:
  - A `build` function that operates as a script for the build system (will not be included in the final output at all)
  - Procedural macros to reduce boilerplate, ala Rust (also not included in the output)
  - A main activity, which will be compiled to Dalvik bytecode
  - A Vulkan renderer implemented in specially-annotated functions which will be compiled to native ARM machine code with automatically-generated JNI glue, as necessary
  - Shaders that will run on the GPU, compiled to SPIR-V bytecode and consumed by the renderer

## Status
Pre-pre-alpha. There is a working interpreter and a VSCode plugin, and work has started on an arm64 backend for macOS. But fundamental aspects of the language are still missing.
