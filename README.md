# Dusk
[Dusk](https://dusklang.org/) is a work-in-progress systems programming language.

## Status
Pre-pre-alpha. Barely usable for anything. There is a working interpreter and a VSCode plugin, and some early work has been done on a few backends and linkers, as shown in the tables below. But fundamental aspects of the language are still missing.

#### Legend
| Symbol | Meaning                                      |
|--------|----------------------------------------------|
| ⋯      | Planned                                      |
| 🚧     | Minimal functioning artifacts can be created |
| ✅     | Usable with real code                        |

### Backends
| Platform | x86-64 | arm64 | Dalvik bytecode | WASM | JavaScript |
|----------|--------|-------|-----------------|------|------------|
| Windows  | 🚧     | 🚧    |                 |      |            |
| macOS    | ⋯      | 🚧    |                 |      |            |
| Android  |        | ⋯     | 🚧              |      |            |
| Linux    | ⋯      | 🚧    |                 |      |            |
| Web      |        |       |                 | ⋯    | ⋯          |

### Linkers
| Format | Status |
|--------|--------|
| PE32+  | 🚧     |
| Mach-O | 🚧     |
| DEX    | 🚧     |
| ELF    | 🚧     |

### Bundlers
| Format            | Status |
|-------------------|--------|
| APK               | 🚧     |
| macOS app bundles | ⋯      |

## AI Policy

This project strictly forbids all AI-generated code, docs, etc., while still allowing agents to be used to assist in reading, reviewing and debugging. This is not set in stone, nor should it be taken as a personal statement against the use of AI coding tools more generally; my main reason for the policy is to give myself an outlet where I can do things more like how I used to!
