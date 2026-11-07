# Dusk
[Dusk](https://dusklang.org/) is a work-in-progress systems programming language.

## Status
Pre-pre-alpha. There is a working interpreter and a VSCode plugin, and some early work has been done on a few backends and linkers, as shown in the tables below.

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
| macOS    |        | 🚧    |                 |      |            |
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

See AGENTS.md.
