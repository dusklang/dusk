# Contexts
- Return
  - Gets val, returns it
  - Control destination must be unreachable
  - Outputs nothing
- Receive(value: Instr)
  - Gets val with one extra layer of indirection, stores value at the address it points to
  - Control destination is inherited
  - Outputs nothing
- Set(dest: Expr)
  - Gets val, stores it in dest
  - Control destination is inherited
  - Outputs nothing
- Store(location: Instr)
  - Gets val, stores it at address location points to
  - Control destination is inherited
  - Outputs nothing
- Read
  - Does not do anything with val
  - Control destination is inherited
  - Outputs value at its natural indirection
- Void
  - Does not do anything with val
  - Control destination is inherited
  - Outputs nothing
- Branch(true: BasicBlock, false: BasicBlock)
  - Gets val, and branches to the appropriate basic block
  - Control destination is inherited, and is respected after both blocks
  - Outputs nothing


# Handling indirection
- First, get Value with its "natural" indirection (this may also be influenced by the context)
  - I.e. a literal has a natural indirection of 0, while a variable ref has a natural indirection of 1
- Next, subtract the context's indirection from the Value's indirection
  - example: `*foo_mem`, where `foo_mem` is a variable of type *i32
    - `*foo_mem` will be read with indirection 0
      - `foo_mem` will be read with indirection -1
        - getting `foo_mem` will result in a Value with type `*i32`, and indirection 1
        - subtracting -1 from 1 results in the memory address of `foo_mem`, with an indirection of 2
- Finally, use the Value as intended