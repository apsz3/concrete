A first foray into a statically-typed language, which is implemented as a bytecode interpreter. Concrete has static typing at the expression level. It also has lexical scoping. The language is not fully featured yet (for example, it lacks control flow), and only supports a few primitive types, but I see these things being added with more time.

This demo does not have a REPL. `python3 concrete.py` will run the code in `demo.ccr`.
Expressions that are ill-typed are commented out in the demo code. Remove the comment and re-run the command to see the compiler errors.