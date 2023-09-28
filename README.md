# Concrete (WIP)

Concrete is a statically-typed language, implemented with a bytecode interpreter. Parsing is done using Ply/Sly.

# Example

```
num n = 10
fun fib (num n) -> num {
    if n == 0 return 0 end
    if n == 1 return 1 end
    return fib(n-1) + fib(n-2)
}
print(fib(n)) # 55

fun quadratic(num a, num b, num c, num x) -> num {
    # Nested function
    fun square (num x) -> num {
       return x * x
    }
    num res = square(a) * x + b * x + c
    return res
}

print(QUADRATIC(1,1,1,1)) # 3
```

# About
A potential goal is to make this a lightweight embedded language, similar to Lua, but statically typed.

Compiler code likely to be reduced to RPython or rewritten in C.

Codebase is WIP refactoring, transforming from an experimental / hobby project, to something more robust.

# Usage

```
Usage: ccr [OPTIONS] [FILE]

Options:
  -i, --interactive  Run in interactive mode
  -s, --string TEXT  Interpret a string
  -h, --help         Display help information
  -d, --debug        Enable debug mode
  -p, --parse        Enable parse mode
  -c, --compile      Enable compiler mode
  -l, --lex          Enable lex mode
```

# Testing

Run tests with

`pytest`

# Typing

Illustrations of the static typing:

```
# Assignment type errors
# num x = 1 # TypeError: Cannot redefine x -- already defined
# x = "a" # TypeError:30:2 Cannot assign x of num to expression of type str

# Scoping type errors
# num res = SQUARE(2) # TypeError: unable to find function SQUARE in any scope.

# Type checking
# str bad_return_type = inc(1) # TypeError:36:2 Cannot assign bad_return_type of str to expression of type num
# num bad_input_type = inc("a") # TypeError:inc: Expected num for v; got str

# TypeError:52:6 Function bad of return type str but got expression of num
# fun bad () -> str {
#     return 1
# }
```
# Known issues
* [MAYBE NOT BUG] Lexical scoping of function parameters when the symbol exists in global scope
* [FIXED] Bug in conditionals always taking first case in function scope
* [FIXED] Bug in return type when recursive function calls involved in an expression
