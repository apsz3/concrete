# Concrete (WIP)

Concrete is a statically-typed language, implemented with a bytecode interpreter. Parsing is done using Ply/Sly.

A potential goal is to make this a lightweight embedded language, similar to Lua, but statically typed.

Compiler code likely to be reduced to RPython or rewritten in C.

Codebase is WIP refactoring, transforming from an experimental / hobby project, to something more robust.

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

Run tests with

`pytest`


```
num a = 1
fun add (num a, num b) -> num {
    return a + b
}
num res = add(a,a)
print(res)

# Control flow implemented at global scope,
# subtle bug WIP in function scope;
# Else also WIP.
if res > 1:
    print("GT 1")
end
```

```
#  Basic functionality

num x = 1
x = 2
num y = x + 1
str abc = "abc"
str _def = abc + "def"

fun inc (num v) -> num {
    return v + 1
}

num incd = inc(y)

fun QUADRATIC(num a, num b, num c, num x) -> num {
    num A = a
    # Nested scope
    fun SQUARE (num x) -> num {
        num i = x * x
        return x
    }
    num res= (A * SQUARE(a)) + b * x + c
    return res
}

num quad = QUADRATIC(1,1,1,1)

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
* Lexical scoping of function parameters when the symbol exists in global scope
* Bug in conditionals always taking first case in function scope
* Bug in return type when recursive function calls involved in an expression
