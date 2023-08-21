A first foray into a statically-typed language, which is implemented as a bytecode interpreter. Concrete has static typing at the expression level. It also has lexical scoping. The language is not fully featured yet (for example, it lacks control flow), and only supports a few primitive types, but I see these things being added with more time.

This demo does not have a REPL. `python3 concrete.py` will run the code in `demo.ccr`.
Expressions that are ill-typed are commented out in the demo code. Remove the comment and re-run the command to see the compiler errors.

The below code is copied from `demo.ccr`

```
###### Basic functionality

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

# Resulting environment after running:
# {
#     'x': 2.0,
#     'y': 3.0
#     'abc': 'abc',
#     '_def': 'abcdef',
#     'incd': 4.0,
#     'quad': 3.0,
# }



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

# Known issues
# Lexical scoping of function parameters when the symbol exists in global scope

# Example of what compiled Bytecode looks like

# ('pnum', '1')
# ('pid', 'x')
# ('var_assgn',)
# ('pnum', '2')
# ('pid', 'x')
# ('var_assgn',)
# ('pval', 'x')
# ('pnum', '1')
# ('add',)
# ('pid', 'y')
# ('var_assgn',)
# ('pstr', '"abc"')
# ('pid', 'abc')
# ('var_assgn',)
# ('pval', 'abc')
# ('pstr', '"def"')
# ('add',)
# ('pid', '_def')
# ('var_assgn',)
# ('pval', 'y')
# ('call', '__module__.inc')
# ('pid', 'v')
# ('var_assgn',)
# ('pval', 'v')
# ('pnum', '1')
# ('add',)
# ('ret',)
# Leaving: {'v': 3.0}
# ('pid', 'incd')
# ('var_assgn',)
# ('pnum', '1')
# ('pnum', '1')
# ('pnum', '1')
# ('pnum', '1')
# ('call', '__module__.QUADRATIC')
# ('pid', 'a')
# ('var_assgn',)
# ('pid', 'b')
# ('var_assgn',)
# ('pid', 'c')
# ('var_assgn',)
# ('pid', 'x')
# ('var_assgn',)
# ('pval', 'a')
# ('pid', 'A')
# ('var_assgn',)
# ('pval', 'A')
# ('pval', 'a')
# ('call', '__module__.QUADRATIC.SQUARE')
# ('pid', 'x')
# ('var_assgn',)
# ('pval', 'x')
# ('pval', 'x')
# ('mul',)
# ('pid', 'i')
# ('var_assgn',)
# ('pval', 'x')
# ('ret',)
# Leaving: {'x': 1.0, 'i': 1.0}
# ('mul',)
# ('pval', 'b')
# ('pval', 'x')
# ('mul',)
# ('add',)
# ('pval', 'c')
# ('add',)
# ('pid', 'res')
# ('var_assgn',)
# ('pval', 'res')
# ('ret',)
# Leaving: {'a': 1.0, 'b': 1.0, 'c': 1.0, 'x': 1.0, 'A': 1.0, 'res': 3.0}
# ('pid', 'quad')
# ('var_assgn',)
# [] {'x': 2.0, 'y': 3.0, 'abc': 'abc', '_def': 'defabc', 'incd': 4.0, 'quad': 3.0}
# {'_def': 'defabc', 'abc': 'abc', 'incd': 4.0, 'quad': 3.0, 'x': 2.0, 'y': 3.0}
```