"""
We are developing a statically typed imperative language called Concrete.

The base data types are `num`, `str`, `bool`

This code illustrates the tenants of the language

```
num N = 5 # Assignment Statement
num _N = N + 1 # expression on the right hand side
# str N = 5 # Will raise NotWellTypedException; str N = "5" wouldn't.
fun fib (num n) -> num {
    num n = n # The language is lexically scoped
    if num == 0 return 0 end
    if num == 1 return 1 end
    return fib(n-1) + fib(n-2)
}
fib(N) # The S-expression tree can accept expressions and statements
```

The execution entry point is by calling the function RUN ("some Concrete program string")
Execution returns a stack (list) and symbol table (dict):

stack, sym = RUN("some Concrete source code string")

To check the end-program state, inspect the values on `stack`.
For example, we would expect running the above to leave 13 as the only
element on the stack.

Generate parametrized tests in the Pytest framework for this language, divided
into separate functions. There should be test functions for type checking (valid types),
type checking invalid types (which should raise NotWellTypedException), expressions,
sequences of statements, functions, and large programs
"""
import pytest

import concrete as cc

def run_test(func):
    def wrapper(expr, res):
        RUN = cc.concrete.Concrete().run
        stack, sym = RUN(expr)
        # Multiple values checked
        if res is None:
            assert stack == []
        elif isinstance(res, tuple):
            assert len(stack) == len(res)
            for a,b in zip(stack, res):
                assert a == b
        else:
            assert len(stack) == 1
            assert stack.pop() == res
    return wrapper



@pytest.mark.parametrize("c", [
    'num x = "a"',
    "num x = true",
    "str x = 1",
    "str x = 1.0",
    "str x = true",
    "bool x = 1",
])
def test_invalid_typing(c):
    with pytest.raises(cc.exceptions.NotWellTypedException): #as e:
        # Call the function that should throw an exception
        cc.concrete.Concrete().run(c)

@pytest.mark.parametrize("c", [
    'str x = "a"',
    "num x = 1",
    # "num x = 1.0",
    "bool x = true",
])
def test_valid_typing(c):
    cc.concrete.Concrete().run(c)

@pytest.mark.parametrize("expr, res", [
    ("num x = 1 num y = 2 x + y", 3),
    ("num x = 1 x", 1)
])
@run_test
def test_sequence(expr, res):
    pass

@pytest.mark.parametrize("expr, res", [
    ("fun id () -> void {}", None),
    ("fun id (num x) -> num { return x } id(1)", 1),
    ("fun id (num x) -> num { return x } num x = 1 id(x)", 1),
])
@run_test
def test_fn(expr, res):
    pass

@pytest.mark.parametrize("expr, res", [
    ("num a = 1 fun id (num x) -> num { return x } id(a)", 1),
    ("fun id (num x) -> num { x = 2 return x } id(x)", 2),
])
@run_test
def test_fn_binding(expr,res):
    pass

@pytest.mark.parametrize("expr, res", [

    ("num x = 1 fun id (num x) -> num { return x } id(x)", 1),
    ("num x = 1 fun id (num x) -> num { x = 2 return x } x id(x)", (1.0, 2.0)), # TODO: fix when float/int sorted
    ("num y = 100 fun add (num x, num y) -> num { return x + y } add(1,2)", 3),
    ("fun add (num x, num y) -> num { num z = x return z + y } add(1,2)", 3),
])
@run_test
def test_scope(expr, res):
    pass

@pytest.mark.parametrize("expr, res", [
    ("fun add (num x, num y) -> num { return x + y } add(1,2)", 3),
])
@run_test
def test_fn_advanced(expr, res):
    pass

@pytest.mark.parametrize("expr, res", [
    ("num x = 1 if x == 1 x = 2 num blah = 0 end x", 2),
    ("num x = 3 if x == 1 x = 2 num blah = 0 end x", 3),

    ("fun x () -> num { if true return 1 end return 2 } x()", 1),
    ("fun x () -> num { if false return 1 end return 2 } x()", 2),
    ("fun x (num z) -> num { if z == 1 return 1 end return 2 } x(2)", 2 )


])
@run_test
def test_cond(expr, res):
    pass
