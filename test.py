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
@pytest.mark.parametrize("c", [
    'num x = "a"',
    "num x = true",
    "str x = 1",
    "str x = 1.0",
    "str x = true",
    "bool x = 1",
])
def test_invalid_typing(c):
    RUN = cc.concrete.Concrete().run
    with pytest.raises(cc.exceptions.NotWellTypedException): #as e:
        # Call the function that should throw an exception
        RUN(c)

@pytest.mark.parametrize("c", [
    'str x = "a"',
    "num x = 1",
    # "num x = 1.0",
    "bool x = true",
])
def test_valid_typing(c):
    RUN = cc.concrete.Concrete().run
        # Call the function that should throw an exception
    RUN(c)
#    assert str(e.value) == "Expected Exception message"  # Replace "Expected Exception message" with the actual expected exception message

@pytest.mark.parametrize("expr, res", [
    ("num x = 1 num y = 2 x + y", 3),
    ("num x = 1 x", 1)
])
def test_sequence(expr, res):
        # Call the function that should throw an exception
    RUN = cc.concrete.Concrete().run
    stack, sym = RUN(expr)
    assert len(stack) == 1
    assert stack.pop() == res
#    assert str(e.value) == "Expected Exception message"  # Replace "Expected Exception message" with the actual expected exception message
# def test_demo():
#     with open("tests/fib.ccr", "r") as fp:
#         text = fp.read()
#         cc().run(text)

# test_demo()