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
        # Call the function that should throw an exception
    cc.concrete.Concrete().run(c)
#    assert str(e.value) == "Expected Exception message"  # Replace "Expected Exception message" with the actual expected exception message

@pytest.mark.parametrize("expr, res", [
    ("num x = 1 num y = 2 x + y", 3)
])
def test_valid_typing(expr, res):
        # Call the function that should throw an exception
    stack, sym = cc.concrete.Concrete().run(expr)
    assert len(stack) == 1
    assert stack.pop() == res
#    assert str(e.value) == "Expected Exception message"  # Replace "Expected Exception message" with the actual expected exception message


def function_that_throws_exception():
    raise Exception("Expected Exception message")

# def test_demo():
#     with open("tests/fib.ccr", "r") as fp:
#         text = fp.read()
#         cc().run(text)

# test_demo()