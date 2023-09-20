import pytest

import concrete as cc

@pytest.mark.parametrize("c", [
    "num x = 'a'",
    "num x = true",
    "str x = 1",
    "str x = 1.0",
    "str x = true",
    "bool x = 1",
])
def test_typing(c):
    with pytest.raises(cc.NotWellTypedException): #as e:
        # Call the function that should throw an exception
        cc.run(c)

#    assert str(e.value) == "Expected Exception message"  # Replace "Expected Exception message" with the actual expected exception message

def function_that_throws_exception():
    raise Exception("Expected Exception message")

# def test_demo():
#     with open("demo.ccr", "r") as fp:
#         text = fp.read()
