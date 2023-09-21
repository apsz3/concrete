
DEBUG = False
# Compute column.
#     input is the input text string
#     idx is the index of the token from lexer
def find_column(text, idx):
    last_cr = text.rfind("\n", 0, idx)
    if last_cr < 0:
        last_cr = 0
    column = (idx - last_cr) + 1
    return column

from pprint import pprint
def print_debug(*args, **kwargs):
    if DEBUG:
        # TODO: pprint dicts / lists
        print(*args, **kwargs)
