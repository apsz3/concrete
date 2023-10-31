#!/usr/local/bin/python3
from pprint import pprint


from .cc_typing import Checker
from .parser import parse
from .vm import VM
from .compiler import walk
from .exceptions import *
import concrete.utils as utils

# ---------- VM


class Concrete:
    def __init__(self):
        self.env = {"__module__": {"locals": {}}}

    def bytecode(self, s):
        checker = Checker(s)
        ast = parse(s)
        _well_typed = checker.check_ast(
            ast, self.env
        )  # This annotates the tree we need for compiling!
        code = walk(ast, self.env, "__module__")
        # TODO: need to print all the code blocks
        to_print = [
            (k, self.env["__module__"][k])
            for k in self.env["__module__"]
            if k != "locals"
        ]

        for ip, instr in enumerate(code):
            print("{:>12}  {}".format(ip, instr))

        for fn, val in to_print:
            print(f"{fn}:")
            for ip, instr in enumerate(val["code"]):
                print("{:>12}  {}".format(ip, instr))

    def run(self, s, debug=False):
        utils.DEBUG = debug

        checker = Checker(s)
        ast = parse(s)

        well_typed = checker.check_ast(ast, self.env)
        #    pprint(ast)
        if well_typed:
            code = walk(ast, self.env, "__module__")  # compiling at the top-level
            # pprint(code)
            # pprint(env)
            # print("--------")
            stack, symbol_table = VM(code, self.env).run(debug)

            utils.print_debug("--------")
            utils.print_debug("#- Env -#")
            utils.print_debug(self.env)
            utils.print_debug("--------")
            utils.print_debug("#- Sym -#")
            utils.print_debug(symbol_table)
            utils.print_debug("--------")
            utils.print_debug("#- Stk -#")
            utils.print_debug(stack)

        if not well_typed:
            raise NotWellTypedException()

        return stack, symbol_table
