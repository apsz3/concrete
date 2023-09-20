#!/usr/local/bin/python3
from pprint import pprint


from .cc_typing import Checker
from .lexer import CCRLexer
from .parser import CCRParser
from .vm import VM
from .compiler import walk
from .exceptions import *
import concrete.utils as u
# ---------- VM

class Concrete:
    def __init__(self):
        self.env = {"__module__": {"locals": {}}}

    def run(self, s, debug=False):
        u.DEBUG = debug

        lexer = CCRLexer(s)
        parser = CCRParser(s)
        checker = Checker(s)
        ast = parser.parse(lexer.tokenize(s))

        well_typed = checker.check_ast(ast, self.env)
        #    pprint(ast)
        if not lexer.HAS_LEXER_ERROR and not parser.HAS_PARSER_ERROR and well_typed:
            code = walk(ast, self.env, "__module__")  # compiling at the top-level
            #pprint(code)
            # pprint(env)
            #print("--------")
            stack, symbol_table = VM(code, self.env).run(debug)
            # pprint(env)
            #print("--------")
            # pprint(symbol_table)

        if not well_typed:
            raise NotWellTypedException()
        if lexer.HAS_LEXER_ERROR:
            raise HasLexerErrorException()
        if parser.HAS_PARSER_ERROR:
            raise HasParserErrorException()
