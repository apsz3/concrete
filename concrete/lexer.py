# ---------- Lexer
from sly import Lexer

RESERVED = [
    "if",
    "else",
    "end",

    "fun",
    "return",

    "num",
    "str",
    "bool",
    "void"
]
class CCRLexer(Lexer):

    HAS_LEXER_ERROR = False

    tokens = {
        TYPE,
        BOOLEAN,
        ID,
        NUMBER,
        STRING,
        FUN,
        BOP,
        RETURN,
        FUN_RET_SYNTAX,
        CONST,
        IF,
        ELSE,
        END,
        PRINT,
    }

    ignore = " \t"
    BOP = r">=|<=|==|!="
    BOOLEAN = r"true|false"
    literals = {
        "!",
        "=",
        ",",
        "+",
        "-",
        "*",
        "/",
        "(",
        ")",
        "[",
        "]",
        ";",
        '"',
        "'",
        "{",
        "}",
        ">",
        "<",
    }

    IF = r"if"
    ELSE = r"else"
    END = r"end"
    STRING = r'".*"'
    FUN = r"fun"
    RETURN = r"return"
    CONST = r"const"
    PRINT = r"print"

    NUMBER = r"\d+|0x[0-9a-fA-F]+"
    FUN_RET_SYNTAX = r"\->"

    def __init__(self, text):
        self.text = text

    TYPE = r"num|str|fun|void|bool|any"  # MUST MATCH THESE FIRST BEFORE ID
    # NOTE: this also messes things up and means we can't prefix
    # things with a type name ?
    ID = r"[a-zA-Z_][a-zA-Z0-9_]*"  # no leading numerics

    ignore_comment = r"\#.*"

    @_(r"\n+")
    def newline(self, t):
        self.lineno += t.value.count("\n")

    def error(self, t):
        last_cr = self.text.rfind("\n", 0, t.index)
        if last_cr < 0:
            last_cr = 0
        next_cr = self.text.find("\n", t.index)
        if next_cr < 0:
            next_cr = len(self.text) - t.index

        print(self.text[last_cr : t.index + (next_cr - t.index)])
        print("^" * (t.index + (next_cr - t.index) - last_cr))
        print(f"Illegal character {t.value[0]} on line {self.lineno}")

        self.index += 1  # Keep going with the lexing, even with the error
        self.HAS_LEXER_ERROR = True