# ---------- Parser
# from sly import Parser

# from .lexer import CCRLexer

from lark import Lark, Transformer, v_args
from lark.indenter import Indenter


class CCRTransformer(Transformer):
    def start(self, args):
        # Use this so we can group all the AST nodes
        # into an S-expr for pretty printing.
        return args
        # return s[1:-1].replace('\\"', '"')

    @v_args(inline=True)
    def assgn_stmt(self, name, expr):
        return ("assgn_stmt", name.value, expr)

    @v_args(inline=True)
    def decl_stmt(self, args):
        # Index [0] here because we only expect a single
        # input, which is a full name-type-anno
        return ("decl_stmt", args[0])

    @v_args(inline=True)
    def anno_assgn_stmt(self, name_anno, val):
        return ("anno_assgn_stmt", name_anno, val)

    # @v_args(inline=True)
    @v_args(inline=True)
    def if_stmt(self, cond, block_if, *block_elifs):
        # We include else here; it is nothing more than
        # just the final block to process.
        return ("if", cond, block_if, *block_elifs)
        # return ("if", cond, block_if, block_elifs)

    # @v_args(inline=True)
    # def else_stmt(self, block):
    #     # Use this so that we can more easily distinguish
    #     # between elifs, which might help us with semantic analysis
    #     return ("else", block)

    @v_args(inline=True)
    def elifs(self, cond, block):
        return ("elif", cond, block)

    @v_args(inline=True)
    def inline_if(self, expr_if, cond, *expr_else):
        # Use this so that we can more easily distinguish
        # between elifs, which might help us with semantic analysis
        return ("if", expr_if, cond, *expr_else)

    def flow_stmt(self, args):
        return ("flow_stmt", *args)  # str(args[0]))

    @v_args(inline=True)
    def block_assgn_stmt(self, name, block_stmts):
        return ("block_assgn_stmt", name.value, block_stmts)

    @v_args(inline=True)
    def block(self, name, *stmts):
        return ("block", name, stmts)

    def return_stmt(self, args):
        return ("return", *args)

    def name_type_anno(self, args):
        return ("name_type_anno", args[0].value, args[1])

    def type(self, args):
        return ("type", args[0])

    def type_nullable(self, args):
        return ("type_nullable", args)

    @v_args(inline=True)
    def fun(self, name, args_list, type_anno, block):
        return ("fun", name.value, args_list, type_anno, block)

    def cs_list(self, args):
        return args

    # Expressions
    # Operations
    @v_args(inline=True)
    def op_binop(self, a, op, b):
        # The op_binop is a Tree() in Lark,
        # which must be accessed via `.data`
        return (op.data, a, b)

    @v_args(inline=True)
    def op_unary(self, op, a):
        # The op_binop is a Tree() in Lark,
        # which must be accessed via `.data`
        return (op.data, a)

    # Values
    @v_args(inline=True)
    def type(self, _t):
        return ("type", _t.value)

    @v_args(inline=True)
    def type_nullable(self, _t):
        return ("type_nullable", _t.value)

    @v_args(inline=True)
    def name_untyped(self, name):
        return ("name_untyped", name.value)

    # @v_args(inline=True)
    # def naked_expr(self, name):
    #     # An expression in a statement
    #     return ("naked_expr", name)

    @v_args(inline=True)
    def NAME(self, name):
        return name

    @v_args(inline=True)
    def string(self, s):
        return s  # This doesnt trim qts the way you think it does.
        # return s[1:-1].replace('\\"', '"')

    def const_nil(self, args):
        return None

    def const_false(self, args):
        return False

    def const_true(self, args):
        return True

    def STRING(self, args):
        # args = lark.lexer.Token
        # .type = STRING
        # .value = <the string>
        return args[1:-1]  # Strip leading/trailing `"`

    def float(self, args):
        return float(args[0])

    def integer(self, args):
        return int(args[0])

    def imaginary(self, args):
        return complex(args[0])


ccr_grammar = Lark(grammar, parser="lalr")  # transformer=CCRTransformer())


def parse(code):
    class TreeIndenter(Indenter):
        NL_type = "_NEWLINE"
        OPEN_PAREN_types = []
        CLOSE_PAREN_types = []
        INDENT_type = "_INDENT"
        DEDENT_type = "_DEDENT"
        tab_len = 4

    with open("concrete.lark", "r") as fp:
        grammar = fp.read()

    parser = Lark(
        grammar,
        parser="lalr",
        postlex=TreeIndenter(),
        maybe_placeholders=True,
        transformer=CCRTransformer(),
        propagate_positions=True,
    )

    return parser.parse(code)


from pprint import pprint

if __name__ == "__main__":
    with open("test", "r") as fp:
        code = fp.read()
        # Insert a trailing "\n" for parsing ease
        code += "\n"

        pprint(parse(code))  # .pretty())


from enum import Enum


class PARSE_ENUM(Enum):
    END_OF_FN_ARGS = 1
    END_OF_STMTS = 2
