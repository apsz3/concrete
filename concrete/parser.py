# ---------- Parser
# from sly import Parser

# from .lexer import CCRLexer
from .exceptions import HasParserErrorException
from lark import Lark, Transformer, v_args
from lark.indenter import Indenter

Reserved = [
    "fun",
    "if",
    "elif",
    "else",
    "num",
    "str",
    "bool",
    "void",
    "return",
]


def fmt(meta):
    return (meta.column, meta.line)


class CCRTransformer(Transformer):
    def start(self, args):
        # Use this so we can group all the AST nodes
        # into an S-expr for pretty printing.
        return args
        # return s[1:-1].replace('\\"', '"')

    @v_args(inline=True, meta=True)
    def assgn_stmt(self, meta, name, expr):
        return ("assgn_stmt", name.value, expr, fmt(meta))

    @v_args(inline=True, meta=True)
    def decl_stmt(self, meta, args):
        # Index [0] here because we only expect a single
        # input, which is a full name-type-anno
        return ("decl_stmt", args[0], fmt(meta))

    @v_args(inline=True, meta=True)
    def anno_assgn_stmt(self, meta, name_anno, val):
        return ("anno_assgn_stmt", name_anno, val, fmt(meta))

    # @v_args(inline=True)
    @v_args(inline=True, meta=True)
    def if_stmt(self, meta, cond, block_if, *block_elifs):
        # We include else here; it is nothing more than
        # just the final block to process.
        return ("if", cond, block_if, *block_elifs, fmt(meta))
        # return ("if", cond, block_if, block_elifs)

    #   @v_args(meta=True)@v_args(inline=True, inline=True)
    # def else_stmt(self meta,, block):
    #     # Use this so that we can more easily distinguish
    #     # between elifs, which might help us with semantic analysis
    #     return ("else", block)

    @v_args(inline=True, meta=True)
    def elifs(self, meta, cond, block):
        return ("elif", cond, block, fmt(meta))

    @v_args(inline=True, meta=True)
    def inline_if(self, meta, expr_if, cond, *expr_else):
        # Use this so that we can more easily distinguish
        # between elifs, which might help us with semantic analysis
        return ("if", expr_if, cond, *expr_else, fmt(meta))

    @v_args(meta=True, inline=True)
    def flow_stmt(self, meta, args):
        return ("flow_stmt", *args, fmt(meta))  # str(args[0]), fmt(meta))

    @v_args(inline=True, meta=True)
    def block_assgn_stmt(self, meta, name, block_stmts):
        return ("block_assgn_stmt", name.value, block_stmts, fmt(meta))

    @v_args(inline=True, meta=True)
    def block(self, meta, name, *stmts):
        return ("block", name, stmts, fmt(meta))

    @v_args(meta=True, inline=True)
    def return_stmt(self, meta, args):
        return ("return", *args, fmt(meta))

    @v_args(meta=True, inline=True)
    def name_type_anno(
        self,
        meta,
        name,
        type,
    ):
        return ("name_type_anno", name.value, type, fmt(meta))

    @v_args(meta=True, inline=True)
    def type(self, meta, type):
        return ("type", type, fmt(meta))

    @v_args(meta=True, inline=True)
    def type_nullable(
        self,
        meta,
        type,
    ):
        return ("type_nullable", type, fmt(meta))

    @v_args(inline=True, meta=True)
    def fun(self, meta, name, args_list, type_anno, block):
        return ("fun", name.value, args_list, type_anno, block, fmt(meta))

    @v_args(meta=True, inline=True)
    def cs_list(self, meta, *args):
        return (args, fmt(meta))

    # Expressions
    # Operations
    @v_args(inline=True, meta=True)
    def op_binop(self, meta, a, op, b):
        # The op_binop is a Tree() in Lark,
        # which must be accessed via `.data`
        return (op.data, a, b, fmt(meta))

    @v_args(inline=True, meta=True)
    def op_unary(self, meta, op, a):
        # The op_binop is a Tree() in Lark,
        # which must be accessed via `.data`
        return (op.data, a, fmt(meta))

    # Values
    @v_args(inline=True, meta=True)
    def type(self, meta, _t):
        return ("type", _t.value, fmt(meta))

    @v_args(inline=True, meta=True)
    def type_nullable(self, meta, _t):
        return ("type_nullable", _t.value, fmt(meta))

    @v_args(inline=True, meta=True)
    def name_untyped(self, meta, name):
        return ("name_untyped", name.value, fmt(meta))

    #   @v_args(meta=True)@v_args(inline=True, inline=True)
    # def naked_expr(self meta,, name):
    #     # An expression in a statement
    #     return ("naked_expr", name)

    @v_args(inline=True, meta=True)
    def NAME(self, name):
        return name

    @v_args(inline=True, meta=True)
    def string(self, meta, s):
        return s  # This doesnt trim qts the way you think it does.
        # return s[1:-1].replace('\\"', '"')

    @v_args(meta=True, inline=True)
    def const_nil(self, _):
        return None

    @v_args(meta=True, inline=True)
    def const_false(self, _):
        return False

    @v_args(meta=True, inline=True)
    def const_true(self, _):
        return True

    @v_args(meta=True, inline=True)
    def STRING(self, s):
        # args = lark.lexer.Token
        # .type = STRING
        # .value = <the string>
        return s[1:-1]  # Strip leading/trailing `"`

    @v_args(meta=True, inline=True)
    def float(self, meta, f):
        return float(f)

    @v_args(meta=True, inline=True)
    def integer(self, meta, i):
        return int(i)

    @v_args(meta=True, inline=True)
    def imaginary(self, meta, im):
        return (complex(im), meta)


class TreeIndenter(Indenter):
    NL_type = "_NEWLINE"
    OPEN_PAREN_types = []
    CLOSE_PAREN_types = []
    INDENT_type = "_INDENT"
    DEDENT_type = "_DEDENT"
    tab_len = 4


from pathlib import Path

f: Path = Path(__file__).resolve().parent
with (f / "concrete.lark").open("r") as fp:
    _grammar = fp.read()

CCRParser = Lark(
    _grammar,
    parser="lalr",
    postlex=TreeIndenter(),
    maybe_placeholders=True,
    # transformer=CCRTransformer(),
    propagate_positions=True,
)


def parse(s):
    try:
        parse = CCRParser.parse(s)
        ast = CCRTransformer().transform(parse)
        return ast
    except Exception as e:
        raise HasParserErrorException()


from enum import Enum


class PARSE_ENUM(Enum):
    END_OF_FN_ARGS = 1
    END_OF_STMTS = 2
