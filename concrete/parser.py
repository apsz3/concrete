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


# ccr_grammar = Lark(grammar, parser='lalr')# transformer=CCRTransformer())


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


class CCRParser(Parser):
    # TODO: MUCH BETTER ERROR PRINTING https://sly.readthedocs.io/en/latest/sly.html#recovery-and-resynchronization-with-error-rules
    debugfile = "parser.out"
    tokens = CCRLexer.tokens

    # TODO: this parse error thing might be global....
    HAS_PARSER_ERROR = False
    precedence = (
        ("nonassoc", "BOP"),
        ("right", "UMINUS"),
        ("left", "+", "-"),
        ("left", "*", "/"),
    )

    def __init__(self, text):
        self.index = 0
        self.lineno = 0
        self.text = text

    def error(self, t):
        last_cr = self.text.rfind("\n", 0, t.index)
        if last_cr < 0:
            last_cr = 0
        next_cr = self.text.find("\n", t.index)
        if next_cr < 0:
            next_cr = len(self.text) - t.index

        print(self.text[last_cr : t.index + (next_cr - t.index)])
        print("^" * (t.index + (next_cr - t.index) - last_cr))
        print(f"Bad expression {t.value[0]} on {t.lineno}")
        self.HAS_PARSER_ERROR = True

    @_("statement_list")
    def s(self, p):
        return p

    @_("expr statement_list")
    def statement_list(self, p):
        ls = p.statement_list
        if ls:
            return (p.expr, ls)
        return (p.expr, (PARSE_ENUM.END_OF_STMTS,))

    # You don't actually need statement terminators!
    @_("statement statement_list")
    def statement_list(self, p):
        ls = p.statement_list
        if ls:
            return (p.statement, ls)
        return (p.statement, (PARSE_ENUM.END_OF_STMTS,))

    @_("")
    def statement_list(self, p):
        # TODO: need a better ret val
        return None

    @_('TYPE ID "=" expr')
    def statement(self, p):
        # Assignment with initialization=
        return ("var_decl_assgn", p.TYPE, p.ID, p.expr, (p.index, p.lineno))

    @_('CONST ID "=" expr')
    def statement(self, p):
        # Assignment with initialization=
        return ("const_decl_assgn", p.ID, p.expr, (p.index, p.lineno))

    @_("TYPE ID")
    def statement(self, p):
        # Declaration
        return ("var_decl", p.TYPE, p.ID, (p.index, p.lineno))

    @_('ID "=" expr')
    def statement(self, p):
        # Assignment
        return ("var_assgn", p.ID, p.expr, (p.index, p.lineno))

    @_("RETURN expr")
    def statement(self, p):
        return ("return", p.expr, (p.index, p.lineno))

    @_("PRINT expr")
    def statement(self, p):
        return ("print", p.expr, (p.index, p.lineno))

    @_('FUN ID "(" fn_arglist ")" FUN_RET_SYNTAX TYPE "{" fn_body "}"')
    def statement(self, p):
        # Function def
        return ("fun_def", p.TYPE, p.ID, p.fn_arglist, p.fn_body, (p.index, p.lineno))

    @_("TYPE ID fn_arglist")
    def fn_arglist(self, p):
        # Match a list of args delimd by ',', or a single one,
        # or an arg with a trailing comma (with the subsequent arg the empty arg)
        return ("fun_arg", p.TYPE, p.ID, p.fn_arglist, (p.index, p.lineno))

    @_('"," fn_arglist')
    def fn_arglist(self, p):
        # The comma is really just syntactic
        return p.fn_arglist

    @_("")
    def fn_arglist(self, p):
        # Keep list type like the args are so we can 'iterate' over it
        # before breaking
        return (PARSE_ENUM.END_OF_FN_ARGS,)

    @_("statement_list")
    def fn_body(self, p):
        if p.statement_list is None:
            return ("fn_body", (PARSE_ENUM.END_OF_STMTS,))
        return ("fn_body", p.statement_list)

    @_('ID "(" fn_call_args ")"')
    def call(self, p):
        return ("call", p.ID, ("fn_call_args", p.fn_call_args))

    @_("expr fn_call_args")
    def fn_call_args(self, p):
        return (p.expr, p.fn_call_args)

    @_("',' fn_call_args")
    def fn_call_args(self, p):
        return p.fn_call_args

    @_("")
    def fn_call_args(self, p):
        return (PARSE_ENUM.END_OF_FN_ARGS,)

    @_("IF expr statement_list END")
    def expr(self, p):
        if p.statement_list is None:
            return ("if", p.expr, (PARSE_ENUM.END_OF_STMTS,))
        return ("if", p.expr, (p.statement_list,))

    @_("IF expr statement_list ELSE statement_list END")
    def expr(self, p):
        l0 = (
            (p.statement_list0,)
            if p.statement_list0 is not None
            else (PARSE_ENUM.END_OF_STMTS,)
        )
        l1 = (
            (p.statement_list1,)
            if p.statement_list1 is not None
            else (PARSE_ENUM.END_OF_STMTS,)
        )
        return ("ifelse", p.expr, l0, l1)

    @_("call")
    def expr(self, p):
        return p.call

    @_('expr "+" expr')
    def expr(self, p):
        return ("add", p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr "-" expr')
    def expr(self, p):
        return ("sub", p.expr0, p.expr1, (p.index, p.lineno))

    @_('"-" expr %prec UMINUS')
    def expr(self, p):
        return ("neg", p.expr, (p.index, p.lineno))

    @_('expr "*" expr')
    def expr(self, p):
        return ("mul", p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr "/" expr')
    def expr(self, p):
        return ("div", p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr ">" expr')
    def expr(self, p):
        return ("gt", p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr "<" expr')
    def expr(self, p):
        return ("lt", p.expr0, p.expr1, (p.index, p.lineno))

    @_("expr BOP expr")
    def expr(self, p):
        op = None
        if p.BOP == "==":
            op = "eq"
        elif p.BOP == ">=":
            op = "gte"
        elif p.BOP == "<=":
            op = "lte"
        elif p.BOP == "!=":
            op = "neq"
        if op is None:
            self.error(p)
        return (op, p.expr0, p.expr1, (p.index, p.lineno))

    @_('"(" expr ")"')
    def expr(self, p):
        return p.expr

    @_("NUMBER")
    def expr(self, p):
        return ("loadconst", "num", p.NUMBER, ((p.index, p.lineno)))

    @_("BOOLEAN")
    def expr(self, p):
        return ("loadconst", "bool", p.BOOLEAN, ((p.index, p.lineno)))

    @_("STRING")
    def expr(self, p):
        return ("loadconst", "str", p.STRING, (p.index, p.lineno))  # strip delimiters

    @_("ID")
    def expr(self, p):
        return ("loadvar", p.ID, (p.index, p.lineno))
