#!/usr/local/bin/python3

from enum import Enum
from os import name

from sly import Lexer, Parser

# ---------- Lexer
HAS_LEXER_ERROR = False
class CCRLexer(Lexer):
    tokens = {  TYPE, BOOLEAN, ID, NUMBER, STRING, FUN, BOP, RETURN, FUN_RET_SYNTAX, CONST}
    ignore = ' \t'
    BOP = r">=|<=|==|!="
    BOOLEAN = r"true|false"
    literals = { '!', '=', ',', '+', '-', '*', '/', '(', ')' , '[', ']', ';', '"', "'", "{", "}", ">", "<",}

    STRING = r'".*"'
    FUN = r"fun"
    RETURN = r"return"
    CONST = r"const"

    NUMBER = r'\d+|0x[0-9a-fA-F]+'
    FUN_RET_SYNTAX = r"\->"



    TYPE = r'num|str|fun|void|bool' # MUST MATCH THESE FIRST BEFORE ID
    # NOTE: this also messes things up and means we can't prefix
    # things with a type name ?
    ID = r'[a-zA-Z_][a-zA-Z0-9_]*' # no leading numerics

    ignore_comment = r'\#.*'

    @_(r'\n+')
    def newline(self, t):
        self.lineno += t.value.count('\n')

    def error(self, t):
        global HAS_LEXER_ERROR
        last_cr = TEXT.rfind('\n', 0, t.index)
        if last_cr < 0:
            last_cr = 0
        next_cr = TEXT.find('\n', t.index)
        if next_cr < 0:
            next_cr = len(TEXT) - t.index

        print(TEXT[last_cr:t.index+(next_cr-t.index)])
        print("^"*(t.index+(next_cr-t.index)-last_cr))
        print(f"Illegal character {t.value[0]} on line {self.lineno}")

        self.index += 1 # Keep going with the lexing, even with the error
        HAS_LEXER_ERROR = True

# Compute column.
#     input is the input text string
#     idx is the index of the token from lexer
def find_column(text, idx):
    last_cr = text.rfind('\n', 0, idx)
    if last_cr < 0:
        last_cr = 0
    column = (idx - last_cr) + 1
    return column

# ---------- Parser
HAS_PARSER_ERROR = False
class CCRParser(Parser):
    tokens = CCRLexer.tokens

    precedence = (
        ('right', 'UMINUS'),
        ('left', 'BOP' ),

        ('left', '+', '-'),
        ('left', '*', '/'),

        )

    def __init__(self):
        self.index = 0
        self.lineno = 0

    def error(self, t):
        global HAS_PARSER_ERROR
        last_cr = TEXT.rfind('\n', 0, t.index)
        if last_cr < 0:
            last_cr = 0
        next_cr = TEXT.find('\n', t.index)
        if next_cr < 0:
            next_cr = len(TEXT) - t.index

        print(TEXT[last_cr:t.index+(next_cr-t.index)])
        print("^"*(t.index+(next_cr-t.index)-last_cr))
        print(f"Bad expression {t.value[0]} on {t.lineno}")
        HAS_PARSER_ERROR = True

    @_('statement_list')
    def s(self, p):
        return p

    # You don't actually need statement terminators!
    @_('statement statement_list')
    def statement_list(self, p):
        ls = p.statement_list
        if ls:
            return (p.statement, ls)
        return (p.statement, (ENUM.END_OF_STMTS,))

    @_('')
    def statement_list(self, p):
        # TODO: need a better ret val
        return None

    @_('TYPE ID "=" expr')
    def statement(self, p):
        # Assignment with initialization=
        return ('var_decl_assgn', p.TYPE, p.ID, p.expr, (p.index, p.lineno))

    @_('CONST ID "=" expr')
    def statement(self, p):
        # Assignment with initialization=
        return ('const_decl_assgn', p.ID, p.expr, (p.index, p.lineno))

    @_('TYPE ID')
    def statement(self, p):
        # Declaration
        return ('var_decl', p.TYPE, p.ID, (p.index, p.lineno))

    @_('ID "=" expr')
    def statement(self, p):
        # Assignment
        return ('var_assgn', p.ID, p.expr, (p.index, p.lineno))

    @_('RETURN expr')
    def statement(self, p):
        return ('return', p.expr, (p.index, p.lineno))

    @_('FUN ID "(" fn_arglist ")" FUN_RET_SYNTAX TYPE "{" fn_body "}"')
    def statement(self, p):
        # Function def
        return ('fun_def', p.TYPE, p.ID, p.fn_arglist, p.fn_body, (p.index, p.lineno))

    @_('TYPE ID fn_arglist')
    def fn_arglist(self, p):
        # Match a list of args delimd by ',', or a single one,
        # or an arg with a trailing comma (with the subsequent arg the empty arg)
        return ('fun_arg', p.TYPE, p.ID, p.fn_arglist, (p.index, p.lineno))
    @_('"," fn_arglist')
    def fn_arglist(self, p):
        # The comma is really just syntactic
        return p.fn_arglist
    @_("")
    def fn_arglist(self, p):
        # Keep list type like the args are so we can 'iterate' over it
        # before breaking
        return (ENUM.END_OF_FN_ARGS,)

    @_("statement_list")
    def fn_body(self, p):
        if p.statement_list is None:
            return ('fn_body', (ENUM.END_OF_STMTS,))
        return ('fn_body', p.statement_list)



    @_('ID "(" fn_call_args ")"')
    def call(self, p):
        return ('call', p.ID, ('fn_call_args', p.fn_call_args))

    @_("expr fn_call_args")
    def fn_call_args(self, p):
        return (p.expr, p.fn_call_args)
    @_("',' fn_call_args")
    def fn_call_args(self, p):
        return p.fn_call_args

    @_("")
    def fn_call_args(self, p):
        return  (ENUM.END_OF_FN_ARGS,)

    @_('call')
    def expr(self, p):
        return p.call

    @_('"-" expr %prec UMINUS')
    def expr(self, p):
        return ('neg', p.expr, (p.index, p.lineno))

    @_('expr "+" expr')
    def expr(self, p):
        return ('add', p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr "-" expr')
    def expr(self, p):
        return ('sub', p.expr0, p.expr1, (p.index, p.lineno))


    @_('expr "*" expr')
    def expr(self, p):
        return ('mul', p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr "/" expr')
    def expr(self, p):
        return ('div', p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr ">" expr')
    def expr(self, p):
        return ('gt', p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr "<" expr')
    def expr(self, p):
        return ('lt', p.expr0, p.expr1, (p.index, p.lineno))

    @_('expr BOP expr')
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

    @_('NUMBER')
    def expr(self, p):
        return ('loadconst', 'num', p.NUMBER, ((p.index, p.lineno)))

    @_('BOOLEAN')
    def expr(self, p):
        return ('loadconst', 'bool', p.BOOLEAN, ((p.index, p.lineno)))

    @_('STRING')
    def expr(self, p):
        return ('loadconst', 'str', p.STRING, (p.index, p.lineno)) # strip delimiters
    @_('ID')
    def expr(self, p):
        return ('loadvar', p.ID, (p.index, p.lineno))

# ---------- Compilation


class ENUM(Enum):
    UNINIT_T_NUM = 0
    UNINIT_T_STR = 1
    UNINIT_T_BOOL = 2
    UNDECL_VAR = 3
    INVALID_TYPE = 4
    UNINIT_VAL = 5 # For declared vars not yet initialized. Should probably use another method / datastructure for 'setting' this 'value', e.g., we need to check that the name is defined, then assign it; we shoulnd't keep it with the already defined vars.
    END_OF_FN_ARGS = 6
    END_OF_STMTS = 7
    CONST=8

UNINIT_TYPE_MAP = {
    "num": ENUM.UNINIT_T_NUM,
    "str": ENUM.UNINIT_T_STR,
    "bool": ENUM.UNINIT_T_BOOL,
}
REV_TYPE_MAP = {
    v: k for k, v in UNINIT_TYPE_MAP.items()
}


# Contains environment names, which are prefixed with
# __<name>__.
# Fn names are regular.
# Top-level one is always __module__ -- this modules scope.
def emit(*args, buf):
    buf.append(args)

def compile_fn(node, buf, env, namespace_scope):
    # first,
    codebuf = []
    # record the n_args in the environment.
    fn_name = node[2]
    argslist = node[3]
    fn_body = node[4]
    # Arg setup assumes that the values are pushed onto the stack
    # when calling.
    nargs = 0
    rev_args = []
    while True:
        if argslist == (ENUM.END_OF_FN_ARGS,):
            break
        argname = argslist[2]
        argslist = argslist[3] # next arg in list
        nargs += 1
        rev_args.append(argname)

    # Define before compiling body, so we
    # have a target for recursion...
    define_at = get_scope_child(namespace_scope, env)
    define_at[fn_name]["code"] = codebuf # Using this data structure is in place of writing to an ASM file. Also save it for inspection reasons.
    define_at[fn_name]['nargs'] = nargs
    for stmt in walk_stmt_list(fn_body):
        compile(stmt, codebuf, env, f"{namespace_scope}.{fn_name}")

def compile(stmt, buf, env, scope):
    # NOTE: though we never actually change 'scope' in the body
    # of this function, it IS changed when we do function definitions / nesting.
    # So, we do not kwarg default it.
    # RHS will always be an expr according to well-typed rules.
    # TODO: declare things as (expr) in the AST
    # RHS will always be an expr for assgn
    op = stmt[0]
    if op == 'var_decl_assgn':
        _type = stmt[1] # We don't care about the type here,
        # because we type check beforehand.
        var = stmt[2]
        compile(stmt[3], buf, env, scope)
        emit('pid', var, buf=buf)
        emit('var_assgn', buf=buf)
    elif op == 'var_assgn':
        var = stmt[1]
        compile(stmt[2], buf, env, scope)
        emit('pid', var, buf=buf)
        emit('var_assgn', buf=buf)
    elif op == 'var_decl':
        var = stmt[2]
        emit('pid', var, buf=buf)
        emit('var_decl', buf=buf)
    elif op == 'const_decl_assgn':
        # _type = stmt[1] # We don't care about the type here,
        # # because we type check beforehand.
        # We can Infer the type of the RHS.
        var = stmt[1]
        compile(stmt[2], buf, env, scope)
        emit('pid', var, buf=buf)
        emit('const_assgn', buf=buf)
    elif op == 'loadconst':
        type = stmt[1]
        emit(f'p{type}', stmt[2], buf=buf)
    elif op == 'loadvar':
        emit('pval', stmt[1], buf=buf)
    elif op == 'neg':
        compile(stmt[1], buf, env, scope)
        emit('neg', buf=buf)
    elif op in ['add', 'sub', 'mul', 'div',
                'gt', 'gte', 'lt', 'lte', 'eq', 'neq']:
        compile(stmt[1], buf, env, scope)
        compile(stmt[2], buf, env, scope)
        emit(op, buf=buf)
    elif op == 'fun_def':
        # eventually, we will need to write to a <label> in the ASM
        # file. But parsing the ASM file requires an assembler,
        # and we leave that for later, specifically for when we map to
        # Python bytecode, or decide to do another implementation.
        compile_fn(stmt, buf, env, scope)
    elif op == "return":
        compile(stmt[1],buf, env, scope) # expr
        emit("ret", buf=buf) # We would ideally want this to be sidelined with the compiled expr above, instead of always having to pop etc...
    elif op == 'call':
        # Push args onto stack.
        rev_args=[]
        fn_name = stmt[1]
        fn_call_args = stmt[2]
        # NOTE: our namespace searching is NOT implemented yet.
        #fn_obj = fn_lookup(fn_name, scope)
        rev_args = [] # have to group these
        # into lists, so we can properly shuffle them
        # around, and then flatten them into a coherent series of instrs.
        for arg in walk_stmt_list(fn_call_args):
            this_arg_buffer = []
            compile(arg, this_arg_buffer, env, scope)
            rev_args.append(this_arg_buffer)
        rev_args.reverse()
        # Cumulatively these instrs
        # construct the arguments we pass
        # into the function call.
        for arg_group in rev_args:
            for instr in arg_group:
                # instr is a tuple, we need to unpack it
                # to fit with emit's expected args
                emit(*instr, buf=buf)

        # emit the fully qualified path name
        # so we don't need to know context when executing.
        print(scope, fn_name)
        path_to = find_path_to(fn_name, env, scope)
        emit("call", f"{path_to}.{fn_name}", buf=buf)
    else:
        assert False, f"Unhandled AST node {op}"

# ---------- Module / scope resolution
def get_scope_child(key, env):
    # __foo__.__bar__.<fnmame>
    keys = key.split(".")
    if len(keys) == 1:
        return env[key]
    hd = env
    for k in keys:
        hd = hd[k]
    return hd
def find_outermost_name(name, env, namespace):
    # Look through progressively outer scopes,
    # until we find a match on the name, or none.
    # We essentially walk backwards, taking the first
    # N steps each time, starting from the back.
    namespace_components = namespace.split(".")
    hd = env
    found = []
    for ns in namespace_components:
        hd = hd[ns]
        # TODO: this conflates a local with a function.
        # We don't want overwriting.
        # But the inner scope can't know which of the
        # outer scopes to use.
        # Therefore, we have to restrict function names
        # in their definitions, if such a name already
        # exists in the current env.
        if hd.get(name, None) is not None:
            found.append(hd.get(name))
        elif 'locals' in hd and hd.get('locals').get(name, None) is not None:
            found.append( hd.get('locals').get(name))
    if found:
        return found[-1]
    return None
def find_path_to(name, env, namespace):
    # Look through progressively outer scopes,
    # until we find a match on the name, or none.
    # We essentially walk backwards, taking the first
    # N steps each time, starting from the back.
    namespace_components = namespace.split(".")
    hd = env
    found = []
    for i, ns in enumerate(namespace_components):
        hd = hd[ns]
        # TODO: this conflates a local with a function.
        # We don't want overwriting.
        # But the inner scope can't know which of the
        # outer scopes to use.
        # Therefore, we have to restrict function names
        # in their definitions, if such a name already
        # exists in the current env.
        if name in hd:
            found.append('.'.join(namespace_components[:i+1]))
        elif 'locals' in hd and 'name' in hd['locals']:
            found.append('.'.join(namespace_components[:i+1]))
    if found:
        return '.'.join(found)
    return None

# ---------- Type checking
def check_fun(node, env, namespace_scope="__module__"):
    ret_t = node[1]
    fn_name = node[2]
    argslist = node[3]
    body = node[4]
    # First, add to env, so we can have recursive calls
    scope = {}
    scope.update(env) # Copy it, so we can overwrite lexically
    # Set locals and arglist to empty, overwriting
    # outer scope, before preceeding to
    # write to its own locals
    scope = {"ret": ret_t, "arglist": [], "locals": {}}
    child_scope = get_scope_child(f"{namespace_scope}", env)
    child_scope[fn_name] = scope # Save the fn scope in the env.

    HAVE_RETURN = False
    while True:
        if argslist == (ENUM.END_OF_FN_ARGS,):
            break
        argname = argslist[2]
        arg_t = argslist[1]
        argslist = argslist[3] # next arg in list
        # Add fn args to scope
        scope["arglist"].append((argname, arg_t)) # save this so we can look up its type.
        # Also overwrite any bindings in the body
        scope["locals"][argname] = arg_t

    type_err = False
    for child_node in walk_stmt_list(body):
        # TODO: write child values to 'locals' env.
        t = check(child_node, env, scope=f"{namespace_scope}.{fn_name}")
        if t == ENUM.INVALID_TYPE:
            # The type error should be printed in the sub-call;
            # no need to print it here, unless we wanted to 'capture'
            # the scope, such as saying, "in function x:"
            # ...
            # We should have type errors return their message
            # to caller. If the outer scope (the env) is global,
            # print it; otherwise, return it as a msg to the outer scope.
            type_err = True
        if child_node[0] == 'return':
            HAVE_RETURN = True
            # Could also detect dead code this way.
            if t != ret_t:
                _idx, _lineno = child_node[2]; col = find_column(TEXT, _idx)
                type_err= True
                print(f"TypeError:{_lineno}:{col} Function {fn_name} of return type {ret_t} but got expression of {t}")
    # TODO: we need to find that the type of the
    # last / returned stmt is valid,
    # else we complain.
    if not HAVE_RETURN and ret_t != 'void':
        print(f"TypeError: function {fn_name} requires return statement in non-void function")
        return ENUM.INVALID_TYPE
    return ENUM.INVALID_TYPE if type_err else ret_t

def walk_stmt_list(ast):
    # Start at ast[1] because ast[0] will be the
    # opcode that is followed by the statement list.
    next = ast[1]
    if next is None:
        return
    hd = next[0]
    while True:
        if hd == ENUM.END_OF_STMTS:
            return
        elif hd == ENUM.END_OF_FN_ARGS:
            return
        if type(hd) is not tuple:
            assert False, "Shouldn't have ended here -- need end of stmts"

        yield hd
        next = next[1]
        hd = next[0]

def check(node, env, scope="__module__"):
    # Check the head of the ast, determining its type validity
    # This can involve recursively checking component of it,
    # for example the RHS of the assignment.
    #print(node, env)
    op = node[0]
#    print(node, env)
    # Dig down into the scope we're at,
    # then obtain 'locals'.
    # This way we never leave the top-level scope.
    # More nesting would be
    # __module__.foo.bar etc.
    cur_env = get_scope_child(scope, env) # todo -- i think this is just
    # a convenience for not having to resolve scope in every opcode process.
    # in reality, on its owen, its a convenience, we shoulnd't be using it like it is.
    # Basically the function frame

    locals=cur_env.get("locals", {})

    if op in ['var_decl_assgn', 'var_decl', 'const_decl_assgn'] and node[2] in locals:
        # TODO: seems redundant.
        print(f"TypeError: Cannot redefine {node[2]} -- already defined")
        return ENUM.INVALID_TYPE
    if op == 'var_decl':
        type_var = node[1]
        var_name = node[2]
        _idx, _lineno = node[3]; col = find_column(TEXT, _idx)
        if locals.get(var_name, None) is not None and locals[var_name] != type_var:
            print(f"TypeError:{_lineno}:{col} Cannot re-define {var_name} of {locals[var_name]} to new type {type_var}")
            return ENUM.INVALID_TYPE
        type_var = UNINIT_TYPE_MAP[type_var]
        locals[var_name] = type_var # Set it in the env so we can check for it being declared already
        return type_var # Don't return the uninit thing -- we need to check it
    elif op == 'var_assgn':
        # Already declared
        var_name = node[1]
        type_var = locals.get(var_name, ENUM.UNDECL_VAR)
        _idx, _lineno = node[3]; col = find_column(TEXT, _idx)

        if type_var == ENUM.UNDECL_VAR:
            print(f"TypeError:{_lineno}:{col} {var_name} not defined")
            return ENUM.INVALID_TYPE
        # We are assigning it now, so check if its 'declared' type
        # would match the other expression.
        # If we have previously assigned it, and are simply
        # re-assigning the value (our language is mutable),
        # proceed with the original type.
        type_var = REV_TYPE_MAP.get(type_var, type_var)
        type_val = check(node[2], env, scope)
        if type_var != type_val:
            print(f"TypeError:{_lineno}:{col} Cannot assign {var_name} of {type_var} to expression of type {type_val}")
            return ENUM.INVALID_TYPE
        locals[var_name] = type_var # It's initialized now, we're ok. Set it to
        # the _key_ matching the uninitialized type.
        return type_var
    elif op == 'var_decl_assgn':
        # TODO: assgn should probably group (T, varname) in a tuple,
        # not flattened as it is now.
        type_var = node[1]
        type_val = check(node[3], env, scope) # If this type is invalid, we do not actually get to see the 'type' of the erroneous node, because its own check is None (ENUM.invalid_type)... we need a type() function that doesn't check.
        _idx, _lineno = node[4]; col = find_column(TEXT, _idx)
        if type_var != type_val:
            print(f"TypeError:{_lineno}:{col} Cannot assign {node[2]} of {type_var} to expression of type {type_val}")
            return ENUM.INVALID_TYPE
        locals[node[2]] = type_var
        return type_var
    elif op == 'const_decl_assgn':
        # this will currently not support declaring the type
        # of the const.
        # one could check the arity of the nodes though
        _idx, _lineno = node[3]; col = find_column(TEXT, _idx)

        op_val = node[2][0] # the operation; must be loadconst
        if op_val != "loadconst":
            print(f"TypeError:{_lineno}:{col} Cannot assign {node[2]} of constant type to non-constant expression")
            return ENUM.INVALID_TYPE
        type_val = check(node[2], env, scope)
        _idx, _lineno = node[3]; col = find_column(TEXT, _idx)
        locals[node[1]] = type_val
        return type_val

        # Typecheck, then add it to the env, for catching future errors too.
    elif op == 'loadconst':
        return node[1]
    elif op == 'loadvar':
        # This var could be in locals, or the greater scope.
        # As an optimization, we check the LOCAL scope first,
        # before handing it off to the full outermost searcher.
        val = locals.get(node[1], None)
        if val is None:
            val = find_outermost_name(node[1], env, scope) or None
            if val == None:
                print(f"TypeError:{node[1]} not defined")
                return ENUM.INVALID_TYPE
        _idx, _lineno = node[2]; col = find_column(TEXT, _idx)
        # Check that this value is not uninitialized.
        if val in UNINIT_TYPE_MAP.values():
            print(f"TypeError:{_lineno}:{col} Attempted to use uninitialized var {node[1]} in expression")
            return ENUM.INVALID_TYPE
        return val
    elif op in ['neg']:
        a = node[1]
        _idx, _lineno = node[2]
        t_a = check(a, env, scope)
        if t_a != 'num':
            print(f"TypeError:{_lineno}: Unary negation requires a numeric type. Got {t_a}")
            return ENUM.INVALID_TYPE
        return t_a

    elif op in ['add', 'sub', 'mul', 'div']:
        a, b = node[1], node[2]
        _idx, _lineno = node[3]

        # We cannot reliably guess the index of the info here,
        # because the nodes could be compounds, etc.
        # TODO: we need to class this with attributes.
        t_a, t_b = check(a, env, scope), check(b, env, scope)
        if t_a != t_b:
            print(f"TypeError:{_lineno}: {op} requires {a} and {b} of same type. Got {t_a} and {t_b}")
            return ENUM.INVALID_TYPE
        return t_a # Same as b; and axiom is that these ops return the type of their subtypes.
    elif op in ['gt', 'gte', 'eq', 'neq', 'lte', 'lt']:
        a, b = node[1], node[2]
        _idx, _lineno = node[3]
        # We cannot reliably guess the index of the info here,
        # because the nodes could be compounds, etc.
        # TODO: we need to class this with attributes.
        t_a, t_b = check(a, env, scope), check(b, env, scope)
        if t_a != t_b:
            print(f"TypeError:{_lineno}: {op} requires {a} and {b} of same type. Got {t_a} and {t_b}")
            return ENUM.INVALID_TYPE
        #NOTE: TODO: We have the unfortunate situation where
        # we actually return the string type (`t_a` in many situations)
        # rather than the ENUM type. Thus, we return `bool` here,
        # not ENUM.UNINIT_T_BOOL.
        # We should change these types to return non-strings.
        return "bool" # Returns a BOOLEAM
    elif op == 'fun_def':
        return check_fun(node, env, scope)
    elif op == 'return':
        return check(node[1], env, scope)
    elif op == ENUM.END_OF_STMTS:
        return True
    elif op == 'call':
        # Check that the args on the stack
        # match those expected by the function.
        # This includes cardinality check, by virtue
        # of iterating over them.
        # First, check that each
        # type in the fn_call_args
        # matches (in order)
        # the type we know about from env.
        fn_name = node[1]
        fn_args = node[2]
        # Preserve the 'outer' env when we pass it into check()
        referred_scope = find_outermost_name(fn_name, env, scope)
        if referred_scope is None:
            print(f"TypeError: unable to find function {fn_name} in any scope.")
            return ENUM.INVALID_TYPE
        path_to = find_path_to(fn_name, env, scope)

        arg_checks = [check(arg, env, scope=f"{path_to}.{fn_name}") for arg in walk_stmt_list(fn_args)]
        # but we must use the 'nested' env for the arglist check
        # etc. CUR_ENV is the environment where FN_NAME
        # is a TOP-LEVEL key!
        # This lets us access context.
        if len(arg_checks) != len(referred_scope["arglist"]):
            print(f"TypeError:{fn_name}: expected {len(referred_scope['arglist'])} args, got {len(arg_checks)}")
            return ENUM.INVALID_TYPE
        for (t_actual,(arg_name,t_expected)) in zip(arg_checks, referred_scope["arglist"]):
            if t_actual != t_expected:
                print(f"TypeError:{fn_name}: Expected {t_expected} for {arg_name}; got {t_actual}")
                return ENUM.INVALID_TYPE
        return referred_scope["ret"] # The final value is the return type.
    assert False, f"Didn't process all types. {node}"
def check_ast(ast, env):
    next = ast[1]
    valid = True
    while True:
        hd = next[0]
        if type(hd) is not tuple:
            # No more lists in the s-expr.
            if check(next, env) == ENUM.INVALID_TYPE:
                valid = False
            break
        if check(hd, env)== ENUM.INVALID_TYPE:
            # Note the different pattern
            # from the walk.
            valid = False
        next = next[1]
    return valid
def walk(ast, env, scope):
    buf = []
    next = ast[1]
    while True:
        hd = next[0]
        if hd == ENUM.END_OF_STMTS:
            break
        compile(hd, buf, env, scope)
        next = next[1]
    return buf

# ---------- VM
def vm(code, compilation_env):
    # Note: this will be rewritten in C / ported.
    # (ip, max_ip)
    call_stack_ptrs = [(0, code, [])]
    symbol_table = {} # this is a RUNTIME object.
    print("########### RUNTIME")
    while call_stack_ptrs:
        print("#### Frame")
        ip,code,stack= call_stack_ptrs.pop()
        while ip < len(code):
            instr = code[ip]
            print(instr)
            ip += 1
            op = instr[0]
            if op == 'pstr':
                stack.append(instr[1].strip('"'))
            if op == 'pnum':
                stack.append(float(instr[1]))
            if op == 'pbool':
                stack.append(instr[1] == "true")
            if op == 'pid':
                # For assignments, etc.
                stack.append(instr[1])
            if op == 'pval':
                var = instr[1]
                if symbol_table.get(var, None) is None:
                    raise Exception(f"UNDEFINED VAR {var}")
                    # Error recovery, lets continue
                else:
                    stack.append(symbol_table.get(var))
            if op == 'pop':
                stack.pop()
            if op == 'var_decl':
                var = stack.pop()
                symbol_table[var] = ENUM.UNINIT_VAL # Default to nothing.
            if op == 'var_assgn':


                var = stack.pop()
                val = stack.pop()
                symbol_table[var]=val
            if op == 'const_assgn':

                var = stack.pop()
                val = stack.pop()
                symbol_table[var]=val
            if op == 'add':
                a, b = stack.pop(), stack.pop()
                stack.append(b + a)
            if op == 'mul':
                stack.append(stack.pop()*stack.pop())
            if op == 'sub':
                stack.append(-stack.pop()+stack.pop())
            if op == 'neg':
                stack.append(-1*stack.pop())
            if op == 'eq':
                stack.append(stack.pop() == stack.pop())
            if op == 'neq':
                stack.append(stack.pop() != stack.pop())
            if op == "gt":
                a = stack.pop()
                b = stack.pop()
                stack.append(a > b)
            if op == "gte":
                a = stack.pop()
                b = stack.pop()
                stack.append(a >= b)
            if op == "lt":
                a = stack.pop()
                b = stack.pop()
                stack.append(a < b)
            if op == "lte":
                a = stack.pop()
                b = stack.pop()
                stack.append(a <= b)
            if op == 'call':
                target = instr[1]
                # Get argument information from context
                # TODO: we should be using a TEXTUAL CONTEXT
                # e.g. no data structure here.
                # For now, for PoC, we will figure out
                # labels and jump targets etc later.

                data = get_scope_child(target, env)

                call_stack_ptrs.append((ip, code, stack, symbol_table))
                ip,code = 0,data['code']
                arg_stack_instrs = []
                arg_stack_vals = []
                symbol_table = {}
                # Set up the arguments on the function stack frame
                for arg,_ in data['arglist']:
                    val = stack.pop()
                    arg_stack_vals.append(val)
                    emit('pid', arg, buf=arg_stack_instrs)
                    emit('var_assgn', buf=arg_stack_instrs)

                code = arg_stack_instrs + code
                arg_stack_vals.reverse() # why do i have to do this when i went through all the trouble to do it before?
                stack = arg_stack_vals
                #call_stack_ptrs.append((0, data['code']))
                #break # essentially, JMP, indicating
                # a context switch
            if op == 'ret':
                print(f"Leaving: {symbol_table}")
                data = get_scope_child(target, env)
                if data['ret'] != 'void':
                    # Pop the result, put onto new stack
                    ret_val = stack.pop()
                    ip,code,stack,symbol_table = call_stack_ptrs.pop()
                    stack.append(ret_val)
                else:
                    ip,code,stack,symbol_table = call_stack_ptrs.pop()

    print(stack, symbol_table)
    return stack, symbol_table

# TODO: do NOT define functions in an inner context.
# Send them all up to the top level with __module__.foo.bar.etc.
# With the 'reverse' lookup we do, we will effectively obtain the
# 'overwriting' of lexically-scoped vars that we wanted,
# from when we defined the fn objects nested in a parent one.
with open("demo.ccr", "r") as fp:
    TEXT = fp.read()

if __name__ == '__main__':
    lexer = CCRLexer()
    parser = CCRParser()


    ast = parser.parse(lexer.tokenize(TEXT))
    from pprint import pprint

    print('--------')
    env = {"__module__": {"locals": {}}}
    well_typed = check_ast(ast, env)
#    pprint(ast)
    if not HAS_LEXER_ERROR and not HAS_PARSER_ERROR and well_typed:
        code = walk(ast, env, '__module__') # compiling at the top-level
        pprint(code)
        pprint(env)
        print('--------')
        stack, symbol_table = vm(code, env)
        pprint(env)
        print("--------")
        pprint(symbol_table)
    if not well_typed:
        print("NOT WELL TYPED")
    if HAS_LEXER_ERROR:
        print("LEX ERROR")
    if HAS_PARSER_ERROR:
        print("PARSER ERROR")
