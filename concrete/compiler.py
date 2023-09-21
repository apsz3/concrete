# ---------- Compilation
from .parser import PARSE_ENUM
from .cc_ast import get_scope_child, walk_stmt_list, walk_stmt_list_base, find_path_to
from .utils import print_debug

def walk(ast, env, scope):
    buf = []
    next = ast[1]
    while True:
        hd = next[0]
        if hd == PARSE_ENUM.END_OF_STMTS:
            break
        compile(hd, buf, env, scope)
        next = next[1]
    return buf

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
        if argslist == (PARSE_ENUM.END_OF_FN_ARGS,):
            break
        argname = argslist[2]
        argslist = argslist[3]  # next arg in list
        nargs += 1
        rev_args.append(argname)

    # Define before compiling body, so we
    # have a target for recursion...
    define_at = get_scope_child(namespace_scope, env)
    define_at[fn_name][
        "code"
    ] = codebuf  # Using this data structure is in place of writing to an ASM file. Also save it for inspection reasons.
    define_at[fn_name]["nargs"] = nargs
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
    if op == "var_decl_assgn":
        _type = stmt[1]  # We don't care about the type here,
        # because we type check beforehand.
        var = stmt[2]
        compile(stmt[3], buf, env, scope)
        emit("pid", var, buf=buf)
        emit("var_assgn", buf=buf)
    elif op == "var_assgn":
        var = stmt[1]
        compile(stmt[2], buf, env, scope)
        emit("pid", var, buf=buf)
        emit("var_assgn", buf=buf)
    elif op == "var_decl":
        var = stmt[2]
        emit("pid", var, buf=buf)
        emit("var_decl", buf=buf)
    elif op == "const_decl_assgn":
        # _type = stmt[1] # We don't care about the type here,
        # # because we type check beforehand.
        # We can Infer the type of the RHS.
        var = stmt[1]
        compile(stmt[2], buf, env, scope)
        emit("pid", var, buf=buf)
        emit("const_assgn", buf=buf)
    elif op == "loadconst":
        type = stmt[1]
        emit(f"p{type}", stmt[2], buf=buf)
    elif op == "loadvar":
        emit("pval", stmt[1], buf=buf)
    elif op == "print":
        compile(stmt[1], buf, env, scope)
        emit("print", buf=buf)
    elif op == "neg":
        compile(stmt[1], buf, env, scope)
        emit("neg", buf=buf)
    elif op in ["add", "sub", "mul", "div", "gt", "gte", "lt", "lte", "eq", "neq"]:
        compile(stmt[1], buf, env, scope)
        compile(stmt[2], buf, env, scope)
        emit(op, buf=buf)
    elif op == "if":
        # TODO: USING AN ARGUMENT ON THE FUNCTION CALL STACK
        # WILL ISSUE NEW INSTRS ON COMPILATION
        # AND WILL CHANGE THE POSITION OF THE POINTER?
        compile(stmt[1], buf, env, scope)
        pos = len(buf)  # current location
        emit("jmp_if_false", None, buf=buf)
        for s in walk_stmt_list_base(stmt[2]):
            compile(s, buf, env, scope)
        cur_pos = len(buf)
        buf[pos] = ("jmp_if_false", cur_pos)  # Backpatch

    elif op == "fun_def":
        # eventually, we will need to write to a <label> in the ASM
        # file. But parsing the ASM file requires an assembler,
        # and we leave that for later, specifically for when we map to
        # Python bytecode, or decide to do another implementation.
        compile_fn(stmt, buf, env, scope)
    elif op == "return":
        compile(stmt[1], buf, env, scope)  # expr
        emit(
            "ret", buf=buf
        )  # We would ideally want this to be sidelined with the compiled expr above, instead of always having to pop etc...
    elif op == "call":
        # Push args onto stack.
        rev_args = []
        fn_name = stmt[1]
        fn_call_args = stmt[2]
        # NOTE: our namespace searching is NOT implemented yet.
        # fn_obj = fn_lookup(fn_name, scope)
        rev_args = []  # have to group these
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
        print_debug(f"Entering {scope}:{fn_name}")
        path_to = find_path_to(fn_name, env, scope)
        emit("call", f"{path_to}.{fn_name}", buf=buf)
    else:
        assert False, f"Unhandled AST node {op}"



