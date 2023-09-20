from enum import Enum

from .cc_ast import get_scope_child, walk_stmt_list, find_outermost_name, find_path_to
from .utils import find_column
from .parser import PARSE_ENUM

class TYPE(Enum):
    UNINIT_T_NUM = 0
    UNINIT_T_STR = 1
    UNINIT_T_BOOL = 2
    UNDECL_VAR = 3
    INVALID_TYPE = 4
    UNINIT_VAL = 5  # For declared vars not yet initialized. Should probably use another method / datastructure for 'setting' this 'value', e.g., we need to check that the name is defined, then assign it; we shoulnd't keep it with the already defined vars.
    CONST = 7


UNINIT_TYPE_MAP = {
    "num": TYPE.UNINIT_T_NUM,
    "str": TYPE.UNINIT_T_STR,
    "bool": TYPE.UNINIT_T_BOOL,
}
REV_TYPE_MAP = {v: k for k, v in UNINIT_TYPE_MAP.items()}

# ---------- Type checking
class Checker:
    def __init__(self, text) -> None:
        self.TEXT = text

    def check_fun(self, node, env, namespace_scope="__module__"):
        ret_t = node[1]

        fn_name = node[2]
        argslist = node[3]
        body = node[4]
        # First, add to env, so we can have recursive calls
        scope = {}
        scope.update(env)  # Copy it, so we can overwrite lexically
        # Set locals and arglist to empty, overwriting
        # outer scope, before preceeding to
        # write to its own locals
        scope = {"ret": ret_t, "arglist": [], "locals": {}}
        child_scope = get_scope_child(f"{namespace_scope}", env)
        child_scope[fn_name] = scope  # Save the fn scope in the env.

        HAVE_RETURN = False
        while True:
            if argslist == (PARSE_ENUM.END_OF_FN_ARGS,):
                break
            argname = argslist[2]
            arg_t = argslist[1]
            argslist = argslist[3]  # next arg in list
            # Add fn args to scope
            scope["arglist"].append(
                (argname, arg_t)
            )  # save this so we can look up its type.
            # Also overwrite any bindings in the body
            scope["locals"][argname] = arg_t

        type_err = False
        for child_node in walk_stmt_list(body):
            # TODO: write child values to 'locals' env.
            t = self.check(child_node, env, scope=f"{namespace_scope}.{fn_name}")
            if t == TYPE.INVALID_TYPE:
                # The type error should be printed in the sub-call;
                # no need to print it here, unless we wanted to 'capture'
                # the scope, such as saying, "in function x:"
                # ...
                # We should have type errors return their message
                # to caller. If the outer scope (the env) is global,
                # print it; otherwise, return it as a msg to the outer scope.
                type_err = True
            if child_node[0] == "return":
                HAVE_RETURN = True
                # Could also detect dead code this way.
                if t != ret_t:
                    _idx, _lineno = child_node[2]
                    col = find_column(self.text, _idx)
                    type_err = True
                    print(
                        f"TypeError:{_lineno}:{col} Function {fn_name} of return type {ret_t} but got expression of {t}"
                    )
        # TODO: we need to find that the type of the
        # last / returned stmt is valid,
        # else we complain.
        if not HAVE_RETURN and ret_t != "void":
            print(
                f"TypeError: function {fn_name} requires return statement in non-void function"
            )
            return TYPE.INVALID_TYPE
        return TYPE.INVALID_TYPE if type_err else ret_t

    def check(self, node, env, scope="__module__"):
        # Check the head of the ast, determining its type validity
        # This can involve recursively checking component of it,
        # for example the RHS of the assignment.
        # print(node, env)
        op = node[0]
        #    print(node, env)
        # Dig down into the scope we're at,
        # then obtain 'locals'.
        # This way we never leave the top-level scope.
        # More nesting would be
        # __module__.foo.bar etc.
        cur_env = get_scope_child(scope, env)  # todo -- i think this is just
        # a convenience for not having to resolve scope in every opcode process.
        # in reality, on its owen, its a convenience, we shoulnd't be using it like it is.
        # Basically the function frame

        locals = cur_env.get("locals", {})

        if (
            op in ["var_decl_assgn", "var_decl", "const_decl_assgn"]
            and node[2] in locals
        ):
            # TODO: seems redundant.
            print(f"TypeError: Cannot redefine {node[2]} -- already defined")
            return TYPE.INVALID_TYPE

        if op == "if":
            cond_t = self.check(node[1], env)
            if cond_t != "bool":
                print(f"TypeError: Expect boolean value for expression {node[1]}")
                return TYPE.INVALID_TYPE
            expr_t = self.check(node[1], env)
            return expr_t  # If's return the final expr value
        elif op == "var_decl":
            type_var = node[1]
            var_name = node[2]
            _idx, _lineno = node[3]
            col = find_column(self.TEXT, _idx)
            if locals.get(var_name, None) is not None and locals[var_name] != type_var:
                print(
                    f"TypeError:{_lineno}:{col} Cannot re-define {var_name} of {locals[var_name]} to new type {type_var}"
                )
                return TYPE.INVALID_TYPE
            type_var = UNINIT_TYPE_MAP[type_var]
            locals[
                var_name
            ] = type_var  # Set it in the env so we can check for it being declared already
            return type_var  # Don't return the uninit thing -- we need to check it
        elif op == "var_assgn":
            # Already declared
            var_name = node[1]
            type_var = locals.get(var_name, TYPE.UNDECL_VAR)
            _idx, _lineno = node[3]
            col = find_column(self.TEXT, _idx)

            if type_var == TYPE.UNDECL_VAR:
                print(f"TypeError:{_lineno}:{col} {var_name} not defined")
                return TYPE.INVALID_TYPE
            # We are assigning it now, so check if its 'declared' type
            # would match the other expression.
            # If we have previously assigned it, and are simply
            # re-assigning the value (our language is mutable),
            # proceed with the original type.
            type_var = REV_TYPE_MAP.get(type_var, type_var)
            type_val = self.check(node[2], env, scope)
            if type_var != type_val:
                print(
                    f"TypeError:{_lineno}:{col} Cannot assign {var_name} of {type_var} to expression of type {type_val}"
                )
                return TYPE.INVALID_TYPE
            locals[var_name] = type_var  # It's initialized now, we're ok. Set it to
            # the _key_ matching the uninitialized type.
            return type_var
        elif op == "var_decl_assgn":
            # TODO: assgn should probably group (T, varname) in a tuple,
            # not flattened as it is now.
            type_var = node[1]
            type_val = self.check(
                node[3], env, scope
            )  # If this type is invalid, we do not actually get to see the 'type' of the erroneous node, because its own check is None (TYPE.invalid_type)... we need a type() function that doesn't check.
            _idx, _lineno = node[4]
            col = find_column(self.TEXT, _idx)
            if type_var != type_val:
                print(
                    f"TypeError:{_lineno}:{col} Cannot assign {node[2]} of {type_var} to expression of type {type_val}"
                )
                return TYPE.INVALID_TYPE
            locals[node[2]] = type_var
            return type_var
        elif op == "const_decl_assgn":
            # this will currently not support declaring the type
            # of the const.
            # one could check the arity of the nodes though
            _idx, _lineno = node[3]
            col = find_column(self.TEXT, _idx)

            op_val = node[2][0]  # the operation; must be loadconst
            if op_val != "loadconst":
                print(
                    f"TypeError:{_lineno}:{col} Cannot assign {node[2]} of constant type to non-constant expression"
                )
                return TYPE.INVALID_TYPE
            type_val = self.check(node[2], env, scope)
            _idx, _lineno = node[3]
            col = find_column(self.TEXT, _idx)
            locals[node[1]] = type_val
            return type_val

            # Typecheck, then add it to the env, for catching future errors too.
        elif op == "loadconst":
            return node[1]
        elif op == "loadvar":
            # This var could be in locals, or the greater scope.
            # As an optimization, we check the LOCAL scope first,
            # before handing it off to the full outermost searcher.
            val = locals.get(node[1], None)
            if val is None:
                val = find_outermost_name(node[1], env, scope) or None
                if val == None:
                    print(f"TypeError:{node[1]} not defined")
                    return TYPE.INVALID_TYPE
            _idx, _lineno = node[2]
            col = find_column(self.TEXT, _idx)
            # Check that this value is not uninitialized.
            if val in UNINIT_TYPE_MAP.values():
                print(
                    f"TypeError:{_lineno}:{col} Attempted to use uninitialized var {node[1]} in expression"
                )
                return TYPE.INVALID_TYPE
            return val
        elif op in ["neg"]:
            a = node[1]
            _idx, _lineno = node[2]
            t_a = self.check(a, env, scope)
            if t_a != "num":
                print(
                    f"TypeError:{_lineno}: Unary negation requires a numeric type. Got {t_a}"
                )
                return TYPE.INVALID_TYPE
            return t_a

        elif op in ["add", "sub", "mul", "div"]:
            a, b = node[1], node[2]
            _idx, _lineno = node[3]

            # We cannot reliably guess the index of the info here,
            # because the nodes could be compounds, etc.
            # TODO: we need to class this with attributes.
            t_a, t_b = self.check(a, env, scope), self.check(b, env, scope)
            if t_a != t_b:
                print(
                    f"TypeError:{_lineno}: {op} requires {a} and {b} of same type. Got {t_a} and {t_b}"
                )
                return TYPE.INVALID_TYPE
            return t_a  # Same as b; and axiom is that these ops return the type of their subtypes.
        elif op in ["gt", "gte", "eq", "neq", "lte", "lt"]:
            a, b = node[1], node[2]
            _idx, _lineno = node[3]
            # We cannot reliably guess the index of the info here,
            # because the nodes could be compounds, etc.
            # TODO: we need to class this with attributes.
            t_a, t_b = self.check(a, env, scope), self.check(b, env, scope)
            if t_a != t_b:
                print(
                    f"TypeError:{_lineno}: {op} requires {a} and {b} of same type. Got {t_a} and {t_b}"
                )
                return TYPE.INVALID_TYPE
            # NOTE: TODO: We have the unfortunate situation where
            # we actually return the string type (`t_a` in many situations)
            # rather than the TYPE type. Thus, we return `bool` here,
            # not TYPE.UNINIT_T_BOOL.
            # We should change these types to return non-strings.
            return "bool"  # Returns a BOOLEAM
        elif op == "fun_def":
            return self.check_fun(node, env, scope)
        elif op == "return":
            return self.check(node[1], env, scope)
        elif op == PARSE_ENUM.END_OF_STMTS:
            return True
        elif op == "print":
            return True  # Void?
        elif op == "call":
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
                return TYPE.INVALID_TYPE
            path_to = find_path_to(fn_name, env, scope)

            arg_checks = [
                self.check(arg, env, scope=f"{path_to}.{fn_name}")
                for arg in walk_stmt_list(fn_args)
            ]
            # but we must use the 'nested' env for the arglist check
            # etc. CUR_ENV is the environment where FN_NAME
            # is a TOP-LEVEL key!
            # This lets us access context.
            if len(arg_checks) != len(referred_scope["arglist"]):
                print(
                    f"TypeError:{fn_name}: expected {len(referred_scope['arglist'])} args, got {len(arg_checks)}"
                )
                return TYPE.INVALID_TYPE
            for (t_actual, (arg_name, t_expected)) in zip(
                arg_checks, referred_scope["arglist"]
            ):
                if t_actual != t_expected:
                    print(
                        f"TypeError:{fn_name}: Expected {t_expected} for {arg_name}; got {t_actual}"
                    )
                    return TYPE.INVALID_TYPE
            return referred_scope["ret"]  # The final value is the return type.
        assert False, f"Didn't process all types. {node}"


    def check_ast(
        self,
        ast,
        env,
    ):
        next = ast[1]
        valid = True
        while True:
            hd = next[0]
            if type(hd) is not tuple:
                # No more lists in the s-expr.
                if self.check(next, env) == TYPE.INVALID_TYPE:
                    valid = False
                break
            if self.check(hd, env) == TYPE.INVALID_TYPE:
                # Note the different pattern
                # from the walk.
                valid = False
            next = next[1]
        return valid

