from .compiler import emit
from .cc_typing import TYPE
from .cc_ast import get_scope_child
from .utils import print_debug
class VM:
    def __init__(self, code, compilation_env, symbol_table = {}):
        self.compilation_env = compilation_env
        self.code = code
        self.symbol_table = symbol_table

    # If I initialize this class, then call run() with debug=False,
    # why does print_debug in _run() still behave as print()?
    def run(self, debug=False):
        return VM._run(self.code, self.compilation_env, self.symbol_table, debug)

    @staticmethod
    def _run(code, compilation_env, _symbol_table, debug=False):
        # Note: this will be rewritten in C / ported.
        # (ip, max_ip)
        symbol_table = _symbol_table
#        symbol_table.update(_symbol_table) # In case we are continuing
        call_stack_ptrs = [(0, code, [], symbol_table)]
        # some sort of REPL-like loop.
        while call_stack_ptrs:
            ip, code, stack, symbol_table = call_stack_ptrs.pop()
            while ip < len(code):
                instr = code[ip]
                print_debug("{:>12}  {}".format(ip, instr))
                ip += 1
                op = instr[0]
                if op == "print":
                    print(stack.pop())
                if op == "pstr":
                    stack.append(instr[1].strip('"'))
                if op == "pnum":
                    stack.append(float(instr[1]))
                if op == "pbool":
                    stack.append(instr[1] == "true")
                if op == "pid":
                    # For assignments, etc.
                    stack.append(instr[1])
                if op == "pval":
                    var = instr[1]
                    if symbol_table.get(var, None) is None:
                        raise Exception(f"UNDEFINED VAR {var}")
                        # Error recovery, lets continue
                    else:
                        stack.append(symbol_table.get(var))
                if op == "pop":
                    stack.pop()
                if op == "var_decl":
                    var = stack.pop()
                    symbol_table[var] = TYPE.UNINIT_VAL  # Default to nothing.
                if op == "jmpif":
                    if not stack[-1]:
                        ip = instr[1]
                        continue

                if op == "var_assgn":

                    var = stack.pop()
                    val = stack.pop()
                    symbol_table[var] = val
                if op == "const_assgn":

                    var = stack.pop()
                    val = stack.pop()
                    symbol_table[var] = val
                if op == "add":
                    a, b = stack.pop(), stack.pop()
                    stack.append(b + a)
                if op == "mul":
                    stack.append(stack.pop() * stack.pop())
                if op == "sub":
                    stack.append(-stack.pop() + stack.pop())
                if op == "neg":
                    stack.append(-1 * stack.pop())
                if op == "eq":
                    stack.append(stack.pop() == stack.pop())
                if op == "neq":
                    stack.append(stack.pop() != stack.pop())
                if op == "gt":
                    a = stack.pop()
                    b = stack.pop()
                    stack.append(b > a)
                if op == "gte":
                    a = stack.pop()
                    b = stack.pop()
                    stack.append(b >= a)
                if op == "lt":
                    a = stack.pop()
                    b = stack.pop()
                    stack.append(b < a)
                if op == "lte":
                    a = stack.pop()
                    b = stack.pop()
                    stack.append(b <= a)
                if op == "call":
                    target = instr[1]
                    # Get argument information from context
                    # TODO: we should be using a TEXTUAL CONTEXT
                    # e.g. no data structure here.
                    # For now, for PoC, we will figure out
                    # labels and jump targets etc later.

                    data = get_scope_child(target, compilation_env)

                    call_stack_ptrs.append((ip, code, stack, symbol_table))
                    ip, code = 0, data["code"]
                    arg_stack_instrs = []
                    arg_stack_vals = []
                    symbol_table = {}
                    # Set up the arguments on the function stack frame
                    for arg, _ in data["arglist"]:
                        val = stack.pop()
                        arg_stack_vals.append(val)
                        emit("pid", arg, buf=arg_stack_instrs)
                        emit("var_assgn", buf=arg_stack_instrs)

                    code = arg_stack_instrs + code
                    arg_stack_vals.reverse()  # why do i have to do this when i went through all the trouble to do it before?
                    stack = arg_stack_vals
                    # call_stack_ptrs.append((0, data['code']))
                    # break # essentially, jmpt, indicating
                    # a context switch
                if op == "ret":
                    print_debug(f"Leaving: {symbol_table}")
                    data = get_scope_child(target, compilation_env)
                    if data["ret"] != "void":
                        # Pop the result, put onto new stack
                        ret_val = stack.pop()
                        ip, code, stack, symbol_table = call_stack_ptrs.pop()
                        stack.append(ret_val)
                    else:
                        ip, code, stack, symbol_table = call_stack_ptrs.pop()

        return stack, symbol_table
