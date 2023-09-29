from .parser import PARSE_ENUM


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
        elif "locals" in hd and hd.get("locals").get(name, None) is not None:
            found.append(hd.get("locals").get(name))
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
            found.append(".".join(namespace_components[: i + 1]))
        elif "locals" in hd and "name" in hd["locals"]:
            found.append(".".join(namespace_components[: i + 1]))
    if found:
        return ".".join(found)
    return None


def walk_stmt_list(ast):
    # Start at ast[1] because ast[0] will be the
    # opcode that is followed by the statement list.
    next = ast[1]
    if next is None:
        return
    hd = next[0]
    while True:
        if hd == PARSE_ENUM.END_OF_STMTS:
            return
        elif hd == PARSE_ENUM.END_OF_FN_ARGS:
            return
        if type(hd) is not tuple:
            assert False, "Shouldn't have ended here -- need end of stmts"

        yield hd
        next = next[1]
        hd = next[0]


def walk_stmt_list_base(ast):
    next = ast[0]
    if next is None:
        return
    hd = next[0]
    while True:
        if hd == PARSE_ENUM.END_OF_STMTS:
            return
        elif hd == PARSE_ENUM.END_OF_FN_ARGS:
            return
        if type(hd) is not tuple:
            print(hd)
            assert False, "Shouldn't have ended here -- need end of stmts"

        yield hd
        next = next[1]
        hd = next[0]
