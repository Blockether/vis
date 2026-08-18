"""Static check of a Python extension file: parse it, never import it.

The point is to answer "would this file load, and would the forms it asks for be
accepted" WITHOUT running a single line of it. So there is no import here and no
`eval` of the file: it is parsed with `ast`, and only the expressions that are
statically knowable are turned into values - literals, containers, and calls to
the `vis` form builders, which are pure dict constructors.

Three things come out of that:

* a syntax error, with its own line;
* every `vis.<name>` the module reads that the `vis` module does not have, which
  is how `vis.plaintxt(...)` is caught before it raises in front of a human;
* every `vis.ask(...)` whose request can be reconstructed, judged by CALLING it
  on a host that normalizes the request and settles it `undeliverable` instead
  of asking anybody - the engine's own refusal, never a second opinion, and
  every `vis.live(...)` view judged the same way on a host that mounts nothing.

A `validate=` function is checked by SHAPE, never called: a lambda or a `def` in
the file stands in as a placeholder taking exactly the arguments it declares, so
`validate=lambda: None` is refused here for the same reason `vis.ask` refuses it.

An argument that cannot be known without running the file (a variable from an
argument, an f-string, a comprehension) is COUNTED as skipped rather than
guessed at: a checker that invents values would report problems nobody has.
"""

import ast
import json

import vis

# Pure node constructors: calling one builds a dict and touches nothing else,
# which is why a static check is allowed to call them. The live half builds the
# nodes of a view, the form half the fields of a request.
_BUILDERS = (
    "plaintext",
    "password",
    "multiline",
    "select",
    "multiselect",
    "checkbox",
    "slider",
    "otp",
    "option",
    "row",
    "column",
    "heading",
    "paragraph",
    "status",
    "progress",
    "stat",
    "steps",
    "output",
    "table",
    "table_column",
    "table_row",
    "link",
)

# The two entry points that carry a declaration worth validating: a form to ask
# for, and a view to open.
_ASKS = ("ask",)
_OPENS = ("live",)


class _Unknown(Exception):
    """This expression's value is not knowable without running the file."""


# Stand-ins for a `validate=` function. A validator is CODE, so it is never run;
# only its call shape matters, and these carry every shape the engine tells
# apart: the value alone, the value and every value, anything at all, and one
# that can take neither (which is exactly what `vis.ask` refuses).
def _one(_value):
    return None


def _two(_value, _values):
    return None


def _any_args(*_args):
    return None


def _refused():
    return None


def _placeholder(args):
    """A stand-in with the same call shape as this `lambda` / `def` signature."""
    if args.vararg is not None:
        return _any_args
    if any(default is None for default in args.kw_defaults):
        return _refused  # a required keyword-only argument: never fillable
    total = len(getattr(args, "posonlyargs", [])) + len(args.args)
    required = total - len(args.defaults)
    if total < 1 or required > 2:
        return _refused
    return _two if total >= 2 else _one


def _vis_names(tree):
    """Every local name bound to the `vis` module by an `import`."""
    names = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name == "vis":
                    names.add(alias.asname or "vis")
    return names or {"vis"}


def _read(node, names):
    """The `<vis>.<attr>` this node reads, or None."""
    if (
        isinstance(node, ast.Attribute)
        and isinstance(node.value, ast.Name)
        and node.value.id in names
    ):
        return node.attr
    return None


def _value(node, env, names):
    """`node`'s value, or `_Unknown` when only running the file would tell."""
    if isinstance(node, ast.Constant):
        return node.value
    if isinstance(node, (ast.List, ast.Tuple, ast.Set)):
        return [_value(e, env, names) for e in node.elts]
    if isinstance(node, ast.Dict):
        return {
            _value(k, env, names): _value(v, env, names)
            for k, v in zip(node.keys, node.values)
        }
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
        return -_value(node.operand, env, names)
    if isinstance(node, ast.Name):
        if node.id in env:
            return env[node.id]
        raise _Unknown(node.id)
    if isinstance(node, ast.Lambda):
        return _placeholder(node.args)
    if isinstance(node, ast.Call):
        builder = _read(node.func, names)
        if builder in _BUILDERS:
            args = [_value(a, env, names) for a in node.args]
            return getattr(vis, builder)(*args, **_kwargs(node, env, names))
    raise _Unknown(type(node).__name__)


def _kwargs(node, env, names):
    """A call's keyword arguments, with an unknowable `validate=` stood in for."""
    out = {}
    for kw in node.keywords:
        if kw.arg is None:
            raise _Unknown("**kwargs")
        try:
            out[kw.arg] = _value(kw.value, env, names)
        except _Unknown:
            # A validator nobody can see IS code; the request only cares that it
            # is callable, so the common one-value shape stands in for it.
            if kw.arg != "validate":
                raise
            out[kw.arg] = _one
    return out


def _problem(kind, node, message):
    return {
        "kind": kind,
        "line": getattr(node, "lineno", 0),
        "column": getattr(node, "col_offset", 0) + 1,
        "message": message,
    }


def _declaration(node, env, names, key):
    """The `(title, <key>, options)` a `vis.ask` or a `vis.live` call carries."""
    positional = [_value(a, env, names) for a in node.args]
    options = _kwargs(node, env, names)
    title = positional[0] if positional else options.pop("title")
    listed = positional[1] if len(positional) > 1 else options.pop(key)
    return title, listed, options


def _reason(exc):
    """The one line a refusal says, without the class that carried it.

    A host refusal crosses the polyglot boundary as a foreign exception whose
    text reads `<host.class.Name>: <message>`, and the message is the whole of
    what the author has to fix.
    """
    text = str(exc).strip()
    head, separator, rest = text.partition(": ")
    if separator and "." in head and " " not in head:
        return rest
    return text


def _check_declaration(node, env, names, report, key, declare, kind):
    try:
        title, listed, options = _declaration(node, env, names, key)
    except (_Unknown, KeyError, IndexError):
        report["skipped"] += 1
        return
    report["checked"] += 1
    try:
        declare(title, listed, **options)
    except BaseException as exc:
        # The engine refused the declaration, or a builder refused the shape
        # outright. BaseException, because a host refusal arrives as a foreign
        # exception that `except Exception` does not catch.
        report["problems"].append(_problem(kind, node, _reason(exc)))


def _check_ask(node, env, names, report):
    _check_declaration(node, env, names, report, "fields", vis.ask, "invalid-request")


def _check_live(node, env, names, report):
    # A view is judged exactly as a form is: `vis.live` opens it on a host that
    # normalizes it and mounts nothing, so a node the vocabulary has no name for
    # is a problem HERE instead of in front of the human.
    _check_declaration(node, env, names, report, "nodes", vis.live, "invalid-view")


def _walk(node, env, names, report):
    """Depth first, in source order: bind what is knowable, judge every ask and view."""
    for child in ast.iter_child_nodes(node):
        if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef)):
            # `validate=is_a_slug` is knowable: the name stands for its shape.
            env[child.name] = _placeholder(child.args)
        if isinstance(child, ast.Attribute):
            attr = _read(child, names)
            if attr and not hasattr(vis, attr):
                report["problems"].append(
                    _problem(
                        "unknown-attribute",
                        child,
                        "the vis module has no " + attr,
                    )
                )
        if isinstance(child, ast.Call):
            called = _read(child.func, names)
            if called in _ASKS:
                _check_ask(child, env, names, report)
            elif called in _OPENS:
                _check_live(child, env, names, report)
        if (
            isinstance(child, ast.Assign)
            and len(child.targets) == 1
            and isinstance(child.targets[0], ast.Name)
        ):
            target = child.targets[0].id
            try:
                env[target] = _value(child.value, env, names)
            except _Unknown:
                env.pop(target, None)
        _walk(child, env, names, report)


def vis_check_source(source, path):
    """Check one extension file's `source`, reported as a JSON string."""
    report = {"path": path, "problems": [], "checked": 0, "skipped": 0}
    try:
        tree = ast.parse(source, filename=path)
    except SyntaxError as exc:
        report["problems"].append(
            {
                "kind": "syntax",
                "line": exc.lineno or 0,
                "column": exc.offset or 0,
                "message": exc.msg,
            }
        )
        report["is_valid"] = False
        return json.dumps(report)
    names = _vis_names(tree)
    _walk(tree, {}, names, report)
    if not any(
        isinstance(node, ast.Call) and _read(node.func, names) == "extension"
        for node in ast.walk(tree)
    ):
        report["problems"].append(
            {
                "kind": "no-extension",
                "line": 0,
                "column": 0,
                "message": "nothing calls vis.extension(...), so this file registers nothing",
            }
        )
    report["is_valid"] = not report["problems"]
    return json.dumps(report)
