"""Read what the source of a workflow says about itself, without running it."""

from __future__ import annotations

import ast
import json
from typing import Any

from factory.records import Refused, need


def literal(node: ast.expr | None) -> Any:
    """Return the value of a constant in the source, or None for other code."""
    try:
        return None if node is None else ast.literal_eval(node)
    except (ValueError, TypeError):
        return None


def marks(function: ast.FunctionDef, name: str) -> dict[str, Any] | None:
    """Return the keyword arguments of the decorator `name` of a function."""
    for decorator in function.decorator_list:
        call = decorator if isinstance(decorator, ast.Call) else None
        target = call.func if call else decorator
        if isinstance(target, ast.Name) and target.id == name:
            return {word.arg: word.value for word in (call.keywords if call else []) if word.arg}
    return None


def describe(name: str, source: str) -> dict[str, Any]:
    """Read the title, the inputs and the activities of a workflow. The source does not run."""
    try:
        tree = ast.parse(source)
        compile(tree, f"{name}.py", "exec")  # finds the faults that the parser accepts, such as `return` outside a function
    except SyntaxError as failure:
        raise Refused(400, f"line {failure.lineno}: {failure.msg}") from None
    except ValueError:
        raise Refused(400, "the source is not Python text") from None
    functions = [node for node in tree.body if isinstance(node, ast.FunctionDef)]
    entries = [(node, marks(node, "workflow")) for node in functions]
    entries = [(node, found) for node, found in entries if found is not None]
    if len(entries) != 1:
        raise Refused(400, "a workflow file needs exactly one function with @workflow")
    entry, found = entries[0]
    positional = entry.args.posonlyargs + entry.args.args
    defaults = [None] * (len(positional) - len(entry.args.defaults)) + list(entry.args.defaults)
    arguments = list(zip(positional, defaults, strict=True))[1:] + list(zip(entry.args.kwonlyargs, entry.args.kw_defaults, strict=True))
    about = {key: literal(value) for key, value in found.items() if key != "triggers"}
    if any(value is not None and not isinstance(value, str) for key, value in about.items() if key in ("title", "description", "model", "provider")):
        raise Refused(400, "the workflow title, description, model and provider must be text")
    if about.get("provider") and "model" not in about:
        raise Refused(400, "give a model with the provider of the workflow")
    activities = [(node, marks(node, "activity")) for node in functions]
    return {
        "name": name,
        "title": about.get("title") or entry.name,
        "description": about.get("description") or ast.get_docstring(entry) or "",
        "model": about.get("model"),
        "provider": about.get("provider"),
        "inputs": [{"name": a.arg, "default": literal(d), "required": d is None, "type": ast.unparse(a.annotation) if a.annotation else None} for a, d in arguments],
        "activities": [{"name": node.name, "title": literal(kept.get("title")) or node.name} for node, kept in activities if kept is not None],
        "triggers": trigger_list(name, found.get("triggers"), {a.arg for a, _ in arguments}),
        "lines": source.count("\n") + 1,
    }


def inputs(about: dict[str, Any], text: str) -> dict[str, Any]:
    """Return the input of a run from a text: a JSON object, or the value of the first input."""
    text = text.strip()
    if not text.startswith("{"):
        return {about["inputs"][0]["name"]: text} if text and about["inputs"] else {}
    try:
        return json.loads(text)
    except ValueError:
        raise Refused(400, "the input is not a JSON object") from None


# The annotations that the factory can check without the code of the workflow, and the JSON type of each.
SIMPLE = {"str": str, "int": int, "float": (int, float), "bool": bool, "list": list, "dict": dict}


def check(about: dict[str, Any], given: dict[str, Any]) -> None:
    """Refuse an input that the workflow does not have, a necessary input that is absent, and a value of the wrong simple type."""
    known = {item["name"]: item for item in about["inputs"]}
    unknown = given.keys() - known.keys()
    if unknown:
        raise Refused(400, f"the workflow has no input {sorted(unknown)[0]!r}")
    for name, item in known.items():
        if name not in given:
            if item["required"]:
                raise Refused(400, f"the workflow needs the input {name!r}")
            continue
        kind = SIMPLE.get(item["type"])
        # In Python a bool is an int. In JSON it is not.
        if kind and (not isinstance(given[name], kind) or (isinstance(given[name], bool) and kind is not bool)):
            raise Refused(400, f"the input {name!r} must be {item['type']}")


def trigger_list(name: str, node: ast.expr | None, parameters: set[str]) -> list[dict[str, Any]]:
    """Read the literal calls of the workflow trigger list without running the file."""
    from factory.triggers import EVENT, Cron

    if node is None:
        return []
    if not isinstance(node, (ast.List, ast.Tuple)):
        raise Refused(400, "triggers must be a list of cron or on calls")
    found = []
    for index, call in enumerate(node.elts):
        if not isinstance(call, ast.Call) or not isinstance(call.func, ast.Name) or call.func.id not in ("cron", "on"):
            raise Refused(400, "a trigger must be a cron or on call")
        kind = call.func.id
        names = ["expression" if kind == "cron" else "event", "input"]
        args = dict(zip(names, call.args, strict=False))
        if len(call.args) > 2 or any(arg.arg not in names or arg.arg in args for arg in call.keywords):
            raise Refused(400, "a trigger must have a name or expression and optional input")
        args.update((arg.arg, arg.value) for arg in call.keywords)
        value = literal(args.get(names[0]))
        if kind == "cron":
            Cron.parse(value)
        else:
            need(EVENT, value, "the event kind")
        given = literal(args.get("input"))
        if "input" in args and given is None and not isinstance(args["input"], ast.Constant):
            raise Refused(400, "the trigger input must be a literal object")
        if given is not None and (not isinstance(given, dict) or set(given) - parameters):
            raise Refused(400, "the trigger input must be an object with workflow input names")
        try:
            json.dumps(given, allow_nan=False)
        except (TypeError, ValueError):
            raise Refused(400, "the trigger input must be JSON") from None
        found.append({"id": f"{name}/{index}", "kind": kind, "cron" if kind == "cron" else "event": value, "input": given or {}})
    return found
