"""The workflow files: their sources in S3 and what a source says about its workflow.

workflows/<name>.py   the source of a workflow
"""

from __future__ import annotations

import ast
import glob
import json
import os
from typing import Any

from durable import aws
from durable.domain import NAME, Refused, need

# The workflows of the image. A new stack starts with them.
SAMPLES = "/opt/durable/workflows"


def literal(node: ast.expr | None) -> Any:
    """Return the value of a constant in the source, or None for other code."""
    try:
        return None if node is None else ast.literal_eval(node)
    except ValueError:
        return None


def marks(function: ast.FunctionDef, name: str) -> dict[str, Any] | None:
    """Return the keyword arguments of the decorator `name` of a function."""
    for decorator in function.decorator_list:
        call = decorator if isinstance(decorator, ast.Call) else None
        target = call.func if call else decorator
        if isinstance(target, ast.Name) and target.id == name:
            return {word.arg: literal(word.value) for word in (call.keywords if call else []) if word.arg}
    return None


def describe(name: str, source: str) -> dict[str, Any]:
    """Read the title, the inputs and the activities of a workflow. The source does not run."""
    try:
        tree = ast.parse(source)
    except SyntaxError as failure:
        raise Refused(400, f"line {failure.lineno}: {failure.msg}") from None
    functions = [node for node in tree.body if isinstance(node, ast.FunctionDef)]
    entries = [(node, marks(node, "workflow")) for node in functions]
    entries = [(node, found) for node, found in entries if found is not None]
    if len(entries) != 1:
        raise Refused(400, "a workflow file needs exactly one function with @workflow")
    entry, found = entries[0]
    arguments = entry.args.args[1:]
    defaults = [None] * (len(arguments) - len(entry.args.defaults)) + list(entry.args.defaults)
    activities = [(node, marks(node, "activity")) for node in functions]
    return {
        "name": name,
        "title": found.get("title") or entry.name,
        "description": found.get("description") or ast.get_docstring(entry) or "",
        "model": found.get("model"),
        "inputs": [{"name": a.arg, "default": literal(d)} for a, d in zip(arguments, defaults, strict=True)],
        "activities": [{"name": node.name, "title": kept.get("title") or node.name} for node, kept in activities if kept is not None],
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


def read(name: Any) -> str:
    """Return the source of a workflow, or refuse a name that S3 does not have."""
    raw, _ = aws.get(f"workflows/{need(NAME, name, 'the workflow name')}.py")
    if raw is None:
        raise Refused(404, "no such workflow")
    return raw.decode()


def listing() -> list[dict[str, Any]]:
    """Return what each workflow says about itself, with the time of its last change."""
    found = []
    for item in aws.keys("workflows/"):
        name = item["Key"][len("workflows/") : -len(".py")]
        try:
            about = describe(name, read(name))
        except Refused as refusal:
            about = {"name": name, "title": name, "error": str(refusal), "inputs": [], "activities": []}
        found.append({**about, "updated": item["LastModified"].timestamp()})
    return sorted(found, key=lambda about: about["title"].lower())


def save(name: str, source: str) -> dict[str, Any]:
    """Store the source of a workflow that is valid. Return what it says about itself."""
    if len(source) > 200_000:
        raise Refused(413, "the workflow is too large")
    about = describe(need(NAME, name, "the workflow name"), source)
    aws.put(f"workflows/{name}.py", source.encode())
    return about


def remove(name: str) -> None:
    aws.delete(f"workflows/{need(NAME, name, 'the workflow name')}.py")


def seed() -> None:
    """Give a new stack the workflows of the image."""
    if not aws.keys("workflows/"):
        for path in sorted(glob.glob(f"{SAMPLES}/*.py")):
            with open(path, "rb") as file:
                aws.put(f"workflows/{os.path.basename(path)}", file.read())
