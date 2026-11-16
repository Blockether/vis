"""The workflow files: their sources in S3 and what a source says about its workflow.

workflows/<name>.py   the source of a workflow

A run reads the source from S3. The main branch of the repository has the same files,
and the repo function is the only writer of both: it commits each save and each
removal, and it copies each change of the main branch to S3.
"""

from __future__ import annotations

import ast
import json
from typing import Any

from durable import aws
from durable.domain import NAME, Refused, need
from durable.helpers import log
from durable.settings import REPO_FUNCTION, WORKFLOW_LIMIT


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
    try:
        return raw.decode()
    except UnicodeDecodeError:
        # The file comes from the repository, where a person can put any bytes.
        raise Refused(400, "the file is not UTF-8 text") from None


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


def commit(name: str, source: str | None, base: str | None = None) -> dict[str, Any]:
    """Put the file of a workflow in the repository, or remove it there. The repo function then updates S3. Return the commit."""
    raw, failed = aws.invoke(REPO_FUNCTION, action="store", name=name, source=source, base=base)
    if failed:
        log("store failed", raw[:400].decode(errors="replace"))
        raise Refused(502, "The save did not complete. Save again.")
    stored = json.loads(raw)
    if stored["moved"]:
        raise Refused(409, "The repository has another version of this workflow. Open the workflow again, then change it.")
    return stored


def save(name: str, source: str, base: str | None = None) -> dict[str, Any]:
    """Store the source of a workflow that is valid. Return what it says about itself.

    `base` is the source that the person changed. The save is refused when the
    repository has another source.
    """
    if len(source) > WORKFLOW_LIMIT:
        raise Refused(413, "the workflow is too large")
    about = describe(need(NAME, name, "the workflow name"), source)
    stored = commit(name, source, base)
    return {**about, "commit": stored["sha"], "committed": stored["changed"], "url": stored["url"]}


def remove(name: str) -> None:
    """Remove the source of a workflow."""
    commit(need(NAME, name, "the workflow name"), None)
