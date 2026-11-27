"""Read, describe and save the workflow files of a project."""

from __future__ import annotations

import ast
import json
import subprocess
import tempfile
import threading
from pathlib import Path
from typing import Any

from factory import settings
from factory.records import NAME, Refused, need

LOCK = threading.Lock()


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
    except SyntaxError as failure:
        raise Refused(400, f"line {failure.lineno}: {failure.msg}") from None
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
    if any(value is not None and not isinstance(value, str) for key, value in about.items() if key in ("title", "description", "model")):
        raise Refused(400, "the workflow title, description and model must be text")
    activities = [(node, marks(node, "activity")) for node in functions]
    return {
        "name": name,
        "title": about.get("title") or entry.name,
        "description": about.get("description") or ast.get_docstring(entry) or "",
        "model": about.get("model"),
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


def path_for(name: str) -> Path:
    """Return the file of a workflow. It cannot be a link to another file."""
    path = Path(settings.WORKFLOWS).resolve() / f"{need(NAME, name, 'the workflow name')}.py"
    if path.is_symlink():
        raise Refused(400, "the workflow file is a link")
    return path


def read(name: Any) -> str:
    """Return the UTF-8 source of a workflow."""
    try:
        return path_for(name).read_text(encoding="utf-8")
    except FileNotFoundError:
        raise Refused(404, "no such workflow") from None
    except UnicodeDecodeError:
        raise Refused(400, "the file is not UTF-8 text") from None


def listing() -> list[dict[str, Any]]:
    """Return what each workflow says about itself, and when it changed."""
    found = []
    for path in Path(settings.WORKFLOWS).glob("*.py"):
        name = path.stem
        if not NAME.fullmatch(name) or not path.is_file():
            continue
        try:
            updated = path.stat().st_mtime
        except FileNotFoundError:
            continue
        try:
            about = describe(name, read(name))
        except Refused as refusal:
            about = {"name": name, "title": name, "error": str(refusal), "inputs": [], "activities": [], "triggers": []}
        found.append({**about, "updated": updated})
    return sorted(found, key=lambda about: about["title"].lower())


def git(project: Path, *arguments: str) -> subprocess.CompletedProcess[str]:
    """Run Git in the project. Do not return its output to a caller."""
    return subprocess.run(["git", "--literal-pathspecs", *arguments], cwd=project, capture_output=True, text=True, check=False)


def release() -> str:
    """Return the current Git commit, or an empty string outside Git."""
    try:
        answer = git(Path(settings.PROJECT), "rev-parse", "HEAD")
    except FileNotFoundError:
        return ""
    return answer.stdout.strip() if answer.returncode == 0 else ""


def commit(name: str, source: str | None, base: str | None = None) -> dict[str, Any]:
    """Write or remove a file, and commit only that file when Git contains it."""
    with LOCK:
        path = path_for(name)
        try:
            before = path.read_text(encoding="utf-8")
        except FileNotFoundError:
            before = None
        except UnicodeDecodeError:
            if base is not None:
                raise Refused(409, "the workflow changed; open it again") from None
            before = None
        if base is not None and base != before:
            raise Refused(409, "the workflow changed; open it again")
        if source is None:
            path.unlink(missing_ok=True)
        elif before != source:
            path.parent.mkdir(parents=True, exist_ok=True)
            with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as temp:
                temporary = Path(temp.name)
                try:
                    temp.write(source.encode())
                    temp.close()
                    temporary.replace(path)
                finally:
                    temporary.unlink(missing_ok=True)
        project = Path(settings.PROJECT).resolve()
        if not path.is_relative_to(project):
            return {"sha": "", "changed": False, "url": ""}
        try:
            top = git(project, "rev-parse", "--show-toplevel")
        except FileNotFoundError:
            return {"sha": "", "changed": False, "url": ""}
        if top.returncode:
            return {"sha": "", "changed": False, "url": ""}
        relative = str(path.relative_to(project))
        status = git(project, "status", "--porcelain", "--", relative)
        if status.returncode:
            raise Refused(502, "Git could not read the workflow status")
        if status.stdout:
            if status.stdout.startswith("??") and git(project, "add", "--", relative).returncode:
                raise Refused(502, "Git could not add the workflow")
            action = "Save" if source is not None else "Remove"
            result = git(project, "-c", f"user.name={settings.GIT_NAME}", "-c", f"user.email={settings.GIT_EMAIL}", "commit", "--only", "-m", f"{action} workflow {name}", "--", relative)
            if result.returncode:
                raise Refused(502, "Git could not commit the workflow")
        return {"sha": release(), "changed": bool(status.stdout), "url": ""}


def save(name: str, source: str, base: str | None = None) -> dict[str, Any]:
    """Save a valid workflow file and return what it says about itself."""
    if len(source.encode()) > settings.WORKFLOW_LIMIT:
        raise Refused(413, "the workflow is too large")
    about = describe(need(NAME, name, "the workflow name"), source)
    stored = commit(name, source, base)
    return {**about, "commit": stored["sha"], "committed": stored["changed"], "url": stored["url"]}


def remove(name: str) -> None:
    """Remove a workflow. Its old runs keep their source."""
    commit(name, None)
