"""Regression coverage for SDK types under Pyright standard checks."""

import ast
import importlib.util
import json
import re
import subprocess
import sys
from pathlib import Path

import blockether.vis.extension as vis

_SYMBOL_TAG_FIXTURE = """from typing import assert_type

import blockether.vis.extension as vis


def _knows(tag: vis.SymbolTag) -> bool:
    try:
        vis.method(tag=tag)
    except ValueError:
        return False
    return True


CHECK: vis.SymbolTag = "verification" if _knows("verification") else "observation"


def bind(fn, *, tag: vis.SymbolTag = "observation"):
    return vis.method(tag=tag)(fn)


bind(print, tag=CHECK)
assert_type(vis.Symbol(print, tag=CHECK).tag, vis.SymbolTag)
UNKNOWN: vis.SymbolTag = "unknown"
"""

_SETTING_FIXTURE = """from typing import assert_type

import blockether.vis.extension as vis

ENABLED = vis.Setting(id="enabled", label="Desktop alerts", default=True)
LEVEL = vis.Setting(
    id="level", label="Level", default="fast", type="enum", choices=["fast", "slow"]
)


def start(*, enabled: bool) -> None: ...


def context(env):
    start(enabled=ENABLED.value())
    assert_type(ENABLED, vis.Setting[bool])
    assert_type(ENABLED.value(), bool)
    assert_type(LEVEL.value(), str)
"""

_SESSION_FIXTURE = """from typing import assert_type

from blockether.vis.engine import GatewayClient, Session, final_answer
from blockether.vis.engine._types import Turn


def start(client: GatewayClient, project: str, group: str) -> Session:
    options = client.session_options(project)
    assert_type(options["root"], str)
    session = client.create_session(**options)
    session.update(title="Run")
    return client.create_session(group=group, timeout=30.0, **options)


def answer(session: Session) -> str | None:
    rows = session.turns(status="queued")
    assert_type(rows, list[Turn])
    assert_type(rows[0]["turn_id"], str)
    assert_type(rows[0]["created_at"], int)
    session.send("Hello", provider="p", model="m", attachments=["upload-1"])
    return final_answer(rows[0])


WRONG = final_answer("text")
"""


_EVENTS_FIXTURE = """from typing import Literal, assert_type

from blockether.vis.engine import Event, Events, Session


def follow(session: Session) -> list[str]:
    with session.events(cursor=0, idle_timeout=5.0) as events:
        assert_type(events, Events)
        return [event.type for event in events]


async def follow_async(session: Session) -> list[Event]:
    seen: list[Event] = []
    async with session.events(reconnects=0) as events:
        async for event in events:
            assert_type(event.session_id, str)
            seen.append(event)
    return seen


def answers(session: Session) -> list[str]:
    # A check of `type` narrows `data` to the payload of that event type (#487).
    found: list[str] = []
    with session.events(idle_timeout=5.0) as events:
        for event in events:
            if event.type == "turn.completed":
                assert_type(event.turn_id, str)
                assert_type(event.data["status"], str)
                found.append(event.data["status"])
            elif event.type == "queue.paused":
                assert_type(event.data["held"], int)
            elif event.type == "content.block.delta":
                assert_type(event.data["field"], Literal["text", "markdown"])
                found.append(event.data["state"])
    return found


def wrong(session: Session) -> None:
    session.events(idle_timeout="5")
"""


# These evals modules import the optional `harbor` extra.
_HARBOR_MODULES = ("agent.py", "check.py", "environment.py", "judge.py")


def _pyright(tmp_path, *targets, ignore=(), mode="standard"):
    config = tmp_path / "pyrightconfig.json"
    settings = {"typeCheckingMode": mode, "pythonVersion": "3.12"}
    if ignore:
        settings["ignore"] = [str(path) for path in ignore]
    config.write_text(json.dumps(settings))
    command = [
        sys.executable,
        "-m",
        "pyright",
        "--project",
        str(config),
        "--pythonpath",
        sys.executable,
        "--warnings",
        "--outputjson",
        *(str(target) for target in targets),
    ]
    return subprocess.run(command, capture_output=True, text=True, check=False)


def test_sdk_sources_pass_pyright_standard(tmp_path):
    # The SDK ships py.typed, so type checkers trust its annotations. Each
    # standard-mode finding needs a fix or an explicit suppression (#315).
    package = Path(vis.__file__).resolve().parent
    ignore = ()
    if importlib.util.find_spec("harbor") is None:
        ignore = [package / "evals" / name for name in _HARBOR_MODULES]
    result = _pyright(tmp_path, package, ignore=ignore)
    assert result.returncode == 0, result.stdout + result.stderr
    summary = json.loads(result.stdout)["summary"]
    assert summary["filesAnalyzed"] == len(list(package.rglob("*.py")))


def test_symbol_tag_types_tags_in_variables(tmp_path):
    # A tag in a variable or a helper parameter needs a public type (#313).
    # The alias still rejects a tag that Vis does not know.
    fixture = tmp_path / "symbol_tag.py"
    fixture.write_text(_SYMBOL_TAG_FIXTURE)
    result = _pyright(tmp_path, fixture)
    report = json.loads(result.stdout)
    found = [
        (item["range"]["start"]["line"], item["rule"])
        for item in report["generalDiagnostics"]
    ]
    unknown = _SYMBOL_TAG_FIXTURE.splitlines().index(
        'UNKNOWN: vis.SymbolTag = "unknown"'
    )
    assert found == [(unknown, "reportAssignmentType")], result.stdout


def test_setting_value_has_the_type_of_its_default(tmp_path):
    # A boolean setting reads a bool and a choice setting reads a str (#314).
    fixture = tmp_path / "setting_value.py"
    fixture.write_text(_SETTING_FIXTURE)
    result = _pyright(tmp_path, fixture)
    assert result.returncode == 0, result.stdout + result.stderr


def test_session_calls_have_known_types_under_pyright_strict(tmp_path):
    # `create_session(**layer.session_options(project))` is the documented call.
    # It and the turn calls of a session must have no unknown type for a strict
    # caller. Only the wrong argument of the last line is a finding.
    fixture = tmp_path / "session_calls.py"
    fixture.write_text(_SESSION_FIXTURE)
    result = _pyright(tmp_path, fixture, mode="strict")
    report = json.loads(result.stdout)
    found = [
        (item["range"]["start"]["line"], item["rule"])
        for item in report["generalDiagnostics"]
    ]
    wrong = _SESSION_FIXTURE.splitlines().index('WRONG = final_answer("text")')
    assert found == [(wrong, "reportArgumentType")], result.stdout


def test_event_streams_have_known_types_under_pyright_strict(tmp_path):
    # `for` and `async for` over `Session.events` yield typed events (#485,
    # #487). Only an unknown payload key and the wrong argument are findings.
    fixture = tmp_path / "event_streams.py"
    fixture.write_text(_EVENTS_FIXTURE)
    result = _pyright(tmp_path, fixture, mode="strict")
    report = json.loads(result.stdout)
    found = [
        (item["range"]["start"]["line"], item["rule"])
        for item in report["generalDiagnostics"]
    ]
    lines = _EVENTS_FIXTURE.splitlines()
    unknown = lines.index('                found.append(event.data["state"])')
    wrong = lines.index('    session.events(idle_timeout="5")')
    assert found == [
        (unknown, "reportGeneralTypeIssues"),
        (unknown, "reportUnknownArgumentType"),
        (wrong, "reportArgumentType"),
    ], result.stdout


# A raw decoder and a local file manifest have no route schema to name.
_UNTYPED_RETURNS = {
    ("Response", "json"): "It decodes any JSON body; the route methods name its type.",
    ("ExecutionLayer", "snapshot_info"): "It reads the manifest of a local file.",
}


def test_public_client_methods_return_known_types():
    # A public method names its return type, and a route method names its
    # generated response type, never a bare JSON value or no type (#487). Only
    # the methods in `_UNTYPED_RETURNS` can return `Any`.
    source = Path(vis.__file__).resolve().parent / "engine" / "_client.py"
    untyped = set()
    for node in ast.walk(ast.parse(source.read_text(encoding="utf-8"))):
        if not isinstance(node, ast.ClassDef) or node.name.startswith("_"):
            continue
        for method in node.body:
            if (
                isinstance(method, ast.FunctionDef | ast.AsyncFunctionDef)
                and not method.name.startswith("_")
                and (
                    method.returns is None
                    or re.search(r"\b(?:JSONValue|Any)\b", ast.unparse(method.returns))
                )
            ):
                untyped.add((node.name, method.name))
    assert untyped == set(_UNTYPED_RETURNS)
