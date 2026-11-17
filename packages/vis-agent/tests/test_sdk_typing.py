"""Regression coverage for SDK types under Pyright standard checks."""

import importlib.util
import json
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

from blockether.vis.engine import GatewayClient, JSONValue, Session, final_answer


def start(client: GatewayClient, project: str, group: str) -> Session:
    options = client.session_options(project)
    assert_type(options["root"], str)
    session = client.create_session(**options)
    session.update(title="Run")
    return client.create_session(group=group, timeout=30.0, **options)


def answer(session: Session) -> str | None:
    rows = session.turns(status="queued")
    assert_type(rows, list[dict[str, JSONValue]])
    session.send("Hello", provider="p", model="m", attachments=["upload-1"])
    return final_answer(rows[0])


WRONG = final_answer("text")
"""


_EVENTS_FIXTURE = """from typing import assert_type

from blockether.vis.engine import Event, Events, Session


def follow(session: Session) -> list[str]:
    with session.events(cursor=0, idle_timeout=5.0) as events:
        assert_type(events, Events)
        return [event.type for event in events]


async def follow_async(session: Session) -> list[Event]:
    seen: list[Event] = []
    async with session.events(reconnects=0) as events:
        async for event in events:
            assert_type(event, Event)
            seen.append(event)
    return seen


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
    # `for` and `async for` over `Session.events` yield `Event` (#485). Only
    # the wrong argument of the last line is a finding.
    fixture = tmp_path / "event_streams.py"
    fixture.write_text(_EVENTS_FIXTURE)
    result = _pyright(tmp_path, fixture, mode="strict")
    report = json.loads(result.stdout)
    found = [
        (item["range"]["start"]["line"], item["rule"])
        for item in report["generalDiagnostics"]
    ]
    wrong = _EVENTS_FIXTURE.splitlines().index('    session.events(idle_timeout="5")')
    assert found == [(wrong, "reportArgumentType")], result.stdout
