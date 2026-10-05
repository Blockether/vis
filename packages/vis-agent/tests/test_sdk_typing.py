"""Regression coverage for SDK types under Pyright standard checks."""

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


def _pyright(tmp_path, *targets):
    config = tmp_path / "pyrightconfig.json"
    config.write_text(
        json.dumps({"typeCheckingMode": "standard", "pythonVersion": "3.11"})
    )
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
    result = _pyright(tmp_path, package)
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
