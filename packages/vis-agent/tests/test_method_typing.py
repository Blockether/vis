"""Regression coverage for #217: method decorators preserve callable types."""

import json
import subprocess
import sys
from pathlib import Path

import blockether.vis.extension as vis
import pytest


@pytest.mark.parametrize("descriptor", [None, staticmethod, classmethod])
@pytest.mark.parametrize("configured", [False, True])
def test_method_preserves_identity_and_metadata(descriptor, configured):
    def read(value: str) -> str:
        return value

    original = descriptor(read) if descriptor else read
    decorated = (
        vis.method(tag="mutation")(original) if configured else vis.method(original)
    )
    assert decorated is original
    assert read.__vis_symbol_tag__ == ("mutation" if configured else "observation")
    assert read.__vis_symbol_hidden__ is False
    assert read.__vis_symbol_activity__ is None


def test_method_types_with_pyright(tmp_path):
    # Pylance uses Pyright: check wrappers, bare/configured forms and both
    # descriptor orders, including argument and result type preservation (#217).
    source = """from typing import assert_type
import blockether.vis.extension as vis


def activity():
    return vis.method(tag="observation", activity=vis.Activity(label="Read value"))


class Example:
"""
    names = []
    for decorator in ("vis.method", "vis.method()", "activity()"):
        for descriptor in (None, "staticmethod", "classmethod"):
            for outer in (False, True) if descriptor else (False,):
                name = f"read_{len(names)}"
                names.append(name)
                decorators = [decorator]
                if descriptor:
                    decorators.insert(0 if outer else 1, descriptor)
                source += "".join(f"    @{d}\n" for d in decorators)
                first = "cls, " if descriptor == "classmethod" else "self, "
                if descriptor == "staticmethod":
                    first = ""
                source += f"    def {name}({first}value: str) -> str:\n        return value\n\n"
    source += "example = Example()\n"
    for name in names:
        source += f'assert_type(example.{name}("ok"), str)\n'
    fixture = tmp_path / "method_types.py"
    fixture.write_text(source)
    config = tmp_path / "pyrightconfig.json"
    config.write_text(
        json.dumps(
            {
                "extraPaths": [str(Path(vis.__file__).resolve().parents[2])],
                "typeCheckingMode": "basic",
                "pythonVersion": "3.11",
            }
        )
    )
    command = [
        sys.executable,
        "-m",
        "pyright",
        "--project",
        str(config),
        str(fixture),
        "--outputjson",
    ]
    result = subprocess.run(command, capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stdout + result.stderr
    # A decorator returning Any could hide the original error: wrong arguments
    # must still be rejected for every supported form.
    fixture.write_text(source + "".join(f"example.{name}(123)\n" for name in names))
    result = subprocess.run(command, capture_output=True, text=True, check=False)
    report = json.loads(result.stdout)
    assert report["summary"]["errorCount"] == len(names), result.stdout
    assert all(d["rule"] == "reportArgumentType" for d in report["generalDiagnostics"])
