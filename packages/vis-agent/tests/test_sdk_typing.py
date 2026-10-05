"""Regression coverage for #315: the SDK sources pass Pyright standard checks."""

import json
import subprocess
import sys
from pathlib import Path

import blockether.vis.extension as vis


def test_sdk_sources_pass_pyright_standard(tmp_path):
    # The SDK ships py.typed, so type checkers trust its annotations. Each
    # standard-mode finding needs a fix or an explicit suppression (#315).
    package = Path(vis.__file__).resolve().parent
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
        str(package),
    ]
    result = subprocess.run(command, capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stdout + result.stderr
    summary = json.loads(result.stdout)["summary"]
    assert summary["filesAnalyzed"] == len(list(package.rglob("*.py")))
