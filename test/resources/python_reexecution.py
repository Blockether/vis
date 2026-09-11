"""Vis #199: re-execution uses standalone CPython on JVM and native."""

import os
import subprocess
import sys
import tempfile
from pathlib import Path

assert Path(sys.executable).name == "python3", sys.executable
assert sys._base_executable == sys.executable
child_environment = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
with tempfile.TemporaryDirectory(
    prefix="vis-reexecution-", dir=os.getcwd()
) as directory:
    fixture = Path(directory)
    launcher = fixture / "python3"
    launcher.symlink_to(sys.executable)
    (fixture / "child_probe.py").write_text("print(456)\n")
    for executable in (sys.executable, sys._base_executable, str(launcher)):
        for args, stdin in ((["-c", "print(123)"], None), (["-m", "json.tool"], "123")):
            child = subprocess.run(
                [executable, *args],
                input=stdin,
                env=child_environment,
                capture_output=True,
                text=True,
                timeout=15,
            )
            assert child.returncode == 0, (executable, args, child.stderr)
            assert child.stdout.strip() == "123", child.stdout
    child = subprocess.run(
        [sys.executable, "-m", "child_probe"],
        env=dict(child_environment, PYTHONPATH=directory),
        capture_output=True,
        text=True,
        timeout=15,
    )
    assert child.returncode == 0, child.stderr
    assert child.stdout.strip() == "456", child.stdout
print("python-reexecution-ok")
