"""The repository's own example extension, run with no Vis in the room.

`resources/examples/python-extensions/todo.py` is written for the agent: it
registers tools, keeps durable state and answers a slash command. Nothing about it
knows whether an engine is listening — so it is the honest end-to-end proof that an
extension is an ordinary Python file, importable, runnable and testable from a
plain interpreter.

It runs in a CHILD interpreter because `vis.extension(...)` may only be called once
per process, and because a fresh `python -c` is exactly what an author has.
"""

import json
import os
import subprocess
import sys
from pathlib import Path

SRC = Path(__file__).resolve().parents[1] / "src"
EXAMPLE = (
    Path(__file__).resolve().parents[3]
    / "resources"
    / "examples"
    / "python-extensions"
    / "todo.py"
)

DRIVER = """
import json, runpy, sys
import vis

runpy.run_path(sys.argv[1])
spec = vis._registration["spec"]
tools = {s["name"]: s["fn"] for s in spec["symbols"]}
tools["todo_add"]("ship the package")
tools["todo_add"]("write the release note")
tools["todo_toggle"](1)
listed = tools["todo_list"]()
slash = spec["slash_commands"][0]
print(json.dumps({
    "name": spec["name"],
    "alias": spec["alias"],
    "open": listed["open"],
    "done": listed["done"],
    "texts": [t["text"] for t in listed["todos"]],
    "slash": slash["name"],
    "title": slash["run"](None)["title"],
}))
"""


def test_the_example_extension_registers_and_runs_outside_vis(
    outside_home, monkeypatch
):
    assert EXAMPLE.exists(), EXAMPLE
    env = os.environ.copy()
    env["PYTHONPATH"] = str(SRC)
    env["VIS_OUTSIDE_HOME"] = str(outside_home)
    done = subprocess.run(
        [sys.executable, "-c", DRIVER, str(EXAMPLE)],
        capture_output=True,
        text=True,
        env=env,
        timeout=120,
    )
    assert done.returncode == 0, done.stderr
    report = json.loads(done.stdout.strip().splitlines()[-1])
    assert report["name"] == "todo"
    assert report["alias"] == "todo"
    assert report["texts"] == ["ship the package", "write the release note"]
    assert (report["open"], report["done"]) == (1, 1)
    assert report["slash"] == "todos"
    assert report["title"] == "1/2 completed"
    # The state the child wrote is the state a next run reads.
    assert (
        json.loads((outside_home / "state.json").read_text())["todos"][0]["done"]
        is True
    )
