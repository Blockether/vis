"""Tests for the Harbor host environment."""

from __future__ import annotations

import asyncio
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

import pytest
from blockether.vis.evals.environment import HostEnvironment
from blockether.vis.evals.grade import WORKSPACE_FILESYSTEM_FILE
from harbor.models.task.config import EnvironmentConfig
from harbor.models.trial.paths import TrialPaths

SEED_FILES = {"README.md": "seed\n", "src/app.py": "VALUE = 1\n"}


def _environment(
    tmp_path, *, files=None, registrations=None, workdir=None
) -> HostEnvironment:
    environment_dir = tmp_path / "task" / "environment"
    for name, text in (files or SEED_FILES).items():
        path = environment_dir / "workspace" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
    if registrations is not None:
        (environment_dir / WORKSPACE_FILESYSTEM_FILE).write_text(
            json.dumps(registrations)
        )
    trial_paths = TrialPaths(tmp_path / "trial")
    trial_paths.mkdir()
    return HostEnvironment(
        environment_dir=environment_dir,
        environment_name="host-task",
        session_id="host-task__trial",
        trial_paths=trial_paths,
        task_env_config=EnvironmentConfig(workdir=workdir),
    )


def _git(workspace: Path, *args: str) -> str:
    return subprocess.run(
        ["git", *args], cwd=workspace, capture_output=True, text=True, check=True
    ).stdout


def _assert_stopped(pid: int) -> None:
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return
        time.sleep(0.05)
    pytest.fail(f"process {pid} still runs")


@pytest.fixture
def started(tmp_path):
    environment = _environment(tmp_path)
    asyncio.run(environment.start(force_build=False))
    yield environment
    asyncio.run(environment.stop(delete=True))


def test_start_makes_a_git_repository_with_the_seed_files(started):
    workspace = started.workspace
    assert {name: (workspace / name).read_text() for name in SEED_FILES} == SEED_FILES
    assert _git(workspace, "log", "--format=%an <%ae> %s") == "t <t@t.co> init\n"
    assert _git(workspace, "status", "--porcelain") == ""


def test_path_table_maps_the_harbor_paths(started):
    table = started.path_table()
    assert table["/logs/agent"] == started.trial_paths.agent_dir
    assert table["/logs/verifier"] == started.trial_paths.verifier_dir
    assert table["/logs/artifacts"] == started.trial_paths.artifacts_dir
    assert table["/tests"] == started.root / "tests"
    assert table["/solution"] == started.root / "solution"
    assert all(path.is_dir() for path in table.values())
    assert started.host_path("/tests/test.sh") == started.root / "tests" / "test.sh"
    assert started.host_path("/logs/agent") == started.trial_paths.agent_dir
    assert started.host_path("notes.txt") == started.workspace / "notes.txt"
    assert started.host_path("/etc/hosts") == Path("/etc/hosts")


def test_translate_changes_only_container_paths(started):
    command = "(/tests/test.sh) > /logs/verifier/test-stdout.txt 2>&1; ls /x/tests a/tests /testsuite"
    verifier = started.trial_paths.verifier_dir
    assert started.translate(command) == (
        f"({started.root}/tests/test.sh) > {verifier}/test-stdout.txt 2>&1;"
        " ls /x/tests a/tests /testsuite"
    )


def test_workdir_maps_to_the_workspace(tmp_path):
    environment = _environment(tmp_path, workdir="/app")
    asyncio.run(environment.start(force_build=False))
    try:
        assert environment.host_path("/app/src") == environment.workspace / "src"
        assert environment.translate("cd /app") == f"cd {environment.workspace}"
    finally:
        asyncio.run(environment.stop(delete=True))


def test_exec_sets_the_paths_and_runs_in_the_workspace(started):
    names = ["TASK", "WORKSPACE", "TESTS", "AGENT_LOGS", "VERIFIER_LOGS", "PYTHON"]
    values = " ".join(f'"$VIS_HARBOR_{name}"' for name in names)
    command = f'printf "%s\\n" "$(pwd -P)" {values} "$EXTRA"'
    result = asyncio.run(started.exec(command, env={"EXTRA": "value"}))
    assert result.return_code == 0
    assert result.stdout.splitlines() == [
        str(started.workspace),
        "task",
        str(started.workspace),
        str(started.root / "tests"),
        str(started.trial_paths.agent_dir),
        str(started.trial_paths.verifier_dir),
        sys.executable,
        "value",
    ]


def test_exec_maps_the_cwd_and_returns_the_exit_code(started):
    result = asyncio.run(started.exec("pwd -P; echo oops >&2; exit 3", cwd="/tests"))
    assert result.stdout.strip() == str(started.root / "tests")
    assert result.stderr.strip() == "oops"
    assert result.return_code == 3


def test_timeout_stops_the_process_group(started):
    pid_file = started.workspace / "child.pid"
    with pytest.raises(RuntimeError, match="Command timed out after 1 seconds"):
        asyncio.run(
            started.exec(f"sleep 30 & echo $! > {pid_file}; wait", timeout_sec=1)
        )
    _assert_stopped(int(pid_file.read_text()))


def test_cancellation_stops_the_process_group(started):
    pid_file = started.workspace / "child.pid"

    async def cancel_running_command():
        task = asyncio.create_task(
            started.exec(f"sleep 30 & echo $! > {pid_file}; wait")
        )
        for _ in range(100):
            if pid_file.is_file() and pid_file.read_text().strip():
                break
            await asyncio.sleep(0.05)
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await task

    asyncio.run(cancel_running_command())
    _assert_stopped(int(pid_file.read_text()))


def test_upload_and_download_use_the_host_directories(started, tmp_path):
    source = tmp_path / "tests-source"
    (source / "nested").mkdir(parents=True)
    (source / "test.sh").write_text("#!/usr/bin/env bash\n")
    (source / "nested" / "data.json").write_text("{}")
    asyncio.run(started.upload_dir(source, "/tests"))
    assert (started.root / "tests" / "nested" / "data.json").read_text() == "{}"
    asyncio.run(started.upload_file(source / "test.sh", "/solution/solve.sh"))
    assert (started.root / "solution" / "solve.sh").is_file()
    (started.trial_paths.verifier_dir / "reward.json").write_text('{"reward": 1}')
    out = tmp_path / "out"
    asyncio.run(
        started.download_file("/logs/verifier/reward.json", out / "reward.json")
    )
    assert json.loads((out / "reward.json").read_text()) == {"reward": 1}
    asyncio.run(started.download_dir("/tests", out / "tests"))
    assert (out / "tests" / "nested" / "data.json").is_file()
    asyncio.run(
        started.download_dir("/logs/verifier", started.trial_paths.verifier_dir)
    )
    assert (started.trial_paths.verifier_dir / "reward.json").is_file()


def test_workspace_filesystem_writes_vis_yml_before_the_first_commit(tmp_path):
    environment = _environment(
        tmp_path,
        files={"library/mod.py": "X = 1\n"},
        registrations={"library": "library"},
    )
    asyncio.run(environment.start(force_build=False))
    try:
        workspace = environment.workspace
        assert json.loads((workspace / "vis.yml").read_text()) == {
            "workspace": {
                "filesystem": [{"id": "library", "path": str(workspace / "library")}]
            },
            "jail": {"filesystem": {"allow": ["library"]}},
        }
        assert _git(workspace, "ls-files").split() == ["library/mod.py", "vis.yml"]
    finally:
        asyncio.run(environment.stop(delete=True))


def test_stop_deletes_the_trial_directory_only_on_request(tmp_path):
    environment = _environment(tmp_path)
    asyncio.run(environment.start(force_build=False))
    root = environment.root
    asyncio.run(environment.stop(delete=False))
    assert (root / "workspace" / "README.md").is_file()
    asyncio.run(environment.stop(delete=True))
    assert not root.exists()
    with pytest.raises(RuntimeError, match="not started"):
        environment.host_path("notes.txt")


def test_capabilities_and_definition(tmp_path):
    environment = _environment(tmp_path)
    assert environment.capabilities.mounted
    assert HostEnvironment.type() == "vis-host"
    shutil.rmtree(environment.environment_dir / "workspace")
    with pytest.raises(FileNotFoundError, match="no workspace directory"):
        environment._validate_definition()
