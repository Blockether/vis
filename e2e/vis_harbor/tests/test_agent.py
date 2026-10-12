"""Tests for the Harbor agent that runs the Vis CLI on the host.

The integration tests run ``harbor run`` with a stub engine. They need no
network and no model.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
import time
from pathlib import Path

import pytest
from harbor.models.agent.context import AgentContext
from harbor.models.task.config import AgentConfig, TaskConfig, VerifierConfig

from vis_harbor import agent as agent_module
from vis_harbor.agent import (
    DEFAULT_MODEL,
    DEFAULT_PROVIDER,
    RUN_FILE,
    TRACE_FILE,
    TRAJECTORY_FILE,
    VisSourceAgent,
    check_native_binary,
    source_classpath,
)
from vis_harbor.environment import VIS_ROOT

HARBOR = Path(sys.executable).with_name("harbor")
SETTINGS = (
    "VIS_PROVIDER",
    "VIS_MODEL",
    "VIS_REASONING_EFFORT",
    "VIS_E2E_CLOJURE",
    "VIS_E2E_NATIVE_BIN",
)
TRAJECTORY = {
    "schema_version": "ATIF-v1.6",
    "session_id": "stub-session",
    "agent": {
        "name": "vis",
        "version": "dev",
        "model_name": "stub-provider/stub-model",
    },
    "steps": [
        {"step_id": 1, "source": "user", "message": "prompt"},
        {
            "step_id": 2,
            "source": "agent",
            "message": "done",
            "model_name": "stub-provider/stub-model",
            "metrics": {
                "prompt_tokens": 120,
                "completion_tokens": 30,
                "cached_tokens": 100,
                "cost_usd": 0.01,
            },
        },
    ],
    "final_metrics": {
        "total_prompt_tokens": 120,
        "total_completion_tokens": 30,
        "total_cached_tokens": 100,
        "total_cost_usd": 0.01,
        "total_steps": 2,
    },
}
STUB_ENGINE = """\
import json
import os
import sys
import time
from pathlib import Path

arguments = sys.argv[1:]
here = Path(__file__).parent
if arguments[-1].startswith("sleep"):
    (here / "pid").write_text(str(os.getpid()))
    time.sleep(60)
trajectory = Path(arguments[arguments.index("--trajectory") + 1])
trajectory.write_text((here / "trajectory.json").read_text())
seen = {
    "arguments": arguments,
    "cwd": os.getcwd(),
    "db_path": os.environ.get("VIS_DB_PATH"),
    "gateway_url": os.environ.get("VIS_GATEWAY_URL"),
}
print(json.dumps(seen))
"""
VERIFIER = """\
#!/usr/bin/env bash
set -euo pipefail
"$VIS_E2E_PYTHON" - <<'PY'
import json
import os
from pathlib import Path

logs = Path(os.environ["VIS_E2E_AGENT_LOGS"])
run = json.loads((logs / "run.json").read_text())
passed = run["exit_code"] == 0 and (logs / "trace.jsonl").stat().st_size > 0
reward = Path(os.environ["VIS_E2E_VERIFIER_LOGS"]) / "reward.json"
reward.write_text(json.dumps({"reward": 1.0 if passed else 0.0}))
PY
"""


@pytest.fixture
def clean_settings(monkeypatch):
    for name in SETTINGS:
        monkeypatch.delenv(name, raising=False)


def make_agent(tmp_path: Path, model_name: str | None = None, **kwargs):
    return VisSourceAgent(logs_dir=tmp_path / "agent", model_name=model_name, **kwargs)


def write_task(root: Path, instruction: str, agent_timeout_sec: float = 60.0) -> Path:
    task = root / "stub-task"
    (task / "environment" / "workspace").mkdir(parents=True)
    (task / "environment" / "workspace" / "README.md").write_text("seed\n")
    (task / "tests").mkdir()
    config = TaskConfig(
        agent=AgentConfig(timeout_sec=agent_timeout_sec),
        verifier=VerifierConfig(timeout_sec=60.0),
    )
    (task / "task.toml").write_text(config.model_dump_toml())
    (task / "instruction.md").write_text(instruction)
    verifier = task / "tests" / "test.sh"
    verifier.write_text(VERIFIER)
    verifier.chmod(0o755)
    return task


def run_harbor(tmp_path: Path, task: Path) -> tuple[dict, Path]:
    stub = tmp_path / "stub"
    stub.mkdir()
    (stub / "engine.py").write_text(STUB_ENGINE)
    (stub / "trajectory.json").write_text(json.dumps(TRAJECTORY))
    jobs = tmp_path / "jobs"
    env = {k: v for k, v in os.environ.items() if k not in SETTINGS}
    env["VIS_GATEWAY_URL"] = "http://127.0.0.1:9"
    completed = subprocess.run(
        [
            str(HARBOR),
            "run",
            "--path",
            str(task),
            "--agent",
            "vis_harbor.agent:VisSourceAgent",
            "--env",
            "vis_harbor.environment:HostEnvironment",
            "--model",
            "stub-provider/stub-model",
            "--ak",
            f"command={sys.executable} {stub / 'engine.py'}",
            "--jobs-dir",
            str(jobs),
            "--n-concurrent",
            "1",
            "--quiet",
        ],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        timeout=300,
    )
    assert completed.returncode == 0, (
        completed.stdout[-3000:] + completed.stderr[-3000:]
    )
    (result,) = jobs.glob("*/*/result.json")
    return json.loads(result.read_text()), result.parent


def test_model_name_sets_the_provider_and_the_model(tmp_path, clean_settings):
    agent = make_agent(tmp_path, "openrouter/anthropic/claude-x")
    assert (agent.provider, agent.model) == ("openrouter", "anthropic/claude-x")
    info = agent.to_agent_info().model_info
    assert (info.provider, info.name) == ("openrouter", "anthropic/claude-x")


def test_options_win_over_the_model_name_and_the_environment(
    tmp_path, monkeypatch, clean_settings
):
    monkeypatch.setenv("VIS_PROVIDER", "env-provider")
    monkeypatch.setenv("VIS_MODEL", "env-model")
    agent = make_agent(tmp_path, "name-provider/name-model", provider="option-provider")
    assert (agent.provider, agent.model) == ("option-provider", "name-model")
    assert agent.model_name == "option-provider/name-model"


def test_environment_and_defaults_fill_missing_settings(
    tmp_path, monkeypatch, clean_settings
):
    agent = make_agent(tmp_path)
    assert (agent.provider, agent.model, agent.reasoning_effort) == (
        DEFAULT_PROVIDER,
        DEFAULT_MODEL,
        None,
    )
    monkeypatch.setenv("VIS_MODEL", "env-model")
    monkeypatch.setenv("VIS_REASONING_EFFORT", " high ")
    agent = make_agent(tmp_path, extra_env={"VIS_PROVIDER": "extra-provider"})
    assert (agent.provider, agent.model, agent.reasoning_effort) == (
        "extra-provider",
        "env-model",
        "high",
    )


def test_unknown_option_is_an_error(tmp_path, clean_settings):
    with pytest.raises(ValueError, match="Unknown option 'modle'"):
        make_agent(tmp_path, modle="x")


def test_shell_command_keeps_paths_and_the_prompt_in_variables(
    tmp_path, clean_settings
):
    agent = make_agent(tmp_path, "p/m", reasoning_effort="high")
    command = agent.shell_command()
    assert command.startswith(
        'unset VIS_GATEWAY_URL; cd "$VIS_ROOT" && '
        '"$VIS_E2E_ENGINE" -Scp "$VIS_E2E_CLASSPATH" '
        '-J-Duser.dir="$VIS_E2E_WORKSPACE" -M:vis --full-trace-json-stream'
    )
    assert (
        "--provider p --model m --reasoning-effort high --persist --trajectory"
        in command
    )
    assert command.endswith(
        '"$VIS_E2E_PROMPT" > "$VIS_E2E_AGENT_LOGS/trace.jsonl" '
        '2> "$VIS_E2E_AGENT_LOGS/stderr.txt"'
    )
    assert str(tmp_path) not in command


def test_native_binary_replaces_the_source_engine(tmp_path, clean_settings):
    binary = tmp_path / "vis"
    binary.write_bytes(b"\x7fELF" + bytes(12))
    agent = make_agent(tmp_path, native_bin=str(binary))
    assert (
        '"$VIS_E2E_ENGINE" -Duser.dir="$VIS_E2E_WORKSPACE" --full-trace-json-stream'
        in agent.shell_command()
    )
    assert agent.process_env("go", tmp_path)["VIS_E2E_ENGINE"] == str(binary)
    assert agent.version() is None
    check_native_binary(str(binary))
    launcher = tmp_path / "launcher"
    launcher.write_text("#!/bin/sh\n")
    with pytest.raises(ValueError, match="raw native executable"):
        check_native_binary(str(launcher))


def test_process_env_isolates_the_database(tmp_path, monkeypatch, clean_settings):
    monkeypatch.delenv("VIS_EXTENSIONS_DIR", raising=False)
    agent = make_agent(tmp_path, extra_env={"EXTRA": "1"})
    env = agent.process_env("hello", tmp_path / "runtime")
    assert env["VIS_DB_PATH"] == str(tmp_path / "runtime" / "vis.mdb")
    assert env["VIS_EXTENSIONS_DIR"] == str(tmp_path / "runtime" / "extensions")
    assert (env["VIS_E2E_PROMPT"], env["EXTRA"]) == ("hello", "1")
    monkeypatch.setenv("VIS_EXTENSIONS_DIR", "/opt/extensions")
    assert (
        agent.process_env("hello", tmp_path)["VIS_EXTENSIONS_DIR"] == "/opt/extensions"
    )


def test_source_classpath_makes_relative_entries_absolute(monkeypatch):
    calls = []

    def check_output(command, **kwargs):
        calls.append((command, kwargs["cwd"]))
        return f"src{os.pathsep}/abs/lib.jar\n"

    monkeypatch.setattr(agent_module.subprocess, "check_output", check_output)
    source_classpath.cache_clear()
    try:
        expected = f"{VIS_ROOT / 'src'}{os.pathsep}/abs/lib.jar"
        assert source_classpath("clj") == expected
        assert source_classpath("clj") == expected
    finally:
        source_classpath.cache_clear()
    assert calls == [(["clj", "-Spath", "-A:vis"], VIS_ROOT)]


def test_context_gets_the_trajectory_totals(tmp_path, clean_settings):
    agent = make_agent(tmp_path)
    context = AgentContext()
    agent.populate_context_post_run(context)
    assert context.is_empty()
    agent.logs_dir.mkdir()
    (agent.logs_dir / TRAJECTORY_FILE).write_text("{")
    agent.populate_context_post_run(context)
    assert context.is_empty()
    (agent.logs_dir / TRAJECTORY_FILE).write_text(json.dumps(TRAJECTORY))
    agent.populate_context_post_run(context)
    assert (
        context.n_input_tokens,
        context.n_cache_tokens,
        context.n_output_tokens,
        context.cost_usd,
    ) == (120, 100, 30, 0.01)


def test_a_failed_trajectory_read_does_not_stop_the_job(
    tmp_path, clean_settings, monkeypatch
):
    # Harbor runs this hook outside the error handling of a trial.
    def broken_reader(path, *, strict):
        raise TypeError("unexpected keyword argument")

    agent = make_agent(tmp_path)
    agent.logs_dir.mkdir()
    (agent.logs_dir / TRAJECTORY_FILE).write_text(json.dumps(TRAJECTORY))
    monkeypatch.setattr(agent_module, "load_vis_trajectory", broken_reader)
    context = AgentContext()
    agent.populate_context_post_run(context)
    assert context.is_empty()


def test_harbor_runs_one_trial_on_the_host(tmp_path):
    instruction = "Read /logs/agent/notes.txt and /tests/scenario.json.\n"
    result, trial = run_harbor(tmp_path, write_task(tmp_path, instruction))
    assert result["verifier_result"]["rewards"] == {"reward": 1.0}
    agent_result = result["agent_result"]
    assert (
        agent_result["n_input_tokens"],
        agent_result["n_cache_tokens"],
        agent_result["n_output_tokens"],
        agent_result["cost_usd"],
    ) == (120, 100, 30, 0.01)
    assert result["agent_info"]["model_info"] == {
        "name": "stub-model",
        "provider": "stub-provider",
    }
    logs = trial / "agent"
    record = json.loads((logs / RUN_FILE).read_text())
    assert (record["exit_code"], record["is_cancelled"]) == (0, False)
    seen = json.loads((logs / TRACE_FILE).read_text())
    arguments = seen["arguments"]
    assert arguments[:7] == [
        "--full-trace-json-stream",
        "--provider",
        "stub-provider",
        "--model",
        "stub-model",
        "--persist",
        "--trajectory",
    ]
    assert Path(arguments[7]).resolve() == (logs / TRAJECTORY_FILE).resolve()
    assert arguments[8:] == [instruction]
    assert Path(seen["cwd"]) == VIS_ROOT
    assert seen["gateway_url"] is None
    assert seen["db_path"].endswith("vis.mdb")
    assert not Path(seen["db_path"]).parent.exists()


def test_agent_timeout_stops_the_engine(tmp_path):
    started = time.monotonic()
    result, trial = run_harbor(tmp_path, write_task(tmp_path, "sleep\n", 3.0))
    assert time.monotonic() - started < 50
    assert result["exception_info"]["exception_type"] == "AgentTimeoutError"
    record = json.loads((trial / "agent" / RUN_FILE).read_text())
    assert (record["exit_code"], record["is_cancelled"]) == (None, True)
    pid = int((tmp_path / "stub" / "pid").read_text())
    with pytest.raises(ProcessLookupError):
        os.kill(pid, 0)
