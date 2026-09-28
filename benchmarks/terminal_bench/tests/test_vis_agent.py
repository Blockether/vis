"""Configuration and accounting checks for the Harbor Vis adapter."""

import asyncio
import gzip
import json
import os
import shlex
import subprocess
import sys
from pathlib import Path
from unittest.mock import AsyncMock

import pytest
import vis_agent
from harbor.models.agent.context import AgentContext
from vis_agent import BUNDLE, MODEL, VisAgent, populate_metrics, result_frame


def test_bundle_matches_published_amd64_task_images():
    assert BUNDLE.name == "vis-agent-linux-amd64.tar.gz"


def test_command_is_fixed_and_quotes_the_instruction():
    command = VisAgent.command("Fix 'quoted' text; echo secret")
    assert "--model " + MODEL in command
    assert "providers:" in command
    assert "id: zai-coding-plan" in command
    assert "default_model: glm-5.3-flash" in command
    assert "/.vis/config.yml" in command
    assert "--toggles council=false,draft_backend=off" in command
    assert "--db :memory" in command
    assert "--full-trace-json-stream" in command
    assert "set -o pipefail;" in command
    assert vis_agent.REMOTE_PYTHON + " /installed-agent/capture_trace.py" in command
    assert "--stdout /logs/agent/vis-trace.jsonl.gz" in command
    assert "--stderr /logs/agent/vis-stderr.log" in command
    assert shlex.quote("Fix 'quoted' text; echo secret") in command
    assert "ZAI_CODING_API_KEY" not in command


def test_result_and_subscription_metrics(tmp_path):
    log = tmp_path / "vis-trace.jsonl.gz"
    payload = {
        "tokens": {"input": 100, "cached": 20, "output": 30},
        "cost": {"total_cost": 0.04},
        "duration-ms": 1200,
        "iteration-count": 3,
    }
    with gzip.open(log, "wt", encoding="utf-8") as stream:
        stream.write(
            "bad event\n"
            + json.dumps({"event": "trace-chunk"})
            + "\n"
            + json.dumps({"event": "result", "payload": payload})
            + "\n"
        )
    context = AgentContext()
    populate_metrics(context, result_frame(log))
    assert (
        context.n_input_tokens,
        context.n_cache_tokens,
        context.n_output_tokens,
    ) == (100, 20, 30)
    assert context.cost_usd is None
    assert context.metadata["vis"]["estimated_metered_api_cost_usd"] == 0.04
    assert context.metadata["vis"]["duration_ms"] == 1200


def test_missing_trace_does_not_fabricate_usage(tmp_path):
    assert result_frame(tmp_path / "no-trace.jsonl.gz") is None
    context = AgentContext()
    populate_metrics(context, {"tokens": {"input": -1, "cached": True}})
    assert context.n_input_tokens is None
    assert context.n_cache_tokens is None
    assert context.cost_usd is None


def test_truncated_trace_does_not_fabricate_a_result(tmp_path):
    log = tmp_path / "vis-trace.jsonl.gz"
    with gzip.open(log, "wt", encoding="utf-8") as stream:
        stream.write(
            json.dumps({"event": "result", "payload": {"tokens": {"input": 1}}}) + "\n"
        )
    log.write_bytes(log.read_bytes()[:-8])
    assert result_frame(log) is None


def test_model_must_be_pinned(tmp_path):
    with pytest.raises(ValueError, match="requires --model"):
        VisAgent(logs_dir=tmp_path, model_name="another/model")


def test_key_is_passed_to_exec_only(tmp_path, monkeypatch):
    monkeypatch.setenv("ZAI_CODING_API_KEY", "private-test-token")
    agent = VisAgent(logs_dir=tmp_path, model_name=MODEL)
    agent.exec_as_agent = AsyncMock()
    asyncio.run(agent.run("Task", object(), AgentContext()))
    assert agent.exec_as_agent.await_args.kwargs["env"] == {
        "ZAI_CODING_API_KEY": "private-test-token"
    }
    assert "private-test-token" not in agent.exec_as_agent.await_args.kwargs["command"]


def test_command_redacts_both_streams_before_storage(tmp_path, monkeypatch):
    key = "fixture-zai-credential-12345"
    monkeypatch.setenv("ZAI_CODING_API_KEY", key)
    remote = tmp_path / "fake-vis"
    remote.write_text(
        f"#!{sys.executable}\n"
        "import json, os, sys\n"
        "key = os.environ['ZAI_CODING_API_KEY']\n"
        "print(json.dumps({'event': 'result', 'payload': {'status': 'completed', "
        "'output': key}}))\n"
        "print('provider error: ' + key, file=sys.stderr)\n"
        "sys.exit(7)\n"
    )
    remote.chmod(0o755)
    monkeypatch.setattr(vis_agent, "REMOTE", str(remote))
    monkeypatch.setattr(vis_agent, "REMOTE_PYTHON", sys.executable)
    monkeypatch.setattr(vis_agent, "HOME", str(tmp_path / "home"))
    command = VisAgent.command("task")
    command = command.replace("/logs/agent", str(tmp_path / "logs"))
    command = command.replace(
        "/installed-agent/capture_trace.py",
        str(Path(vis_agent.__file__).with_name("capture_trace.py")),
    )
    completed = subprocess.run(
        ["bash", "-c", command], capture_output=True, env=os.environ.copy()
    )
    assert completed.returncode == 7
    assert key.encode() not in completed.stdout + completed.stderr
    with gzip.open(tmp_path / "logs/vis-trace.jsonl.gz", "rt") as stream:
        trace = stream.read()
    stderr = (tmp_path / "logs/vis-stderr.log").read_text()
    assert key not in trace + stderr
    assert "[REDACTED]" in trace and "[REDACTED]" in stderr
    assert json.loads(trace)["payload"]["status"] == "completed"


def test_credentials_cannot_enter_logged_command_arguments(tmp_path, monkeypatch):
    key = "fixture-zai-credential-12345"
    monkeypatch.setenv("ZAI_CODING_API_KEY", key)
    agent = VisAgent(logs_dir=tmp_path, model_name=MODEL)
    agent.exec_as_agent = AsyncMock()
    with pytest.raises(ValueError, match="must not contain credentials"):
        asyncio.run(agent.run("echo " + key, object(), AgentContext()))
    agent.exec_as_agent.assert_not_called()


def test_metadata_redacts_credentials(tmp_path, monkeypatch):
    key = "fixture-zai-credential-12345"
    monkeypatch.setenv("ZAI_CODING_API_KEY", key)
    context = AgentContext()
    populate_metrics(context, {"eval": {"output": key}, "status": key})
    assert key not in json.dumps(context.metadata)
    assert context.metadata["vis"]["eval"]["output"] == "[REDACTED]"


def test_install_uploads_capture_wrapper_and_uses_bundled_python(tmp_path, monkeypatch):
    bundle = tmp_path / "bundle.tar.gz"
    bundle.write_bytes(b"fixture bundle")
    monkeypatch.setattr(vis_agent, "BUNDLE", bundle)
    agent = VisAgent(logs_dir=tmp_path, model_name=MODEL)
    agent.exec_as_root = AsyncMock()
    environment = AsyncMock()
    asyncio.run(agent.install(environment))
    uploads = environment.upload_file.await_args_list
    assert uploads[0].args == (bundle, "/tmp/vis-benchmark.tar.gz")
    assert uploads[1].args[0].name == "capture_trace.py"
    assert uploads[1].args[1] == "/installed-agent/capture_trace.py"
    command = agent.exec_as_root.await_args.kwargs["command"]
    assert vis_agent.REMOTE_PYTHON + " --version" in command
    assert command.index("tar -xzf") < command.index(vis_agent.REMOTE_PYTHON)
