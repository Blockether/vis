"""Configuration and accounting checks for the Harbor Vis adapter."""

import asyncio
import gzip
import json
import shlex
from unittest.mock import AsyncMock

import pytest
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
    assert "| gzip -1 > /logs/agent/vis-trace.jsonl.gz" in command
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
