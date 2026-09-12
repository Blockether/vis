"""Project-local Agent lifecycle and delegation; real transports live in test_sdk_guide."""

from unittest.mock import Mock

import blockether.vis.engine as engine
import pytest


@pytest.fixture
def local(monkeypatch):
    factory = Mock()
    monkeypatch.setattr("blockether.vis.engine._agent.LocalEngine", factory)
    return factory


def test_default_project_is_resolved_before_chdir(local, monkeypatch, tmp_path):
    monkeypatch.chdir(tmp_path)
    agent = engine.Agent()
    monkeypatch.chdir(tmp_path.parent)
    assert agent.project == tmp_path.resolve()
    local.assert_called_once_with(
        executable="vis-agent", root=tmp_path.resolve(), timeout=30, startup_timeout=120
    )
    local.return_value.connect.assert_not_called()
    with agent:
        assert agent.session is local.return_value.create_session.return_value
        local.return_value.create_session.assert_called_once_with(
            root=str(tmp_path.resolve())
        )
    local.return_value.close.assert_called_once_with()


def test_followups_and_options_use_one_conversation(local, tmp_path):
    with engine.Agent(tmp_path, executable=["custom-vis", "--jvm"]) as agent:
        conversation = agent.session
        result = agent.run("First", timeout=42, provider="provider", model="model")
        conversation.send.assert_called_once_with(
            "First", provider="provider", model="model"
        )
        conversation.send.return_value.wait.assert_called_once_with(timeout=42)
        assert result is conversation.send.return_value.wait.return_value
        turn = agent.send("Follow up", idempotency_key="same-request")
        assert turn is conversation.send.return_value
        conversation.send.assert_called_with(
            "Follow up", idempotency_key="same-request"
        )
    local.return_value.connect.assert_called_once_with()
    local.return_value.create_session.assert_called_once()


@pytest.mark.parametrize("status", ["completed", "failed", "cancelled", "suspended"])
def test_run_preserves_the_canonical_turn_record(local, tmp_path, status):
    result = {"status": status, "content": [], "turn_id": "turn"}
    local.return_value.create_session.return_value.send.return_value.wait.return_value = result
    with engine.Agent(tmp_path) as agent:
        assert agent.run("Request") is result


@pytest.mark.parametrize("operation", ["connect", "create_session"])
def test_startup_failure_closes_engine_and_prevents_reuse(local, tmp_path, operation):
    getattr(local.return_value, operation).side_effect = engine.TransportError(
        "startup"
    )
    agent = engine.Agent(tmp_path)
    with pytest.raises(engine.TransportError, match="startup"):
        with agent:
            pytest.fail("startup should not succeed")
    local.return_value.close.assert_called_once_with()
    with pytest.raises(engine.TransportError, match="closed"):
        agent.send("retry")
    agent.close()
    local.return_value.close.assert_called_once_with()


def test_close_without_start_and_body_exception(local, tmp_path):
    agent = engine.Agent(tmp_path)
    agent.close()
    agent.close()
    local.return_value.connect.assert_not_called()
    local.return_value.close.assert_called_once_with()
    with pytest.raises(engine.TransportError, match="closed"):
        with agent:
            pass
    local.reset_mock()
    with pytest.raises(RuntimeError, match="body"):
        with engine.Agent(tmp_path):
            raise RuntimeError("body")
    local.return_value.close.assert_called_once_with()


def test_wait_timeout_does_not_cancel_the_turn(local, tmp_path):
    with engine.Agent(tmp_path) as agent:
        turn = agent.session.send.return_value
        turn.wait.side_effect = engine.VisTimeout("deadline")
        with pytest.raises(engine.VisTimeout, match="deadline"):
            agent.run("Request", timeout=0.1)
        turn.cancel.assert_not_called()
        local.return_value.close.assert_not_called()
    local.return_value.close.assert_called_once_with()


def test_project_must_be_an_existing_directory(tmp_path):
    with pytest.raises(FileNotFoundError):
        engine.Agent(tmp_path / "missing")
    file = tmp_path / "file"
    file.touch()
    with pytest.raises(NotADirectoryError):
        engine.Agent(file)


def test_missing_launcher_has_no_surviving_process(tmp_path):
    agent = engine.Agent(tmp_path, executable=str(tmp_path / "missing"))
    with pytest.raises(engine.TransportError):
        with agent:
            pass
    agent.close()
