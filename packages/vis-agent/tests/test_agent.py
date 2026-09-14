"""Agent lifecycle and transport injection; real transports live in test_sdk_guide."""

from unittest.mock import Mock

import blockether.vis.engine as engine
import pytest


@pytest.fixture
def local(monkeypatch):
    def create(**options):
        layer = engine.LocalEngine(**options)
        layer.connect = Mock(return_value=layer)
        layer.create_session = Mock()
        layer.close = Mock()
        return layer

    factory = Mock(side_effect=create)
    monkeypatch.setattr("blockether.vis.engine._agent.LocalEngine", factory)
    return factory


@pytest.fixture
def remote():
    layer = engine.GatewayClient("https://gateway.example.com", token="test-token")
    layer.connect = Mock(return_value=layer)
    layer.create_session = Mock()
    layer.close = Mock()
    return layer


def test_default_project_is_resolved_before_chdir(local, monkeypatch, tmp_path):
    monkeypatch.chdir(tmp_path)
    agent = engine.Agent()
    monkeypatch.chdir(tmp_path.parent)
    assert agent.project == str(tmp_path.resolve())
    local.assert_called_once_with(root=".")
    layer = agent.execution_layer
    layer.connect.assert_not_called()
    with agent:
        assert agent.session is layer.create_session.return_value
        layer.create_session.assert_called_once_with(root=str(tmp_path.resolve()))
    layer.close.assert_called_once_with()


def test_followups_and_options_use_one_conversation(local, tmp_path):
    with engine.Agent(tmp_path) as agent:
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
    agent.execution_layer.connect.assert_called_once_with()
    agent.execution_layer.create_session.assert_called_once()


@pytest.mark.parametrize("status", ["completed", "failed", "cancelled", "suspended"])
def test_run_preserves_the_canonical_turn_record(local, tmp_path, status):
    result = {"status": status, "content": [], "turn_id": "turn"}
    agent = engine.Agent(tmp_path)
    agent.execution_layer.create_session.return_value.send.return_value.wait.return_value = result
    with agent:
        assert agent.run("Request") is result


@pytest.mark.parametrize("operation", ["connect", "create_session"])
def test_startup_failure_closes_owned_engine_and_prevents_reuse(
    local, tmp_path, operation
):
    agent = engine.Agent(tmp_path)
    layer = agent.execution_layer
    getattr(layer, operation).side_effect = engine.TransportError("startup")
    with pytest.raises(engine.TransportError, match="startup"):
        with agent:
            pytest.fail("startup should not succeed")
    layer.close.assert_called_once_with()
    with pytest.raises(engine.TransportError, match="closed"):
        agent.send("retry")
    agent.close()
    layer.close.assert_called_once_with()


def test_close_without_start_and_body_exception(local, tmp_path):
    agent = engine.Agent(tmp_path)
    agent.close()
    agent.close()
    agent.execution_layer.connect.assert_not_called()
    agent.execution_layer.close.assert_called_once_with()
    with pytest.raises(engine.TransportError, match="closed"):
        with agent:
            pass
    with pytest.raises(RuntimeError, match="body"):
        with engine.Agent(tmp_path) as other:
            raise RuntimeError("body")
    other.execution_layer.close.assert_called_once_with()


def test_wait_timeout_does_not_cancel_the_turn(local, tmp_path):
    with engine.Agent(tmp_path) as agent:
        turn = agent.session.send.return_value
        turn.wait.side_effect = engine.VisTimeout("deadline")
        with pytest.raises(engine.VisTimeout, match="deadline"):
            agent.run("Request", timeout=0.1)
        turn.cancel.assert_not_called()
        agent.execution_layer.close.assert_not_called()
    agent.execution_layer.close.assert_called_once_with()


def test_project_must_be_an_existing_directory(tmp_path):
    with pytest.raises(FileNotFoundError):
        engine.Agent(tmp_path / "missing")
    file = tmp_path / "file"
    file.touch()
    with pytest.raises(NotADirectoryError):
        engine.Agent(file)


def test_missing_launcher_has_no_surviving_process(tmp_path):
    layer = engine.LocalEngine(root=tmp_path, executable=str(tmp_path / "missing"))
    agent = engine.Agent(tmp_path, execution_layer=layer)
    with pytest.raises(engine.TransportError):
        with agent:
            pass
    assert layer._process is None
    agent.close()
    layer.close()


def test_remote_agent_needs_no_local_project_or_engine(local, remote):
    agent = engine.Agent("/srv/vis-project", execution_layer=remote)
    assert agent.project == "/srv/vis-project"
    assert agent.execution_layer is remote
    local.assert_not_called()
    remote.connect.assert_not_called()
    with agent:
        conversation = agent.session
        remote.create_session.assert_called_once_with(
            root="/srv/vis-project", channel="app"
        )
        result = agent.run("First", timeout=42, provider="provider", model="model")
        assert result is conversation.send.return_value.wait.return_value
        conversation.send.assert_called_once_with(
            "First", provider="provider", model="model"
        )
        conversation.send.return_value.wait.assert_called_once_with(timeout=42)
        assert agent.send("Follow up") is conversation.send.return_value
        conversation.send.assert_called_with("Follow up")
    remote.connect.assert_called_once_with()
    remote.create_session.assert_called_once()
    remote.close.assert_not_called()
    conversation.delete.assert_not_called()
    conversation.send.return_value.cancel.assert_not_called()
    agent.close()
    remote.close.assert_not_called()
    with pytest.raises(engine.TransportError, match="closed"):
        agent.send("Retry")


@pytest.mark.parametrize("operation", ["connect", "create_session"])
def test_remote_startup_failure_does_not_close_borrowed_client(remote, operation):
    getattr(remote, operation).side_effect = engine.TransportError("startup")
    agent = engine.Agent("/srv/vis-project", execution_layer=remote)
    with pytest.raises(engine.TransportError, match="startup"):
        with agent:
            pytest.fail("startup should not succeed")
    remote.close.assert_not_called()
    with pytest.raises(engine.TransportError, match="closed"):
        agent.run("Retry")


def test_remote_wait_timeout_and_context_error_leave_borrowed_client_open(remote):
    with pytest.raises(engine.VisTimeout, match="deadline"):
        with engine.Agent("/srv/vis-project", execution_layer=remote) as agent:
            turn = agent.session.send.return_value
            turn.wait.side_effect = engine.VisTimeout("deadline")
            agent.run("Request", timeout=0.1)
    turn.cancel.assert_not_called()
    remote.close.assert_not_called()
    remote.create_session.return_value.delete.assert_not_called()


def test_remote_close_before_connect_leaves_borrowed_client_open(remote):
    agent = engine.Agent("/srv/vis-project", execution_layer=remote)
    agent.close()
    agent.close()
    remote.connect.assert_not_called()
    remote.close.assert_not_called()


@pytest.mark.parametrize("project", [".", "relative/project", "", "~/project"])
def test_remote_project_must_be_explicit_absolute_path(local, remote, project):
    with pytest.raises(ValueError, match="absolute.*gateway"):
        engine.Agent(project, execution_layer=remote)
    local.assert_not_called()
    remote.connect.assert_not_called()


@pytest.mark.parametrize(
    "options",
    [
        {"token": "test-token"},
        {"gateway_url": "https://gateway.example.com"},
        {"executable": "custom-vis"},
        {"startup_timeout": 10},
        {"timeout": 10},
    ],
)
def test_transport_options_are_not_agent_options(local, tmp_path, options):
    with pytest.raises(TypeError):
        engine.Agent(tmp_path, **options)
    local.assert_not_called()


def test_shared_layer_keeps_distinct_agents_alive(remote):
    first_session, second_session = Mock(), Mock()
    remote.create_session.side_effect = [first_session, second_session]
    first = engine.Agent("/srv/vis-project", execution_layer=remote)
    second = engine.Agent("/srv/vis-project", execution_layer=remote)
    assert first.session is first_session
    assert second.session is second_session
    first.close()
    second.run("Still available")
    second_session.send.assert_called_once_with("Still available")
    second.close()
    remote.close.assert_not_called()


def test_local_layer_is_borrowed_too(tmp_path, monkeypatch):
    layer = engine.LocalEngine(root=tmp_path)
    close = Mock()
    monkeypatch.setattr(layer, "close", close)
    engine.Agent(tmp_path, execution_layer=layer).close()
    close.assert_not_called()
    assert not layer._closed


def test_agent_accepts_transport_independent_layer(local):
    class InMemory(engine.ExecutionLayer):
        def session_options(self, project):
            return {"root": "memory:" + project, "channel": "test"}

        def connect(self):
            return self

        def close(self):
            raise AssertionError("borrowed layer must not be closed")

        def _open(self, *args, **kwargs):
            raise AssertionError("no transport needed by this test")

        def create_session(self, **options):
            assert options == {"root": "memory:project-name", "channel": "test"}
            return Mock()

    layer = InMemory()
    with engine.Agent("project-name", execution_layer=layer) as agent:
        assert agent.project == "memory:project-name"
        agent.run("A transport-neutral request")
    local.assert_not_called()


def test_invalid_execution_layer_is_rejected_before_default_engine(local):
    with pytest.raises(TypeError, match="ExecutionLayer"):
        engine.Agent(execution_layer=object())
    local.assert_not_called()
