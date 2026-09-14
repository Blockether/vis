"""Run public engine docstring examples without a process, gateway or model call."""

import inspect
import re
from unittest.mock import MagicMock

import blockether.vis.engine as engine
import pytest


@pytest.mark.parametrize(
    "document",
    [engine.LocalEngine, engine.GatewayClient],
    ids=["local-engine", "gateway-client"],
)
def test_engine_docstring_examples_use_public_api(document, monkeypatch, capsys):
    examples = re.findall(r"```python\n(.*?)\n```", inspect.getdoc(document), re.S)
    assert len(examples) == 1

    layer = MagicMock(spec=engine.ExecutionLayer)
    layer._streams = set()
    layer._client_extensions = {}
    layer._closed = False
    layer.session_options.return_value = {"root": "/workspace/project"}
    layer.__enter__.return_value = layer
    layer.__exit__.side_effect = lambda *_: layer.close()
    layer.close.return_value = None
    conversation = layer.create_session.return_value
    conversation.id = "doc-session"
    conversation.send.return_value.wait.return_value = {
        "turn_id": "doc-turn",
        "status": "completed",
        "content": [],
    }
    factory = MagicMock(return_value=layer)
    monkeypatch.setattr(engine, "LocalEngine", factory)
    monkeypatch.setattr(engine, "GatewayClient", factory)
    monkeypatch.setattr("blockether.vis.engine._agent.LocalEngine", factory)
    monkeypatch.setenv("VIS_GATEWAY_TOKEN", "test-token")

    def unexpected_io(*_args, **_kwargs):
        pytest.fail("Documentation example attempted real IO")

    monkeypatch.setattr("socket.create_connection", unexpected_io)
    monkeypatch.setattr("subprocess.Popen", unexpected_io)
    exec(compile(examples[0], "<engine-docstring>", "exec"), {})

    assert capsys.readouterr().out == "completed\n"
    conversation.send.assert_called_once_with(
        "Summarize this project without changing files."
    )
    conversation.send.return_value.wait.assert_called_once_with(timeout=300)
    layer.close.assert_called_once_with()
    assert layer._client_extensions == {}
