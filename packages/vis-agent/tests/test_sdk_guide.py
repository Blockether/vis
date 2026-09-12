"""Execute the SDK guide's examples without a model or a user's gateway."""

import json
import re
import sys
from contextlib import contextmanager
from pathlib import Path
from types import ModuleType
from urllib.parse import parse_qs, urlsplit

import blockether.vis.engine as engine
import pytest
from test_client import compatible, endpoint


@pytest.fixture
def recipe(monkeypatch):
    document = Path(__file__).parents[3] / "resources/vis-docs/python-sdk.md"

    def load(name):
        match = re.search(
            rf"```python\n# {re.escape(name)}.py\n(.*?)\n```",
            document.read_text(),
            re.S,
        )
        assert match, f"Missing executable example: {name}"
        module = ModuleType(name)
        monkeypatch.setitem(sys.modules, name, module)
        exec(compile(match[1], str(document), "exec"), module.__dict__)
        return module

    return load


@pytest.fixture
def gateway():
    submissions = []
    result = {"turn_id": "turn-one", "status": "completed", "content": []}

    def respond(method, path, body):
        if response := compatible(method, path, body):
            return response
        if path == "/v1/sessions" and method == "POST":
            submissions.append(json.loads(body))
            return 201, {"id": "session-one"}
        if path == "/v1/sessions/session-one/seq":
            return 200, {"seq": 3}
        if path == "/v1/sessions/session-one/turns" and method == "POST":
            submissions.append(json.loads(body))
            return 202, {"turn_id": "turn-one", "status": "queued"}
        if path == "/v1/sessions/session-one/turns/turn-one":
            return 200, result
        if path.startswith("/v1/sessions/session-one/transcript"):
            return 200, b"# SDK session\n", "text/markdown"
        if path.startswith("/v1/events?"):
            assert parse_qs(urlsplit(path).query)["sids"] == ["session-one:3"]
            events = [
                {
                    "type": "subscription.ready",
                    "session_id": "session-one",
                    "cursor": 3,
                },
                {
                    "type": "turn.completed",
                    "session_id": "session-one",
                    "turn_id": "other-turn",
                    "seq": 4,
                },
                {
                    "type": "turn.completed",
                    "session_id": "session-one",
                    "turn_id": "turn-one",
                    "seq": 5,
                },
            ]
            frames = b"".join(
                ("data: " + json.dumps(event) + "\n\n").encode() for event in events
            )
            return 200, frames, "text/event-stream"
        raise AssertionError(f"Unexpected request: {method} {path}")

    with endpoint(respond) as (url, calls):
        yield url, calls, submissions, result


def test_gateway_recipe_sends_a_task_and_releases_only_its_lease(
    recipe, gateway, monkeypatch, capsys
):
    url, calls, submissions, result = gateway
    result["content"] = [{"type": "text", "text": "Project summary"}]
    monkeypatch.setenv("VIS_GATEWAY_URL", url)
    monkeypatch.setenv("VIS_GATEWAY_TOKEN", "fixture-token")
    monkeypatch.setenv("VIS_PROJECT_ROOT", "/srv/vis-project")
    recipe("gateway_task").main()
    assert submissions[0] == {
        "root": "/srv/vis-project",
        "title": "SDK task",
        "channel": "app",
    }
    assert submissions[1]["request"] == "Summarize this project without changing files."
    assert submissions[1]["idempotency_key"]
    assert calls[-1][:2] == ("DELETE", "/v1/clients/sdk-lease")
    assert {key.lower(): value for key, value in calls[2][2].items()}[
        "authorization"
    ] == "Bearer fixture-token"
    output = capsys.readouterr().out
    assert "Session: session-one" in output
    assert "Project summary" in output
    assert "fixture-token" not in output


@pytest.mark.parametrize("status", ["failed", "cancelled", "suspended"])
def test_agent_wrapper_does_not_treat_every_settled_turn_as_success(
    recipe, gateway, status
):
    url, calls, _, result = gateway
    result["status"] = status
    with engine.GatewayClient(url) as client:
        with pytest.raises(RuntimeError, match=f"Turn turn-one: {status}"):
            recipe("gateway_task").run_task(client, "/srv/vis-project", "Summarize")
    assert calls[-1][:2] == ("DELETE", "/v1/clients/sdk-lease")


def test_local_recipe_exports_before_its_owned_context_closes(
    recipe, gateway, monkeypatch, tmp_path
):
    url, _, _, _ = gateway
    monkeypatch.chdir(tmp_path)
    monkeypatch.setenv("VIS_EXECUTABLE", "/opt/vis/vis-agent")
    lifecycle = []

    @contextmanager
    def local_engine(*, executable, root, startup_timeout):
        assert executable == "/opt/vis/vis-agent"
        assert root == tmp_path.resolve()
        assert startup_timeout == 120
        lifecycle.append("started")
        with engine.GatewayClient(url) as client:
            yield client
            assert (tmp_path / "session.md").read_text() == "# SDK session\n"
        lifecycle.append("closed")

    # The real LocalEngine lifecycle is covered in test_local and test_engine.
    monkeypatch.setattr(engine, "LocalEngine", local_engine)
    recipe("gateway_task")
    recipe("local_task").main()
    assert lifecycle == ["started", "closed"]


def test_progress_recipe_replays_from_submission_and_stops_for_its_turn(
    recipe, gateway, capsys
):
    url, calls, _, _ = gateway
    with engine.GatewayClient(url) as client:
        conversation = client.session("session-one")
        turn = conversation.send("Summarize")
        result = recipe("progress").watch_turn(conversation, turn)
        assert result["status"] == "completed"
    assert capsys.readouterr().out.splitlines() == [
        "subscription.ready",
        "turn.completed",
        "turn.completed",
    ]
    assert calls[-1][:2] == ("DELETE", "/v1/clients/sdk-lease")
