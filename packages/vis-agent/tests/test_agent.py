"""Agent lifecycle and transport injection; real transports live in test_sdk_guide."""

import json
import time
from typing import Annotated
from unittest.mock import Mock

import blockether.vis.engine as engine
import pytest
from pydantic import BaseModel, ConfigDict, Field, RootModel, field_validator


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


class Delivery(BaseModel):
    model_config = ConfigDict(extra="forbid")

    quote_cents: int = Field(ge=0)
    express: bool

    @field_validator("quote_cents")
    @classmethod
    def require_whole_dollars(cls, value):
        if value % 100:
            raise ValueError("quote must be in whole dollars")
        return value


def completed(markdown, turn_id="turn-one"):
    """A completed turn record whose final prose block is `markdown`."""
    return {
        "turn_id": turn_id,
        "status": "completed",
        "content": [{"type": "prose", "markdown": markdown}],
    }


def test_structured_result_uses_final_prose_and_reuses_conversation(local, tmp_path):
    result = {
        "turn_id": "turn-one",
        "status": "completed",
        "content": [
            {"type": "tool", "output": {"quote_cents": 999}},
            {"type": "reasoning", "text": "not the answer"},
            {"type": "prose", "markdown": "Earlier progress"},
            {"type": "prose", "markdown": '{"quote_cents": 1200, "express": true}'},
        ],
    }
    with engine.Agent(tmp_path) as agent:
        conversation = agent.session
        conversation.send.return_value.wait.return_value = result
        quote = agent.run(
            "Quote delivery without changing files.",
            response_model=Delivery,
            timeout=12,
            provider="example",
        )
        assert quote == Delivery(quote_cents=1200, express=True)
        request = conversation.send.call_args.args[0]
        assert "Quote delivery without changing files." in request
        assert '"quote_cents"' in request and '"express"' in request
        assert conversation.send.call_args.kwargs == {"provider": "example"}
        wait = conversation.send.return_value.wait
        wait.assert_called_once()
        assert 11 < wait.call_args.kwargs["timeout"] <= 12
        assert agent.run("Follow up")["status"] == "completed"
        assert conversation.send.call_count == 2
        assert agent.run("No schema", response_model=None) is result
        conversation.send.assert_called_with("No schema")


def test_structured_root_model_requests_an_array_not_an_object(local, tmp_path):
    class Scores(RootModel[list[int]]):
        pass

    with engine.Agent(tmp_path) as agent:
        conversation = agent.session
        conversation.send.return_value.wait.return_value = {
            "turn_id": "turn-one",
            "status": "completed",
            "content": [{"type": "prose", "markdown": "[10, 20]"}],
        }
        assert agent.run("Return scores", response_model=Scores) == Scores([10, 20])
        prompt = conversation.send.call_args.args[0]
        assert '"type": "array"' in prompt
        assert "JSON object" not in prompt


@pytest.mark.parametrize("status", ["failed", "cancelled", "suspended", "error"])
def test_structured_result_does_not_parse_unsuccessful_turn(local, tmp_path, status):
    with engine.Agent(tmp_path) as agent:
        conversation = agent.session
        result = {
            "turn_id": "turn-one",
            "status": status,
            "content": [
                {"type": "error", "code": "turn_failed", "message": "provider down"},
                {"type": "prose", "markdown": '{"quote_cents": 1200, "express": true}'},
            ],
        }
        conversation.send.return_value.wait.return_value = result
        with pytest.raises(engine.StructuredOutputError) as failure:
            agent.run("Quote", response_model=Delivery)
        assert failure.value.turn is result
        assert failure.value.errors == [
            {
                "path": "$",
                "message": f"the turn ended with status {status}: provider down",
                "source": "turn",
            }
        ]
        assert str(failure.value).startswith("Delivery turn did not complete")
        assert conversation.send.call_count == 1


def test_structured_result_corrects_answer_in_same_conversation(local, tmp_path):
    with engine.Agent(tmp_path) as agent:
        conversation = agent.session
        conversation.send.return_value.wait.side_effect = [
            completed('{"quote_cents": 1201}'),
            completed('{"quote_cents": 1201, "express": true}', "turn-two"),
            completed('{"quote_cents": 1200, "express": true}', "turn-three"),
        ]
        quote = agent.run(
            "Quote",
            response_model=Delivery,
            provider="example",
            attachments=["upload-1"],
            idempotency_key="quote-1",
        )
    assert quote == Delivery(quote_cents=1200, express=True)
    first, second, third = conversation.send.call_args_list
    assert first.kwargs == {
        "provider": "example",
        "attachments": ["upload-1"],
        "idempotency_key": "quote-1",
    }
    assert second.kwargs == third.kwargs == {"provider": "example"}
    assert "- $.express: required property is missing" in second.args[0]
    assert '"quote_cents"' in second.args[0]
    assert (
        "- $.quote_cents: Value error, quote must be in whole dollars (input: 1201)"
        in third.args[0]
    )


def test_structured_result_reports_every_attempt(local, tmp_path):
    answer = '{"quote_cents": -1, "express": "yes"}'
    turns = [completed(answer, f"turn-{number}") for number in range(3)]
    with engine.Agent(tmp_path) as agent:
        agent.session.send.return_value.wait.side_effect = turns
        with pytest.raises(engine.StructuredOutputError) as failure:
            agent.run("Quote", response_model=Delivery)
        assert agent.session.send.call_count == 3
    problems = [
        {
            "path": "$.quote_cents",
            "message": "does not satisfy minimum 0 (input: -1)",
            "source": "schema",
        },
        {
            "path": "$.express",
            "message": 'expected boolean, got string (input: "yes")',
            "source": "schema",
        },
    ]
    error = failure.value
    assert error.attempts == [
        {"turn": turn, "answer": answer, "errors": problems} for turn in turns
    ]
    assert error.turn is turns[-1] and error.errors == problems
    assert str(error) == (
        "Delivery answer is invalid after 3 attempts (turn turn-2, status completed)\n"
        "- $.quote_cents: does not satisfy minimum 0 (input: -1)\n"
        '- $.express: expected boolean, got string (input: "yes")'
    )
    assert error.__cause__ is None and error.__context__ is None


@pytest.mark.parametrize(
    ("content", "problem"),
    [
        ([], ("$", "the turn has no final text answer", "answer")),
        (
            [{"type": "tool", "output": {"quote_cents": 1200}}],
            ("$", "the turn has no final text answer", "answer"),
        ),
        (
            "not JSON",
            ("$", "the answer starts with text instead of a JSON value", "json"),
        ),
        (
            '```json\n{"quote_cents": 1200, "express": true}\n```',
            (
                "$",
                "the answer is wrapped in a Markdown code fence; "
                "send only the JSON value",
                "json",
            ),
        ),
        (
            '{"quote_cents": 1200, "express": true} trailing text',
            (
                "$",
                "text follows the JSON value at line 1, column 40; "
                "send exactly one JSON value",
                "json",
            ),
        ),
        (
            '{"quote_cents": -1, "quote_cents": 1200, "express": true}',
            ("$", 'object has duplicate member "quote_cents"', "json"),
        ),
        (
            '{"quote_cents": 1e400, "express": true}',
            ("$", "number 1e400 is outside the finite JSON number range", "json"),
        ),
        (
            '{"quote_cents": NaN, "express": true}',
            ("$", "NaN is not valid JSON", "json"),
        ),
        (
            '{"quote_cents": 1200}',
            ("$.express", "required property is missing", "schema"),
        ),
        (
            '{"quote_cents": 1200, "express": true, "other": 1}',
            (
                "$",
                "Additional properties are not allowed ('other' was unexpected)",
                "schema",
            ),
        ),
        (
            '{"quote_cents": true, "express": true}',
            (
                "$.quote_cents",
                "expected integer, got boolean (input: true)",
                "schema",
            ),
        ),
        (
            '{"quote_cents": 1201, "express": true}',
            (
                "$.quote_cents",
                "Value error, quote must be in whole dollars (input: 1201)",
                "pydantic",
            ),
        ),
    ],
)
def test_structured_result_explains_invalid_answer(local, tmp_path, content, problem):
    if isinstance(content, str):
        content = [{"type": "prose", "markdown": content}]
    result = {"turn_id": "turn-one", "status": "completed", "content": content}
    path, message, source = problem
    with engine.Agent(tmp_path) as agent:
        agent.session.send.return_value.wait.return_value = result
        with pytest.raises(engine.StructuredOutputError) as failure:
            agent.run("Quote", response_model=Delivery, max_corrections=0)
        assert agent.session.send.call_count == 1
    assert failure.value.turn is result
    assert failure.value.errors == [
        {"path": path, "message": message, "source": source}
    ]
    assert str(failure.value) == (
        "Delivery answer is invalid after 1 attempt (turn turn-one, status completed)"
        f"\n- {path}: {message}"
    )


def test_structured_errors_never_print_whole_values(local, tmp_path):
    class Profile(BaseModel):
        name: str
        age: int

    class Account(BaseModel):
        model_config = ConfigDict(hide_input_in_errors=True)

        balance: int = Field(ge=0)
        token: str

    with engine.Agent(tmp_path) as agent:
        conversation = agent.session
        conversation.send.return_value.wait.side_effect = [
            completed('{"name": "canary-name"}'),
            completed('[{"name": "canary-name", "age": 3}]', "turn-two"),
            completed('{"balance": -7, "token": "canary-token"}', "turn-three"),
            completed('{"balance": 1e999, "token": "canary-token"}', "turn-four"),
        ]
        with pytest.raises(engine.StructuredOutputError) as profile:
            agent.run("Profile", response_model=Profile, max_corrections=1)
        with pytest.raises(engine.StructuredOutputError) as account:
            agent.run("Account", response_model=Account, max_corrections=1)
        corrections = [call.args[0] for call in conversation.send.call_args_list]
    assert "- $.age: required property is missing" in corrections[1]
    assert profile.value.errors[0]["message"] == "expected object, got array"
    assert "- $.balance: does not satisfy minimum 0\n" in corrections[3]
    assert account.value.errors[0]["message"] == (
        "a number is outside the finite JSON number range"
    )
    for text in [
        corrections[1],
        corrections[3],
        str(profile.value),
        str(account.value),
    ]:
        assert "canary" not in text and "-7" not in text and "1e999" not in text


def test_structured_patterns_match_in_linear_time(local, tmp_path):
    exponential = r"^(a+)+$"
    hostile = "a" * 26 + "!"

    class Code(BaseModel):
        code: str = Field(pattern=exponential)
        tags: dict[Annotated[str, Field(pattern=exponential)], int] = {}

    with engine.Agent(tmp_path) as agent:
        agent.session.send.return_value.wait.side_effect = [
            completed(json.dumps({"code": hostile})),
            completed(json.dumps({"code": "aa", "tags": {"aaa": "x", hostile: 1}})),
        ]
        started = time.monotonic()
        with pytest.raises(engine.StructuredOutputError) as code:
            agent.run("Code", response_model=Code, max_corrections=0)
        with pytest.raises(engine.StructuredOutputError) as tags:
            agent.run("Code", response_model=Code, max_corrections=0)
    assert time.monotonic() - started < 1
    assert code.value.errors == [
        {
            "path": "$.code",
            "message": f'does not satisfy pattern "{exponential}" (input: "{hostile}")',
            "source": "schema",
        }
    ]
    assert tags.value.errors == [
        {
            "path": "$.tags.aaa",
            "message": 'expected integer, got string (input: "x")',
            "source": "schema",
        }
    ]


def link_note(schema):
    schema["properties"]["note"] = {"$ref": "https://schemas.example.com/note.json"}


class Linked(BaseModel):
    model_config = ConfigDict(json_schema_extra=link_note)

    note: str


@pytest.mark.parametrize(
    ("options", "failure", "message"),
    [
        ({"response_model": dict}, TypeError, "BaseModel"),
        ({"response_model": Linked}, ValueError, "self-contained"),
        ({"response_model": Delivery, "max_corrections": True}, TypeError, "integer"),
        ({"response_model": Delivery, "max_corrections": -1}, ValueError, "negative"),
        ({"response_model": Delivery, "timeout": 0}, ValueError, "timeout"),
    ],
)
def test_structured_result_rejects_invalid_options_before_connect(
    local, tmp_path, options, failure, message
):
    agent = engine.Agent(tmp_path)
    try:
        with pytest.raises(failure, match=message):
            agent.run("Quote", **options)
        agent.execution_layer.connect.assert_not_called()
    finally:
        agent.close()


def test_structured_result_does_not_retry_timeout(local, tmp_path):
    with engine.Agent(tmp_path) as agent:
        turn = agent.session.send.return_value
        turn.wait.side_effect = engine.VisTimeout("deadline")
        with pytest.raises(engine.VisTimeout, match="deadline"):
            agent.run("Quote", response_model=Delivery, timeout=1)
        turn.cancel.assert_not_called()
        agent.session.send.assert_called_once()


def test_structured_result_needs_time_left_for_a_correction(local, tmp_path):
    def slow_invalid_answer(timeout):
        time.sleep(timeout + 0.01)
        return completed('{"quote_cents": 1200}')

    with engine.Agent(tmp_path) as agent:
        agent.session.send.return_value.wait.side_effect = slow_invalid_answer
        with pytest.raises(engine.StructuredOutputError, match="no time") as failure:
            agent.run("Quote", response_model=Delivery, timeout=0.05)
        agent.session.send.assert_called_once()
    assert failure.value.errors[0]["path"] == "$.express"
