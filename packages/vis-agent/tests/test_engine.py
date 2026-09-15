"""Installed SDK against real HTTP/stdio engines and a deterministic model endpoint.

Set VIS_TEST_LOCAL_COMMAND to the engine argv. No gateway discovery, existing
sessions, remote models or user configuration are used. Only the model is a double.
"""

import json
import os
import shlex
import shutil
import signal
import socket
import subprocess
import tempfile
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from itertools import count
from pathlib import Path

import pytest
from blockether.vis.engine import (
    Event,
    GatewayClient,
    GatewayError,
    LocalEngine,
    ProtocolError,
)

EXTENSION = """import blockether.vis.extension as vis
def sdk_activity(phase, **_):
    return vis.ActivityPresentation("SDK flow", phase, (vis.ActivityText("SDK stage: " + phase),))

def sdk_flow(mode):
    "Run a deterministic tool, live View, form and state update."
    vis.publish_activity(vis.ActivityPresentation("SDK flow", "Awaiting answer", (vis.ActivityProgress("Waiting"),)))
    with vis.live("SDK flow", [vis.status("now", "Waiting", tone="running")], flush_ms=0) as view:
        answer = vis.ask("SDK " + mode, [vis.plaintext("name", label="Name", is_required=True)], timeout_ms=60000)
        if not answer:
            return {"reason": answer.reason}
        view.status("Answered", tone="ok")
        vis.state["name"] = answer["name"]
        return {"name": vis.state["name"]}

def sdk_native_wait(marker: str):
    "Wait inside C until cancellation reclaims this trusted extension worker."
    import os
    import threading
    from pathlib import Path
    condition = threading.Condition()
    def notify_waiting():
        with condition:
            Path(marker).write_text(str(os.getpid()))
    with condition:
        threading.Thread(target=notify_waiting, daemon=True).start()
        condition.wait(120)

vis.register_extension(vis.Extension(name="sdk-fixture", description="SDK integration fixture", alias="sdk",
    symbols=[vis.Symbol(sdk_flow, activity=vis.Activity(presenter="observation", label="SDK flow", render=sdk_activity)),
             vis.Symbol(sdk_native_wait)]))
"""


PROVIDER_EXTENSION = """import blockether.vis.extension as vis

def credential():
    return vis.ProviderCredential("fixture-provider", api_url=__MODEL_URL__,
        llm_headers=__CREDENTIAL_HEADERS__)

def enrich(provider, router_opts):
    return [vis.ProviderModel(m["name"], context=200000, is_tool_call=True)
            for m in provider["models"]]

vis.register_extension(vis.Extension(name="sdk-provider", description="Typed provider integration",
    providers=[vis.Provider("sdk-fixture", "SDK fixture", is_managed=__MANAGED__,
        preset=vis.ProviderPreset(base_url="http://127.0.0.1:1/v1", api_style="openai",
            default_models=["sdk-test"], llm_headers={"X-SDK-Preset": "kept"},
            extra_body={"sdk_marker": {"keep_this_key": "preserved"}}),
        get_token_fn=credential, status_fn=lambda: vis.ProviderStatus(True),
        enrich_models_fn=enrich)]))
"""


def activity_rows(projection):
    """Include SDK child invocations inside a Python-execution row."""
    pending = list(projection.rows) if projection else []
    while pending:
        row = pending.pop()
        yield row
        pending.extend(row.children or ())


@contextmanager
def model_endpoint(*, tool_code=None, before_reply=None):
    requests = []
    positions = count(1)

    class Model(BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            if not body.get("tools"):
                delta, finish = {"content": "SDK fixture"}, "stop"
            else:
                body["_test_headers"] = dict(self.headers)
                body["_test_path"] = self.path
                requests.append(body)
                position = next(positions)
                # The default flow completes, cancels without a final model reply,
                # then completes again in the same session.
                tool_reply = (
                    position in (1, 3, 4)
                    if tool_code is None
                    else body["messages"][-1]["role"] != "tool"
                )
                if tool_reply:
                    mode = "cancel" if position == 3 else "complete"
                    if before_reply is not None:
                        before_reply(position)
                    code = (
                        tool_code(position)
                        if callable(tool_code)
                        else tool_code
                        if tool_code is not None
                        else f"print(await sdk_flow({mode!r}))"
                    )
                    delta = {
                        "tool_calls": [
                            {
                                "index": 0,
                                "id": f"call_{position}",
                                "type": "function",
                                "function": {
                                    "name": "python_execution",
                                    "arguments": json.dumps({"code": code}),
                                },
                            }
                        ]
                    }
                    finish = "tool_calls"
                else:
                    delta, finish = {"content": "SDK flow completed"}, "stop"
            value = {
                "id": "fixture",
                "object": "chat.completion.chunk",
                "model": "sdk-test",
                "choices": [{"index": 0, "delta": delta, "finish_reason": finish}],
            }
            data = ("data: " + json.dumps(value) + "\n\ndata: [DONE]\n\n").encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

    server = ThreadingHTTPServer(("127.0.0.1", 0), Model)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}/v1", requests
    finally:
        server.shutdown()
        server.server_close()
        thread.join(2)


@contextmanager
def real_client(transport, command, work, *, on_process=None):
    if transport == "stdio":
        engine = LocalEngine(
            executable=command, root=work, timeout=60, startup_timeout=180
        )
        try:
            with engine:
                if on_process is not None:
                    on_process(engine._process)
                yield engine
        finally:
            engine.close()
            if engine._process is not None:
                assert engine._process.poll() is not None
            if engine._home is not None:
                assert not Path(engine._home.name).exists()
        return
    with socket.socket() as reservation:
        reservation.bind(("127.0.0.1", 0))
        port = reservation.getsockname()[1]
    token = work / "gateway-token"
    token.write_text("sdk-fixture-token")
    token.chmod(0o600)
    with tempfile.TemporaryFile() as output:
        process = subprocess.Popen(
            [
                *command,
                "gateway",
                "start",
                "--host",
                "127.0.0.1",
                "--port",
                str(port),
                "--db",
                str(work / "http.sqlite"),
                "--require-token",
                "--token-file",
                str(token),
            ],
            cwd=work,
            stdout=output,
            stderr=output,
            start_new_session=True,
        )
        try:
            deadline = time.monotonic() + 180
            while True:
                assert process.poll() is None, "fixture gateway exited before binding"
                try:
                    with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                        break
                except OSError:
                    assert time.monotonic() < deadline, (
                        "fixture gateway startup timed out"
                    )
                    time.sleep(0.1)
            url = f"http://127.0.0.1:{port}"
            with pytest.raises(GatewayError) as unauthorized:
                GatewayClient(url, token="not-the-fixture-token").connect()
            assert unauthorized.value.status == 401
            with GatewayClient(url, token="sdk-fixture-token", timeout=60) as client:
                if on_process is not None:
                    on_process(process)
                yield client
            # SDK close releases its lease, not somebody else's server.
            assert process.poll() is None
        finally:
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                process.wait(10)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(5)
        assert process.poll() is not None


@contextmanager
def sdk_fixture(
    tmp_path,
    monkeypatch,
    transport,
    *,
    council=False,
    subagents=False,
    tool_code=None,
    before_reply=None,
    project_extensions=None,
    on_process=None,
):
    raw = os.environ.get("VIS_TEST_LOCAL_COMMAND")
    if not raw:
        pytest.skip("set VIS_TEST_LOCAL_COMMAND to the actual engine argv")
    home = tmp_path / "home"
    config = home / ".vis"
    config.mkdir(parents=True)
    work = tmp_path / "project"
    extensions = work / ".vis" / "extensions"
    extensions.mkdir(parents=True)
    (extensions / "fixture.py").write_text(EXTENSION)
    for name, source in (project_extensions or {}).items():
        (extensions / name).write_text(source)
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("JAVA_TOOL_OPTIONS", f"-Duser.home={home}")
    monkeypatch.delenv("VIS_GATEWAY_URL", raising=False)
    with model_endpoint(tool_code=tool_code, before_reply=before_reply) as (
        model_url,
        requests,
    ):
        managed = transport == "http"
        (extensions / "provider.py").write_text(
            PROVIDER_EXTENSION.replace("__MODEL_URL__", repr(model_url))
            .replace("__MANAGED__", repr(managed))
            .replace(
                "__CREDENTIAL_HEADERS__",
                repr(None if managed else {"X-SDK-Credential": "kept"}),
            )
        )
        (config / "config.yml").write_text(
            json.dumps(
                {
                    "providers": [] if managed else [{"id": "sdk-fixture"}],
                    "toggles": {"council": council, "subagents": subagents},
                    "default_provider": "sdk-fixture",
                    "default_model": "sdk-test",
                }
            )
        )
        command = shlex.split(raw)
        if len(command) == 1:
            command.append(f"-Duser.home={home}")
        with real_client(transport, command, work, on_process=on_process) as client:
            yield client, work, requests


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_agent_tool_view_activity_and_cancellation(
    tmp_path, monkeypatch, transport
):
    # This fixture has no user data; diagnose unsolicited stdout without allowing it.
    decode = json.loads

    def checked_json(data, *args, **kwargs):
        try:
            return decode(data, *args, **kwargs)
        except json.JSONDecodeError:
            pytest.fail(f"non-JSON fixture transport output: {data[:200]!r}")

    monkeypatch.setattr(json, "loads", checked_json)
    decode_event = Event.from_wire

    def checked_event(value):
        try:
            return decode_event(value)
        except ProtocolError:
            pytest.fail(f"invalid fixture event: {value!r}")

    monkeypatch.setattr(Event, "from_wire", checked_event)
    managed = transport == "http"
    with sdk_fixture(tmp_path, monkeypatch, transport) as (client, work, requests):
        session = client.create_session(
            title="SDK fixture", root=str(work), channel="app"
        )
        for mode in ("complete", "cancel", "complete"):
            turn = session.send(mode)
            seen = []
            pending_input = None
            progress_seen = False
            with session.events(cursor=turn.cursor, reconnects=0) as events:
                for event in events:
                    seen.append(event)
                    progress_seen |= any(
                        row.presentation
                        and row.presentation["summary"] == "Awaiting answer"
                        for row in activity_rows(event.activity)
                    )
                    if event.type == "iteration.error":
                        pytest.fail(f"fixture engine error: {str(event.data)[:1200]}")
                    if event.type == "view.open":
                        if event.view.kind == "live":
                            assert any(
                                v.id == event.view.view_id for v in session.live_views()
                            )
                        else:
                            assert any(
                                v.id == event.view.view_id
                                for v in session.input_views()
                            )
                            pending_input = event.view.view_id
                    # Activity frames coalesce. Keep the form pending until the
                    # intermediate presentation has crossed the transport.
                    if pending_input is not None and progress_seen:
                        if mode == "complete":
                            assert session.answer(pending_input, {"name": "Ada"})[
                                "is_accepted"
                            ]
                        else:
                            turn.cancel()
                        pending_input = None
                    if event.type in {
                        "turn.completed",
                        "turn.failed",
                        "turn.cancelled",
                    }:
                        break
                    assert len(seen) < 200, [e.type for e in seen]
            if not any(e.type == "view.open" and e.view.kind == "input" for e in seen):
                pytest.fail(
                    "missing input View; fixture tool evidence: "
                    + repr(
                        [(e.type, e.data) for e in seen if e.type.startswith("block.")]
                    )[:3000]
                )
            closed_views = {
                e.view.kind: e.view.result for e in seen if e.type == "view.close"
            }
            assert set(closed_views) == {"input", "live"}, (
                mode,
                [(e.type, e.view.kind if e.view else None) for e in seen],
            )
            # Public close receipts must not disclose the form answer. HTTP
            # omits the live picture already delivered by open/patch; stdio
            # polling reads the durable receipt, which retains that picture.
            assert set(closed_views["input"].to_wire()) == {"reason"}
            assert (closed_views["live"].view is None) == (transport == "http")
            rows = [row for event in seen for row in activity_rows(event.activity)]
            assert any(
                row.presentation
                and row.presentation["headline"] == "SDK flow"
                and row.presentation["summary"] == "Awaiting answer"
                for row in rows
            ), [(row.operation, row.state, row.presentation) for row in rows]
            assert any(
                row.presentation
                and row.presentation["content"]
                and row.presentation["content"][0]["type"] == "progress"
                for row in rows
            )
            result = turn.wait(timeout=30)
            assert result["status"] == (
                "completed" if mode == "complete" else "cancelled"
            ), result
            assert session.input_views() == []
            assert session.live_views() == []
            if mode == "complete":
                assert any(e.type == "view.patch" for e in seen)
                assert any(e.activity and e.activity.counts.succeeded for e in seen)
                assert any(
                    row.presentation
                    and row.presentation["summary"] == "success"
                    and row.presentation["content"][0]["text"] == "SDK stage: success"
                    for row in rows
                )
        assert len(requests) == 5
        for request in requests:
            assert request["_test_path"] == "/v1/chat/completions"
            headers = {
                key.lower(): value for key, value in request["_test_headers"].items()
            }
            assert headers["authorization"] == "Bearer fixture-provider"
            # Header maps follow whole-field precedence, not a deep merge.
            header = "x-sdk-preset" if managed else "x-sdk-credential"
            assert headers[header] == "kept"
            assert request["sdk_marker"] == {"keep_this_key": "preserved"}
        assert session.transcript().content
        assert len(session.turns()) == 3
        session.delete()


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_agent_cancellation_reclaims_native_extension_wait(
    tmp_path, monkeypatch, transport
):
    marker = tmp_path / "extension-worker.pid"
    code = f"print(await sdk_native_wait({str(marker)!r}))"
    with sdk_fixture(tmp_path, monkeypatch, transport, tool_code=code) as (
        client,
        work,
        _,
    ):
        session = client.create_session(root=str(work), channel="app")
        turn = session.send("Cancel a trusted extension blocked in C")
        deadline = time.monotonic() + 60
        while True:
            pid_text = marker.read_text() if marker.exists() else ""
            if pid_text.isdigit():
                pid = int(pid_text)
                break
            assert time.monotonic() < deadline, (
                "extension did not enter its native wait"
            )
            time.sleep(0.05)
        turn.cancel()
        assert turn.wait(timeout=30)["status"] == "cancelled"
        deadline = time.monotonic() + 10
        while True:
            try:
                os.kill(pid, 0)
            except ProcessLookupError:
                break
            assert time.monotonic() < deadline, (
                "cancelled extension worker is still alive"
            )
            time.sleep(0.05)
        session.delete()


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_council_roundtrip(tmp_path, monkeypatch, transport):
    # Real engine, SQLite and transports; the model barrier is the only scheduling double.
    ready, release = threading.Event(), threading.Event()

    def before_reply(_position):
        ready.set()
        assert release.wait(30), "SDK did not release the model fixture"

    with sdk_fixture(
        tmp_path,
        monkeypatch,
        transport,
        council=True,
        tool_code=(
            "threads = await council.threads()\n"
            "entry = await council.publish('Host continuation', kind='coordination', thread_id=threads['entries'][0]['thread_id'])\n"
            "assert (await council.get(entry['entry_id']))['source'] == 'host'\n"
            "print(session['council'])"
        ),
        before_reply=before_reply,
    ) as (client, work, requests):
        session = client.create_session(
            title="Council SDK", root=str(work), channel="app"
        )
        project = client.post_projects(body={"name": "Council SDK"})
        session.update(project_id=project["id"])
        readonly = session.council()
        assert readonly.members() == ()
        assert readonly.threads().entries == ()
        bound, entry = None, None
        for generation in range(2):
            ready.clear()
            release.clear()
            turn = session.send("Continue the fixture")
            try:
                assert ready.wait(30), "model request was not observed"
                active = session.council()
                assert [member.session_id for member in active.members()] == [
                    session.id
                ]
                with pytest.raises(GatewayError) as inactive:
                    readonly.publish(
                        "Must not bind an idle handle", kind="coordination"
                    )
                assert inactive.value.status == 409
                for invalid in (
                    {"content": " "},
                    {"content": "é" * 32769},
                    {"content": "Valid", "title": "two\nlines"},
                ):
                    with pytest.raises(GatewayError) as invalid_request:
                        active.publish(kind="coordination", **invalid)
                    assert invalid_request.value.status == 400
                if generation == 0:
                    bound = active
                    entry = bound.publish(
                        "SDK conversation",
                        kind="coordination",
                        title="SDK\n",
                        ping="all",
                        idempotency_key="sdk-retry",
                    )
                    assert entry.title == "SDK"
                    assert (
                        entry.source == "sdk"
                        and entry.source_ref.session_id == session.id
                        and entry.ping == ()
                    )
                    assert entry.source_ref.session_state_id
                    assert entry.source_ref.turn == 1
                    assert entry.source_ref.iteration == 1
                    thread_id = bound.threads().entries[0].thread_id
                    assert thread_id == entry.thread_id
                    continuation = bound.publish(
                        "SDK continuation", kind="coordination", thread_id=thread_id
                    )
                    first_page = bound.read(thread_id=thread_id, limit=1)
                    assert first_page.entries == (entry,) and first_page.has_more
                    assert bound.read(
                        thread_id=thread_id, after=first_page.after
                    ).entries == (continuation,)
                    assert bound.get(entry.entry_id) == entry
                else:
                    with pytest.raises(GatewayError) as stale:
                        bound.publish("Must not rebind", kind="coordination")
                    assert stale.value.status == 409
                    assert (
                        bound.publish(
                            "SDK conversation",
                            kind="coordination",
                            title="SDK",
                            ping="all",
                            idempotency_key="sdk-retry",
                        )
                        == entry
                    )
                    active.publish(
                        "New active generation",
                        kind="coordination",
                        thread_id=entry.thread_id,
                    )
            finally:
                release.set()
            operations = {}
            with session.events(cursor=turn.cursor, reconnects=0) as events:
                for event in events:
                    for row in activity_rows(event.activity):
                        if row.operation.startswith("council."):
                            operations[row.id] = row.operation
                    if event.type in {
                        "turn.completed",
                        "turn.failed",
                        "turn.cancelled",
                    }:
                        break
            assert sorted(operations.values()) == [
                "council.get",
                "council.publish",
                "council.threads",
            ]
            assert turn.wait(timeout=30)["status"] == "completed"
            assert (
                bound.publish(
                    "SDK conversation",
                    kind="coordination",
                    title="SDK",
                    ping="all",
                    idempotency_key="sdk-retry",
                )
                == entry
            )
            assert bound.members() == ()
        assert (
            len(requests) == 4
        )  # No Council request started a turn or extra iteration.
        for request in requests:
            assert any(
                "## Council: session conversation" in str(message.get("content", ""))
                for message in request["messages"]
                if message["role"] == "system"
            )
        transcript = json.loads(session.transcript().content)
        assert "default_group_id" in str(transcript)
        assert "council_publications" in str(transcript)
        host_entries = [
            row
            for row in bound.read(thread_id=entry.thread_id).entries
            if row.source == "host"
        ]
        assert len(host_entries) == 2
        assert all(row.source_ref.session_id == session.id for row in host_entries)
        assert [row.source_ref.turn for row in host_entries] == [1, 2]
        assert all(row.source_ref.iteration == 1 for row in host_entries)
        assert all(row.source_ref.session_turn_soul_id for row in host_entries)
        assert all(row.source_ref.session_turn_state_id for row in host_entries)
        assert all(row.source_ref.session_turn_iteration_id for row in host_entries)
        assert all(
            str(row.entry_id) in str(transcript)
            and row.source_ref.operation_id in str(transcript)
            for row in host_entries
        )
        assert len(session.turns()) == 2
        session.delete()


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_council_idle_ping_does_not_wake_independent_peer(
    tmp_path, monkeypatch, transport
):
    ready, release = threading.Event(), threading.Event()

    def before_reply(position):
        if position == 1:
            ready.set()
            assert release.wait(30), "SDK did not release the author"

    with sdk_fixture(
        tmp_path,
        monkeypatch,
        transport,
        council=True,
        tool_code="print('Council fixture')",
        before_reply=before_reply,
    ) as (client, work, requests):
        author = client.create_session(
            title="Council author", root=str(work), channel="app"
        )
        peer = client.create_session(
            title="Past parser research", root=str(work), channel="app"
        )
        turn = author.send("Ask the peer about its research")
        try:
            assert ready.wait(30), "author model request was not observed"
            conversation = author.council()
            assert (
                conversation.publish(
                    "Active peers only", kind="coordination", ping="all"
                ).ping
                == ()
            )
            assert peer.turns() == []
            entry = conversation.publish(
                "What did you learn about the parser?",
                kind="coordination",
                ping=[peer.id, f"vis_session_id#{peer.id}"],
                idempotency_key="wake-once",
            )
            assert entry.ping == (peer.id,)
            # A shared project/group does not make this peer a managed subagent.
            assert peer.council().get(entry.entry_id) == entry
            assert peer.turns() == []
            assert (
                conversation.publish(
                    entry.content,
                    kind="coordination",
                    ping=[peer.id],
                    idempotency_key="wake-once",
                )
                == entry
            )
            assert peer.turns() == []
            assert len(requests) == 1  # Only the explicitly started author is blocked.
        finally:
            release.set()
        assert turn.wait(timeout=30)["status"] == "completed"
        assert len(requests) == 2
        assert peer.turns() == []
        author.delete()
        peer.delete()


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_council_reply_after_author_finishes_does_not_wake_it(
    tmp_path, monkeypatch, transport
):
    author_ready, peer_ready = threading.Event(), threading.Event()
    release_author, release_peer = threading.Event(), threading.Event()

    def before_reply(position):
        if position == 1:
            author_ready.set()
            assert release_author.wait(30), "author was not released"
        elif position == 2:
            peer_ready.set()
            assert release_peer.wait(30), "peer was not released"

    with sdk_fixture(
        tmp_path,
        monkeypatch,
        transport,
        council=True,
        tool_code="print('Council fixture')",
        before_reply=before_reply,
    ) as (client, work, requests):
        author = client.create_session(
            title="Council author", root=str(work), channel="app"
        )
        peer = client.create_session(
            title="Council researcher", root=str(work), channel="app"
        )
        try:
            turn = author.send("Finish the initial research task")
            assert author_ready.wait(30), "author did not start"
            conversation = author.council()
            entry = conversation.publish(
                "Research question", kind="coordination", ping=[peer.id]
            )
            # Independent peers run only after an explicit user/SDK start, not a ping.
            peer_turn = peer.send("Answer the research question")
            assert peer_ready.wait(30), "explicit peer turn did not start"
            release_author.set()
            assert turn.wait(timeout=30)["status"] == "completed"
            reply = peer.council().publish(
                "Research findings",
                kind="informational",
                thread_id=entry.thread_id,
                ping=[author.id],
                idempotency_key="findings",
            )
            replies = [
                row
                for row in conversation.read(thread_id=entry.thread_id).entries
                if row.content == "Research findings"
            ]
            assert replies == [reply]
            assert reply.source == "sdk" and reply.ping == (author.id,)
            assert len(author.turns()) == 1
            release_peer.set()
            assert peer_turn.wait(timeout=30)["status"] == "completed"
            assert len(author.turns()) == len(peer.turns()) == 1
            assert len(requests) == 4
        finally:
            release_author.set()
            release_peer.set()
            author.delete()
            peer.delete()


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_council_managed_child_wakes_once(tmp_path, monkeypatch, transport):
    hold, ready, release = threading.Event(), threading.Event(), threading.Event()

    def before_reply(position):
        if hold.is_set() and not ready.is_set():
            ready.set()
            assert release.wait(30), "parent was not released"

    def settled_turns(child, count):
        deadline = time.monotonic() + 30
        while True:
            turns = [row for row in child.turns() if row.get("council")]
            if len(turns) == count and all(
                row["status"] == "completed" for row in turns
            ):
                return turns
            assert time.monotonic() < deadline, ("managed child did not finish", turns)
            time.sleep(0.05)

    with sdk_fixture(
        tmp_path,
        monkeypatch,
        transport,
        council=True,
        subagents=True,
        before_reply=before_reply,
        tool_code=(
            "if session['agent']['role'] == 'leader':\n"
            "    if not (await council.subagents()):\n"
            "        print(await council.publish_spawn('Report fixture evidence', iteration_budget=8, key='child'))\n"
            "else:\n"
            "    for pending in session['council']['pending_replies']:\n"
            "        print(await council.publish('Fixture evidence', kind='informational', reply_to=pending['entry_id']))\n"
            "print('Managed Council fixture')"
        ),
    ) as (client, work, requests):
        parent = client.create_session(
            title="Managed parent", root=str(work), channel="app"
        )
        turn = parent.send("Delegate a bounded task")
        assert turn.wait(timeout=30)["status"] == "completed"
        team = parent.council()
        agents = team.subagents()
        assert len(agents) == 1 and agents[0].parent_id == parent.id
        child = client.session(agents[0].session_id)
        settled_turns(child, 1)
        hold.set()
        active = parent.send("Collect another child report")
        try:
            assert ready.wait(30), "parent model request was not observed"
            team = parent.council()
            entry = team.publish(
                "Report again",
                kind="coordination",
                ping=[child.id],
                idempotency_key="managed-wake",
            )
            turns = settled_turns(child, 2)
            assert turns[-1]["request"] == entry.content
            assert turns[-1]["council"]["entry_id"] == entry.entry_id
            assert (
                team.publish(
                    "Report again",
                    kind="coordination",
                    ping=[child.id],
                    idempotency_key="managed-wake",
                )
                == entry
            )
            assert len([row for row in child.turns() if row.get("council")]) == 2
        finally:
            release.set()
        assert active.wait(timeout=30)["status"] == "completed"
        child.delete()
        parent.delete()


@pytest.mark.parametrize("transport", ["stdio", "http"])
def test_real_council_disabled(tmp_path, monkeypatch, transport):
    with sdk_fixture(
        tmp_path,
        monkeypatch,
        transport,
        tool_code=(
            "assert 'council' not in session; assert not hasattr(council, 'publish'); "
            "assert callable(council.subagents); print('disabled')"
        ),
    ) as (client, work, requests):
        session = client.create_session(
            title="Council disabled", root=str(work), channel="app"
        )
        assert (
            session.send("Verify the default").wait(timeout=30)["status"] == "completed"
        )
        team = session.council()
        assert team.subagents() == ()
        with pytest.raises(GatewayError) as disabled:
            team.members()
        assert disabled.value.status == 409
        assert len(requests) == 2
        assert all(
            "## Council: session conversation" not in str(message.get("content", ""))
            for request in requests
            for message in request["messages"]
            if message["role"] == "system"
        )
        assert "disabled" in str(session.transcript().content)
        session.delete()


@pytest.mark.parametrize("transport", ["stdio", "http"])
@pytest.mark.parametrize("respond", [True, False], ids=["handshake", "timeout"])
def test_real_mcp_stdio_probe_reclaims_process(
    tmp_path, monkeypatch, transport, respond
):
    python = shutil.which("python3")
    if not python:
        pytest.skip("python3 is required for the stdio MCP fixture")
    server = (
        Path(__file__).resolve().parents[3] / "test/resources/mcp/fake_mcp_server.py"
    )
    with sdk_fixture(tmp_path, monkeypatch, transport) as (client, work, requests):
        marker = work / "mcp.pid"
        program = (
            "import os, pathlib, runpy, sys, time; "
            "pathlib.Path(sys.argv[1]).write_text(str(os.getpid())); "
            + (
                "runpy.run_path(sys.argv[2], run_name='__main__')"
                if respond
                else "time.sleep(60)"
            )
        )
        try:
            candidate = {
                "name": "sdk-process-cleanup",
                "server": {
                    "transport": "stdio",
                    "command": python,
                    "args": ["-c", program, str(marker), str(server)],
                    "timeout_ms": 1000,
                },
            }
            if respond:
                result = client.post_mcp_servers_test(body=candidate)
                assert result["is_connected"] is True
                assert [tool["name"] for tool in result["tools"]] == ["echo"]
            else:
                with pytest.raises(GatewayError) as failure:
                    client.post_mcp_servers_test(body=candidate)
                assert failure.value.code == "timeout"
            assert marker.is_file(), "MCP fixture never started"
            pid = int(marker.read_text())
            deadline = time.monotonic() + 5
            while True:
                try:
                    os.kill(pid, 0)
                except ProcessLookupError:
                    break
                assert time.monotonic() < deadline, "MCP probe left its child alive"
                time.sleep(0.01)
            assert not requests
            assert not client.get_mcp_servers()["servers"], (
                "Probe must not save a server"
            )
        finally:
            # Also reap a child when transport setup fails before returning a connection.
            deadline = time.monotonic() + 2
            while not marker.exists() and time.monotonic() < deadline:
                time.sleep(0.01)
            if marker.exists():
                try:
                    os.kill(int(marker.read_text()), signal.SIGKILL)
                except ProcessLookupError:
                    pass


def test_real_concurrent_sessions_keep_repeated_turns_isolated(tmp_path, monkeypatch):
    with sdk_fixture(
        tmp_path, monkeypatch, "http", tool_code="print(sum(range(1000)))"
    ) as (client, work, requests):
        # SDK clients belong to one calling thread. Share only the fixture origin.
        url = client._url
        markers = [f"isolated-sdk-project-{index}" for index in range(4)]
        ready = threading.Barrier(len(markers), timeout=30)

        def exercise(index):
            with GatewayClient(url, token="sdk-fixture-token", timeout=60) as peer:
                session = peer.create_session(root=str(work), channel="app")
                try:
                    ready.wait()
                    for turn_index in range(3):
                        turn = session.send(
                            f"{markers[index]} turn {turn_index}: compute sum"
                        )
                        if index % 2 == 0:
                            with session.events(
                                cursor=turn.cursor, reconnects=0
                            ) as events:
                                for event in events:
                                    assert event.type != "iteration.error", event.data
                                    if event.type in {
                                        "turn.completed",
                                        "turn.failed",
                                        "turn.cancelled",
                                    }:
                                        assert event.type == "turn.completed", (
                                            event.data
                                        )
                                        break
                        assert turn.wait(timeout=60)["status"] == "completed"
                    transcript = str(session.transcript().content)
                    assert "499500" in transcript
                    assert all(
                        f"{markers[index]} turn {turn_index}" in transcript
                        for turn_index in range(3)
                    )
                    assert all(
                        marker not in transcript
                        for other, marker in enumerate(markers)
                        if other != index
                    )
                finally:
                    session.delete()

        with ThreadPoolExecutor(max_workers=len(markers)) as executor:
            list(executor.map(exercise, range(len(markers))))
        assert len(requests) == 24
