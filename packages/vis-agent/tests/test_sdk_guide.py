"""Execute SDK recipes against HTTP doubles, native engines and opt-in live models."""

import json
import os
import re
import shlex
import subprocess
import sys
import textwrap
from pathlib import Path
from types import ModuleType
from urllib.parse import parse_qs, urlsplit

import blockether.vis.engine as engine
import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts
from test_client import compatible, endpoint
from test_engine import real_client, sdk_fixture


def _recipe_source(name):
    document = Path(__file__).parents[3] / "resources/vis-docs/python-sdk.md"
    match = re.search(
        rf"```python\n# {re.escape(name)}.py\n(.*?)\n```",
        document.read_text(),
        re.S,
    )
    assert match, f"Missing executable example: {name}"
    return match[1]


@pytest.fixture
def recipe(monkeypatch):
    def load(name):
        module = ModuleType(Path(name).name)
        monkeypatch.setitem(sys.modules, module.__name__, module)
        exec(compile(_recipe_source(name), name + ".py", "exec"), module.__dict__)
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


def connection_environment(monkeypatch, client, work):
    monkeypatch.setenv("VIS_GATEWAY_URL", client._url)
    monkeypatch.setenv("VIS_GATEWAY_TOKEN", client._token)
    monkeypatch.setenv("VIS_PROJECT_ROOT", str(work))


def test_view_receipt_recipe_decodes_saved_data_without_io(recipe, monkeypatch):
    def refuse(*_args, **_kwargs):
        pytest.fail("Reading a saved receipt must not start Vis or use the network")

    monkeypatch.setattr("socket.create_connection", refuse)
    monkeypatch.setattr(subprocess, "Popen", refuse)
    result = recipe("view_receipt").result
    assert result.view.nodes[0]["text"] == "Done"
    assert result.to_wire()["view"]["title"] == "Build"


def test_gateway_recipe_sends_a_task_and_releases_only_its_lease(
    recipe, gateway, monkeypatch, capsys
):
    url, calls, submissions, result = gateway
    result["content"] = [{"type": "text", "text": "Project summary"}]
    connection_environment(
        monkeypatch,
        engine.GatewayClient(url, token="fixture-token"),
        "/srv/vis-project",
    )
    recipe("gateway_task").main()
    assert submissions[0] == {
        "root": "/srv/vis-project",
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
    assert "Status: completed" in output
    assert "Project summary" in output
    assert "fixture-token" not in output


@pytest.mark.parametrize("status", ["failed", "cancelled", "suspended"])
def test_gateway_recipe_reports_noncompleted_status(
    recipe, gateway, monkeypatch, capsys, status
):
    url, calls, _, result = gateway
    result["status"] = status
    connection_environment(
        monkeypatch,
        engine.GatewayClient(url, token="fixture-token"),
        "/srv/vis-project",
    )
    recipe("gateway_task").main()
    assert f"Status: {status}" in capsys.readouterr().out
    assert calls[-1][:2] == ("DELETE", "/v1/clients/sdk-lease")


def test_local_recipe_uses_default_project_and_closes_its_engine(
    recipe, gateway, monkeypatch, tmp_path, capsys
):
    url, calls, _, _ = gateway
    monkeypatch.chdir(tmp_path)

    def local_engine(*, root):
        assert root == "."
        client = engine.GatewayClient(url)
        monkeypatch.setattr(
            client,
            "session_options",
            lambda project: {"root": str(Path(project).resolve(strict=True))},
        )
        return client

    monkeypatch.setattr("blockether.vis.engine._agent.LocalEngine", local_engine)
    recipe("local_task").main()
    assert "Status: completed" in capsys.readouterr().out
    assert calls[-1][:2] == ("DELETE", "/v1/clients/sdk-lease")


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


@pytest.mark.parametrize("transport", ["http", "stdio"])
def test_recipes_complete_against_real_engine(
    recipe, tmp_path, monkeypatch, capsys, transport
):
    with sdk_fixture(
        tmp_path, monkeypatch, transport, tool_code='print("Guide tool completed")'
    ) as (client, work, requests):
        monkeypatch.chdir(work)
        if transport == "http":
            connection_environment(monkeypatch, client, work)
            recipe("gateway_task").main()
        else:
            local_engine = engine.LocalEngine
            with monkeypatch.context() as local_patch:
                local_patch.setattr(
                    "blockether.vis.engine._agent.LocalEngine",
                    lambda **opts: local_engine(executable=client._command, **opts),
                )
                recipe("local_task").main()
        with engine.Agent(work, execution_layer=client) as agent:
            session_id = agent.session.id
            first = agent.run("Run the guide fixture.")
            assert first["status"] == "completed"
            turn = agent.send("Run it again.")
            result = recipe("progress").watch_turn(agent.session, turn)
            assert result["status"] == "completed"
            assert (
                "SDK flow completed"
                in agent.session.transcript(format="markdown").content.decode()
            )
            if transport == "stdio":
                borrowed_process = agent.execution_layer._process
        if transport == "http":
            # Closing Agent must leave the existing gateway and saved session usable.
            with engine.GatewayClient(client._url, token=client._token) as resumed:
                saved = resumed.session(session_id)
                assert (
                    "SDK flow completed"
                    in saved.transcript(format="markdown").content.decode()
                )
                assert (
                    saved.send("Continue after Agent closed.").wait(timeout=60)[
                        "status"
                    ]
                    == "completed"
                )
        else:
            assert borrowed_process.poll() is None
        output = capsys.readouterr().out
        assert "Status: completed" in output
        assert "SDK flow completed" in output
        conversation = client.create_session(root=str(work))
        turn = conversation.send("Run the progress fixture.")
        result = recipe("progress").watch_turn(conversation, turn)
        assert result["status"] == "completed"
        assert requests


def test_capability_recipe_declares_local_function_and_activity(recipe, monkeypatch):
    registration = vis._registration["spec"]
    updates = []
    monkeypatch.setattr(
        vis._host, "activity", lambda value: updates.append(value) or True
    )
    module = recipe("delivery_task")
    weights = []
    declaration = module.make_delivery_extension(weights)._spec()
    assert vis._registration["spec"] is registration
    assert declaration["name"] == "delivery"
    (tool,) = declaration["symbols"]
    assert tool["contract"]["name"] == "delivery_quote"
    assert _contracts.validate("symbol", "callable", tool["contract"])
    assert "express" in tool["doc"] and "False" in tool["doc"]
    assert tool["activity"]["label"] == "Quote delivery"
    assert tool["activity"]["show_start"] is False
    assert tool["fn"](1200, express=True) == 1200
    assert updates == [
        {"headline": "Quote delivery", "summary": "1200 cents", "content": []}
    ]
    assert tool["fn"](1200) == 700
    assert tool["fn"](1) == 600
    assert tool["fn"](1000) == 600
    assert tool["fn"](1001) == 700
    assert weights == [1200, 1200, 1, 1000, 1001]
    for weight in (0, -1):
        with pytest.raises(ValueError, match="positive"):
            tool["fn"](weight)
    assert weights == [1200, 1200, 1, 1000, 1001]
    for phase in ("start", "failure"):
        assert module.quote_activity(phase=phase, result=None) is None


@pytest.mark.parametrize("transport", ["http", "stdio"])
def test_capability_recipe_runs_in_application(
    recipe, tmp_path, monkeypatch, capsys, transport
):
    code = """
print(apropos(r"^delivery_quote$"))
print(doc("delivery_quote"))
print("DELIVERY_QUOTE", await delivery_quote(1200, express=True))
try:
    await delivery_quote(0)
except Exception as error:
    print("INVALID_WEIGHT", "positive" in str(error))
"""
    with sdk_fixture(tmp_path, monkeypatch, transport, tool_code=code) as (
        client,
        work,
        requests,
    ):
        module = recipe("delivery_task")
        weights = []
        with engine.Agent(
            work,
            execution_layer=client,
            extensions=[module.make_delivery_extension(weights)],
        ) as agent:
            result = module.quote_delivery(agent)
            assert result["status"] == "completed"
            assert weights == [1200]
        output = capsys.readouterr().out
        assert "Status: completed" in output
        tool_results = [
            message["content"]
            for request in requests
            for message in request["messages"]
            if message["role"] == "tool"
        ]
        assert tool_results, requests
        text = "\n".join(str(content) for content in tool_results)
        assert "DELIVERY_QUOTE 1200" in text
        assert "INVALID_WEIGHT True" in text
        assert "express" in text and "False" in text


def _jvm_candidate_classpath(classpath, version, candidate):
    if not candidate:
        return classpath
    assert version, "VIS_TEST_JVM_JAR requires VIS_TEST_PUBLISHED_JVM=<version>"
    candidate = Path(candidate)
    assert candidate.is_absolute() and candidate.is_file(), (
        "VIS_TEST_JVM_JAR must be an existing absolute JAR path"
    )
    entries = classpath.split(os.pathsep)
    coordinate = ("com", "blockether", "vis", version, f"vis-{version}.jar")
    matches = [
        index
        for index, entry in enumerate(entries)
        if Path(entry).parts[-5:] == coordinate
    ]
    assert len(matches) == 1, (
        f"Expected exactly one com.blockether:vis:{version} JAR; found {len(matches)}"
    )
    entries[matches[0]] = str(candidate)
    return os.pathsep.join(entries)


@pytest.mark.parametrize("version", [None, "0.2.2"])
def test_jvm_candidate_unset_preserves_classpath(version):
    classpath = os.pathsep.join(["src", "resources", "/example/clojure.jar"])
    assert _jvm_candidate_classpath(classpath, version, None) == classpath


def test_jvm_candidate_replaces_only_the_exact_maven_coordinate(tmp_path):
    candidate = tmp_path / "candidate.jar"
    candidate.write_bytes(b"candidate")
    entries = [
        "/example/org/clojure/clojure/1.12.4/clojure-1.12.4.jar",
        "/example/com/blockether/vis/0.2.2/vis-0.2.2.jar",
        "/example/com/blockether/vis/0.2.2/vis-0.2.2-sources.jar",
        "/example/com/blockether/vis/0.2.1/vis-0.2.1.jar",
    ]
    actual = _jvm_candidate_classpath(os.pathsep.join(entries), "0.2.2", candidate)
    assert actual.split(os.pathsep) == [entries[0], str(candidate), *entries[2:]]


@pytest.mark.parametrize("count", [0, 2])
def test_jvm_candidate_requires_exactly_one_resolved_vis_jar(tmp_path, count):
    candidate = tmp_path / "candidate.jar"
    candidate.write_bytes(b"candidate")
    entries = ["/example/com/blockether/vis/0.2.2/vis-0.2.2.jar"] * count
    with pytest.raises(AssertionError, match=f"found {count}"):
        _jvm_candidate_classpath(os.pathsep.join(entries), "0.2.2", candidate)


@pytest.mark.parametrize("candidate", ["relative.jar", "missing.jar", "."])
def test_jvm_candidate_requires_an_existing_absolute_file(tmp_path, candidate):
    path = candidate if candidate == "relative.jar" else tmp_path / candidate
    with pytest.raises(AssertionError, match="existing absolute JAR path"):
        _jvm_candidate_classpath("", "0.2.2", path)


def test_jvm_candidate_requires_published_mode(tmp_path):
    with pytest.raises(AssertionError, match="requires VIS_TEST_PUBLISHED_JVM"):
        _jvm_candidate_classpath("src", None, tmp_path / "candidate.jar")


@pytest.fixture
def jvm_recipe(tmp_path):
    if not os.environ.get("VIS_TEST_LOCAL_COMMAND"):
        pytest.skip("set VIS_TEST_LOCAL_COMMAND to the actual engine argv")
    repository = Path(__file__).parents[3]
    document = (repository / "resources/vis-docs/jvm-sdk.md").read_text()
    source = re.search(r"```java\n(.*?)\n```", document, re.S)
    assert source, "Missing executable Java example"
    java = tmp_path / "VisExample.java"
    java.write_text(source[1])
    clojure = re.search(r"```clojure\n(.*?)\n```", document, re.S)
    assert clojure, "Missing executable Clojure example"
    task = tmp_path / "task.clj"
    task.write_text(clojure[1])
    # Build dependencies belong to the caller, not the isolated engine's home.
    if version := os.environ.get("VIS_TEST_PUBLISHED_JVM"):
        # Optional release-closure check, separate from the source-based guide.
        # Published 0.2.2 fails loading vis-python-runtime; do not hide that failure.
        (tmp_path / "pom.xml").write_text(
            '<project xmlns="http://maven.apache.org/POM/4.0.0">'
            "<modelVersion>4.0.0</modelVersion><groupId>example</groupId>"
            "<artifactId>vis-guide-test</artifactId><version>1</version>"
            "<repositories><repository><id>clojars</id>"
            "<url>https://repo.clojars.org</url></repository></repositories>"
            "<dependencies><dependency><groupId>com.blockether</groupId>"
            "<artifactId>vis</artifactId><version>" + version + "</version>"
            "</dependency></dependencies></project>"
        )
        resolution = subprocess.run(
            [
                "mvn",
                "-q",
                "dependency:build-classpath",
                "-Dmdep.outputFile=classpath.txt",
            ],
            cwd=tmp_path,
            capture_output=True,
            text=True,
            timeout=300,
        )
        assert resolution.returncode == 0, resolution.stdout + resolution.stderr
        classpath = (tmp_path / "classpath.txt").read_text().strip()
    else:
        resolution = subprocess.run(
            ["clojure", "-Spath"],
            cwd=repository,
            capture_output=True,
            text=True,
            timeout=120,
        )
        assert resolution.returncode == 0, resolution.stderr
        classpath = resolution.stdout.strip()
    classpath = _jvm_candidate_classpath(
        classpath, version, os.environ.get("VIS_TEST_JVM_JAR")
    )
    compilation = subprocess.run(
        ["javac", "-cp", classpath, str(java)],
        capture_output=True,
        text=True,
        timeout=60,
    )
    assert compilation.returncode == 0, compilation.stderr

    def run(client, work, language):
        main = (
            ["VisExample", str(work), "Summarize this project without changing files."]
            if language == "java"
            else ["clojure.main", str(task)]
        )
        outcome = subprocess.run(
            ["java", "-cp", os.pathsep.join([str(tmp_path), classpath]), *main],
            cwd=repository,
            env={
                **os.environ,
                "VIS_GATEWAY_URL": client._url,
                "VIS_GATEWAY_TOKEN": client._token,
                "VIS_PROJECT_ROOT": str(work),
            },
            capture_output=True,
            text=True,
            timeout=300,
        )
        assert outcome.returncode == 0, outcome.stdout + outcome.stderr
        expected = "Status: completed" if language == "java" else '"status" "completed"'
        assert expected in outcome.stdout, outcome.stdout
        assert client._token not in outcome.stdout + outcome.stderr
        return outcome.stdout

    return run


@pytest.mark.parametrize("language", ["java", "clojure"])
def test_jvm_recipe_runs_against_real_gateway(
    jvm_recipe, tmp_path, monkeypatch, language
):
    with sdk_fixture(
        tmp_path, monkeypatch, "http", tool_code='print("JVM guide tool completed")'
    ) as (client, work, requests):
        assert "SDK flow completed" in jvm_recipe(client, work, language)
        assert requests


@pytest.fixture
def live_project(tmp_path, monkeypatch):
    provider = os.environ.get("VIS_TEST_LIVE_PROVIDER")
    model = os.environ.get("VIS_TEST_LIVE_MODEL")
    if not (provider and model and os.environ.get("VIS_TEST_LOCAL_COMMAND")):
        pytest.skip(
            "live model calls require VIS_TEST_LIVE_PROVIDER, VIS_TEST_LIVE_MODEL and VIS_TEST_LOCAL_COMMAND"
        )
    work = tmp_path / "live-project"
    config = work / ".vis"
    config.mkdir(parents=True)
    (config / "config.yml").write_text(
        json.dumps(
            {
                "default_provider": provider,
                "default_model": model,
                "toggles": {"council": False},
            }
        )
    )
    # Gateway credentials and its database must not be part of the inspected project.
    state = tmp_path / "gateway-state"
    (state / ".vis").mkdir(parents=True)
    (state / ".vis/config.yml").write_text((config / "config.yml").read_text())
    (work / "README.md").write_text(
        "A disposable project for testing Vis SDK calls. No files should change.\n"
    )
    # Keep configured provider credentials, but do not inherit a dev launcher track.
    monkeypatch.setenv("VIS_HOME", str(tmp_path / "install-state"))
    monkeypatch.delenv("VIS_GATEWAY_URL", raising=False)
    monkeypatch.delenv("VIS_GATEWAY_TOKEN", raising=False)
    return work, shlex.split(os.environ["VIS_TEST_LOCAL_COMMAND"]), state


@pytest.mark.parametrize("transport", ["http", "stdio"])
def test_live_python_recipes(recipe, live_project, monkeypatch, capsys, transport):
    work, command, state = live_project
    monkeypatch.chdir(work)
    before = (work / "README.md").read_bytes()
    if transport == "stdio":
        # Execute the literal default launcher + project recipe, not an Agent double.
        assert len(command) == 1, (
            "the live quickstart test needs one installed launcher path"
        )
        monkeypatch.setenv(
            "PATH", str(Path(command[0]).parent) + os.pathsep + os.environ["PATH"]
        )
        recipe("local_task").main()
        with engine.Agent() as agent:
            turn = agent.send(
                "Use Python to compute 7 * 6. Print the result, then reply SDK_GUIDE_OK 42. Do not read or change files."
            )
            result = recipe("progress").watch_turn(agent.session, turn)
            assert result["status"] == "completed"
            assert "SDK_GUIDE_OK 42" in json.dumps(result["content"])
    else:
        with real_client("http", command, state) as client:
            connection_environment(monkeypatch, client, work)
            recipe("gateway_task").main()
            with engine.Agent(work, execution_layer=client) as agent:
                turn = agent.send(
                    "Use Python to compute 7 * 6. Print the result, then reply SDK_GUIDE_OK 42. Do not read or change files."
                )
                result = recipe("progress").watch_turn(agent.session, turn)
                assert result["status"] == "completed"
                assert "SDK_GUIDE_OK 42" in json.dumps(result["content"])
    output = capsys.readouterr().out
    assert "Status: completed" in output
    assert "sdk-fixture-token" not in output
    assert (work / "README.md").read_bytes() == before


@pytest.mark.parametrize("transport", ["http", "stdio"])
def test_live_agent_repairs_python_project(live_project, monkeypatch, transport):
    """Dogfood file inspection, editing and tests with a real provider, opt-in only."""
    work, command, state = live_project
    monkeypatch.chdir(work)
    (work / "README.md").write_text(
        "Repair invoices.py using test_invoices.py. Only paid invoices count. "
        "Use exact Decimal arithmetic, including negative adjustments. "
        "Return Decimal zero for empty input; accept any iterable.\n"
    )
    module = work / "invoices.py"
    module.write_text(
        "def invoice_total(rows):\n"
        "    return sum(float(row['amount']) for row in rows)\n"
    )
    tests = work / "test_invoices.py"
    tests.write_text(
        textwrap.dedent(
            """\
            import unittest
            from decimal import Decimal
            from invoices import invoice_total


            class InvoiceTests(unittest.TestCase):
                def test_paid_only(self):
                    rows = [
                        {"status": "paid", "amount": "19.95"},
                        {"status": "open", "amount": "100.00"},
                        {"status": "cancelled", "amount": "25.00"},
                        {"status": "paid", "amount": "0.05"},
                    ]
                    self.assertEqual(invoice_total(rows), Decimal("20.00"))

                def test_exact_decimal(self):
                    rows = [{"status": "paid", "amount": x} for x in ["0.10", "0.20"]]
                    result = invoice_total(rows)
                    self.assertIsInstance(result, Decimal)
                    self.assertEqual(result, Decimal("0.30"))

                def test_empty(self):
                    result = invoice_total([])
                    self.assertIsInstance(result, Decimal)
                    self.assertEqual(result, Decimal("0"))

                def test_iterable_and_adjustment(self):
                    rows = ({"status": "paid", "amount": x} for x in ["10.00", "-2.50"])
                    self.assertEqual(invoice_total(rows), Decimal("7.50"))
            """
        )
    )
    protected = {path: path.read_bytes() for path in [tests, work / "README.md"]}
    original = module.read_bytes()
    test_command = [sys.executable, "-m", "unittest", "-v"]
    baseline = subprocess.run(
        test_command, cwd=work, capture_output=True, text=True, timeout=15
    )
    assert baseline.returncode != 0 and "FAILED" in baseline.stderr
    with real_client(transport, command, state) as client:
        session = client.create_session(root=str(work), channel="app")
        try:
            turn = session.send(
                "Fix invoices.py to meet README.md and test_invoices.py. "
                "First run python -m unittest -v to reproduce the failures, then "
                "inspect and patch the implementation and rerun the tests. "
                "Modify only invoices.py. Do not change tests or config, install "
                "packages, commit, push, access the network, or read outside this project. "
                "Finish with a short explanation of the bug and the test result."
            )
            assert turn.wait(timeout=240)["status"] == "completed"
            assert module.read_bytes() != original
            assert all(
                path.read_bytes() == content for path, content in protected.items()
            )
            verified = subprocess.run(
                test_command, cwd=work, capture_output=True, text=True, timeout=15
            )
            assert verified.returncode == 0, verified.stdout + verified.stderr
            assert "Ran 4 tests" in verified.stderr
        finally:
            session.delete()


@pytest.mark.parametrize("language", ["java", "clojure"])
def test_live_jvm_recipes(live_project, jvm_recipe, language):
    work, command, state = live_project
    with real_client("http", command, state) as client:
        jvm_recipe(client, work, language)


def test_native_guide_packages_and_runs(tmp_path, monkeypatch):
    binary = os.environ.get("VIS_TEST_NATIVE_BINARY")
    if not binary:
        pytest.skip("set VIS_TEST_NATIVE_BINARY to package an existing native build")
    repository = Path(__file__).parents[3]
    bundle = tmp_path / "bundle"
    archive = tmp_path / "vis-agent-local.tar.gz"
    staging = subprocess.run(
        [str(repository / "bin/stage-release-bundle"), binary, str(archive)],
        cwd=repository,
        env={**os.environ, "VIS_BUNDLE_DIR": str(bundle)},
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert staging.returncode == 0, staging.stdout + staging.stderr
    assert archive.is_file()
    for name in [
        "vis-agent",
        "vis-agent-native",
        "vis-agent-native.build",
        "vis-agent-python",
    ]:
        assert (bundle / name).exists()
    home = tmp_path / "native-test-home"
    home.mkdir()
    launcher = bundle / "vis-agent"
    version = subprocess.run(
        [str(launcher), f"-Duser.home={home}", "--version"],
        env={
            **os.environ,
            "HOME": str(home),
            "VIS_HOME": str(home / ".vis"),
            "JAVA_CMD": str(tmp_path / "no-jvm"),
        },
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert version.returncode == 0, version.stdout + version.stderr
    assert version.stdout.strip()
    monkeypatch.setenv("VIS_TEST_LOCAL_COMMAND", str(launcher))
    with sdk_fixture(
        tmp_path / "execution",
        monkeypatch,
        "stdio",
        tool_code='print("Packaged guide tool completed")',
    ) as (client, work, requests):
        turn = client.create_session(root=str(work)).send(
            "Run the packaged guide fixture."
        )
        assert turn.wait(timeout=60)["status"] == "completed"
        assert requests
