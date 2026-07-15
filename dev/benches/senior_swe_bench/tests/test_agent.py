from pathlib import Path
import asyncio
import importlib.util
import json

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("agent", ROOT / "agent.py")
agent = importlib.util.module_from_spec(spec)
spec.loader.exec_module(agent)  # type: ignore[union-attr]


class FakeEnvironment:
    def __init__(self):
        self.uploads = []
        self.execs = []
        self.trace_contents = None

    async def upload_file(self, local_path, remote_path):
        self.uploads.append((Path(local_path), remote_path))

    async def exec(self, **kwargs):
        self.execs.append(kwargs)
        command = kwargs["command"]
        if "--version" in command:
            return {"returncode": 0, "stdout": "vis 0.0-test\n", "stderr": ""}
        return {"returncode": 0, "stdout": "", "stderr": ""}

    async def download_file(self, source_path, target_path):
        if self.trace_contents is not None:
            target_path.parent.mkdir(parents=True, exist_ok=True)
            target_path.write_text(self.trace_contents)


def test_summarize_trace_counts_trace_chunk_iterations(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        "\n".join(
            [
                '{"event":"trace-chunk","payload":{"phase":"provider-call","iteration":1}}',
                '{"event":"trace-chunk","payload":{"phase":"reasoning","iteration":4}}',
                '{"event":"trace-chunk","payload":{"phase":"iteration-final","iteration":4}}',
            ]
        )
    )

    assert agent._summarize_trace(trace)["iterations"] == 4


def test_summarize_trace_counts_payload_tool_invocations_once(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        "\n".join(
            [
                json.dumps(
                    {
                        "event": "trace-chunk",
                        "payload": {
                            "phase": "form-start",
                            "scope": "t1/i1/f1",
                            "tool-name": "cat",
                            "iteration": 1,
                        },
                    }
                ),
                json.dumps(
                    {
                        "event": "trace-chunk",
                        "payload": {
                            "phase": "tool-start",
                            "scope": "t1/i1/f1",
                            "tool-event": {"op": "cat"},
                            "iteration": 1,
                        },
                    }
                ),
            ]
        )
    )

    summary = agent._summarize_trace(trace)

    assert summary["tool_calls"] == 1
    assert summary["file_reads"] == 1


def test_summarize_trace_reads_wrapped_final_usage(tmp_path):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(
        json.dumps(
            {
                "event": "trace-chunk",
                "payload": {
                    "phase": "iteration-final",
                    "iteration": 12,
                    "final": {
                        "iteration-count": 12,
                        "tokens": {"input": 100, "output": 20, "total": 120},
                        "cost": {"total-cost": 0.42},
                    },
                },
            }
        )
        + "\n"
    )

    summary = agent._summarize_trace(trace)

    assert summary["iterations"] == 12
    assert summary["tokens"]["total"] == 120
    assert summary["cost_usd"] == 0.42


def test_install_uploads_config_to_remote_home(monkeypatch, tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    artifact.write_bytes(b"artifact")
    config = tmp_path / "config.edn"
    config.write_text('{:providers [{:id :dummy :api-key "secret"}]}')
    metadata = tmp_path / "install-metadata.json"

    monkeypatch.setenv("VIS_BENCH_ARTIFACT", str(artifact))
    monkeypatch.setenv("VIS_BENCH_CONFIG", str(config))
    monkeypatch.setenv("VIS_BENCH_REMOTE_HOME", "/root")
    monkeypatch.setenv("VIS_BENCH_INSTALL_PREFIX", "/opt/vis")
    monkeypatch.setenv("VIS_BENCH_INSTALL_METADATA", str(metadata))

    environment = FakeEnvironment()
    asyncio.run(agent.VisInstalledAgent().install(environment))

    assert environment.uploads == [
        (artifact.resolve(), "/tmp/vis-agent.tar.gz"),
        (config.resolve(), "/tmp/vis-config.edn"),
    ]

    install_cmd = environment.execs[0]["command"]
    assert "vis-agent/install.sh" in install_cmd
    assert "/opt/vis" in install_cmd
    assert environment.execs[0]["user"] == "root"

    config_cmd = environment.execs[1]["command"]
    assert "mkdir -p /root/.vis" in config_cmd
    assert "cp /tmp/vis-config.edn /root/.vis/config.edn" in config_cmd
    assert "chmod 600 /root/.vis/config.edn" in config_cmd

    version_cmd = environment.execs[2]["command"]
    assert "HOME=/root" in version_cmd
    assert "/opt/vis/vis" in version_cmd
    assert "-Duser.home=/root" in version_cmd

    install_meta = json.loads(metadata.read_text())
    assert install_meta["config_uploaded"] is True
    assert install_meta["remote_home"] == "/root"
    assert install_meta["install_prefix"] == "/opt/vis"


def test_version_command_and_parser_use_configured_install(monkeypatch):
    monkeypatch.setenv("VIS_BENCH_REMOTE_HOME", "/root")
    monkeypatch.setenv("VIS_BENCH_INSTALL_PREFIX", "/opt/vis")
    installed = agent.VisInstalledAgent()

    assert installed.get_version_command() == "HOME=/root /opt/vis/vis -Duser.home=/root --version"
    assert installed.parse_version("startup noise\nvis e674131ef\n") == "vis e674131ef"


def test_run_sets_provider_native_effort_and_populates_harbor_context(monkeypatch, tmp_path):
    monkeypatch.setenv("VIS_BENCH_INSTALL_PREFIX", "/opt/vis")
    monkeypatch.setenv("VIS_BENCH_REMOTE_HOME", "/root")
    monkeypatch.setenv("VIS_BENCH_REASONING_EFFORT", "max")
    environment = FakeEnvironment()
    environment.trace_contents = json.dumps(
        {
            "payload": {
                "eval": {
                    "valid?": True,
                    "invalid-reasons": [],
                    "reasoning-effort": {"requested": "max", "iterations": []},
                }
            }
        }
    ) + "\n" + json.dumps(
        {
            "payload": {
                "phase": "iteration-final",
                "iteration": 3,
                "final": {"iteration-count": 3, "tokens": {"input": 100, "output": 20, "total": 120}},
            }
        }
    ) + "\n"
    context = {"task_id": "task-a", "workspace": str(tmp_path)}

    installed = agent.VisInstalledAgent()
    installed.logs_dir = tmp_path
    asyncio.run(installed.run("inspect", environment, context))

    assert "--reasoning-effort max" in environment.execs[0]["command"]
    assert "reasoning-level" not in environment.execs[0]["command"]
    assert context["n_input_tokens"] == 100
    assert context["n_output_tokens"] == 20
    assert context["metadata"]["vis"]["reasoning_effort"] == "max"
    assert context["metadata"]["vis"]["reasoning_effort_explicit"] is True
    assert context["metadata"]["vis"]["eval_valid"] is True
