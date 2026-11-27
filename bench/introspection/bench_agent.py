"""Harbor Vis agent for the synthetic session benchmark."""

import json
import os
import shlex
import subprocess
from pathlib import Path

from blockether.vis.evals.agent import VisAgent, source_classpath

HERE = Path(__file__).resolve().parent
TASKS = HERE.parents[1] / "e2e"
SID = "10200000-0000-4000-8000-000000000001"
TURN = "10200000-0000-4000-8001-000000000002"


def journal(kind):
    """Return scoped events and adjacent distractors using the gateway wire shape."""
    if kind not in {"wait", "origin", "task", "log"}:
        return []
    task = {"id": "t1", "kind": "shell", "session_id": SID, "name": "Check release",
            "origin": {"turn": 2, "iteration": 2}, "started_at": 1746139203000,
            "view_ids": [], "detached": False}
    rows = [
        {"type": "turn.started", "request_kind": "council" if kind == "origin" else "user"},
        {"type": "turn.progress", "iteration": 2, "progress": "provider-wait", "connection": "silent", "awaiting_output": True, "silent_ms": 18000},
        {"type": "task.started", "task": {**task, "state": "running"}},
        {"type": "turn.progress", "iteration": 2, "progress": "provider-wait", "connection": "silent", "awaiting_output": True, "silent_ms": 47000},
        {"type": "task.ended", "task": {**task, "state": "failed"}},
        {"type": "turn.completed", "status": "done"} if kind in {"task", "origin"}
        else {"type": "turn.failed", "status": "error"},
        {"type": "turn.started", "turn_id": "10200000-0000-4000-8001-000000000003", "position": 3, "request_kind": "user"},
    ]
    return [{"session_id": SID, "seq": n, "ts": 1746139200000 + n * 1000,
             **({"position": 2, "turn_id": TURN} if row["type"].startswith("turn.") else {}), **row}
            for n, row in enumerate(rows, 1)]


def write_fallback(workspace, home, kind):
    """Install the same fake journal at the host and the documented fallback path."""
    if rows := journal(kind):
        body = "".join(json.dumps(row) + "\n" for row in rows)
        base = home / "gateway" / "events"
        base.mkdir(parents=True, exist_ok=True)
        (base / f"{SID}.ndjson").write_text(body, encoding="utf-8")
    source = workspace / "diagnostics" / "vis.log"
    if kind == "log":
        base = home / "logs"
        base.mkdir(parents=True, exist_ok=True)
        (base / "vis.log").write_bytes(source.read_bytes())


class InspectionAgent(VisAgent):
    """Seed once before the first measured model call and capture its accounting."""

    def vis_arguments(self):
        return [*super().vis_arguments(), "--toggles", "introspection=true,council=true,subagents=true"]

    def process_env(self, instruction, runtime, **kwargs):
        values = super().process_env(instruction, runtime, **kwargs)
        home = runtime / "home"
        values.update(VIS_HOME=str(home), VIS_BENCH_KIND=self.kind)
        write_fallback(self.workspace, home, self.kind)
        # Config holds an environment reference, never a credential value.
        home.mkdir(parents=True, exist_ok=True)
        config = {"providers": [{"id": self.provider, "api_key": "${VIS_BENCH_API_KEY}",
                                 "models": [self.model]}]}
        handoff = os.environ.get("VIS_BENCH_HANDOFF")
        if handoff is not None:
            if handoff not in {"full", "bounded"}:
                raise ValueError("VIS_BENCH_HANDOFF must be full or bounded")
            config["toggles"] = {"subagent_handoff": handoff}
        (home / "config.yml").write_text(json.dumps(config) + "\n", encoding="utf-8")
        return values

    async def setup(self, environment):
        await super().setup(environment)
        task = environment.environment_dir.parent / "tests" / "fixture.json"
        self.kind = json.loads(task.read_text(encoding="utf-8"))["kind"]
        self.workspace = environment.workspace
        if not os.environ.get("VIS_BENCH_API_KEY"):
            raise ValueError("Set VIS_BENCH_API_KEY for the paid trial route")
        source = Path(os.environ["VIS_BENCH_SOURCE"]).resolve(strict=True)
        self.fixture_cp = source_classpath(self.clojure, source)

    def harness_command(self, name):
        form = "(load-file " + json.dumps(str(HERE / name)) + ")"
        return shlex.join([self.clojure, "-Scp", self.fixture_cp, "-M", "-e", form])

    def shell_command(self):
        prelude = (
            'export VIS_BENCH_ROOT="$VIS_HARBOR_WORKSPACE" VIS_BENCH_DB="$VIS_DB_PATH"; '
            'unset VIS_GATEWAY_URL; '
        )
        if self.kind not in {"handoff", "resume"}:
            prelude += f'{self.harness_command("seed.clj")} > /dev/null || exit $?; '
        return prelude + super().shell_command()

    def _capture_task_evidence(self, runtime):
        # The parent must not read the operator’s real journal.
        from blockether.vis.evals.agent import _session_id
        from blockether.vis.evals.grade import TRAJECTORY_FILE
        from blockether.vis.evals.task_evidence import FILE, capture

        sid = _session_id(self.logs_dir / TRAJECTORY_FILE)
        db_dir = runtime / "vis.mdb"
        if sid is None:
            return
        evidence = capture(sid, runtime / "home", db_dir / "vis.db")
        (self.logs_dir / FILE).write_text(json.dumps(evidence) + "\n", encoding="utf-8")
        # The collector sees only the just-finished trial's DB; never copy it.
        command = shlex.split(self.harness_command("accounting.clj"))
        env = dict(os.environ, VIS_HOME=str(runtime / "home"),
                   VIS_BENCH_DB=str(db_dir), VIS_BENCH_SID=sid)
        try:
            result = subprocess.run(command, env=env, capture_output=True, check=True,
                                    text=True, timeout=90, cwd=self.workspace)
            prefix = "VIS_BENCH_ACCOUNTING "
            accounting = json.loads(next(line[len(prefix):] for line in result.stdout.splitlines()
                                         if line.startswith(prefix)))
        except (subprocess.SubprocessError, OSError, StopIteration, ValueError) as error:
            self.logger.warning("Prompt accounting unavailable (%s)", type(error).__name__)
        else:
            (self.logs_dir / "prompt-accounting.json").write_text(
                json.dumps(accounting) + "\n", encoding="utf-8")
