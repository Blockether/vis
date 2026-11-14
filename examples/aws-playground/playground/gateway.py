"""The Vis gateway: the engine process of the worker.

The gateway listens only on the loopback interface with a token that this worker
makes at each start. Its model provider is the broker of this worker, so the gateway
never holds a provider key. Its environment has no AWS credentials.
"""

from __future__ import annotations

import http.client
import json
import os
import secrets
import subprocess
import time
from typing import Any

from playground.env import (
    ENV,
    HOME,
    MODELS,
    PRIVATE_TMP,
    WORKER_DIR,
    log,
    private,
    write_secret,
)

BINARY = "/opt/vis/vis-agent-native"
HOST, PORT, TOKEN_FILE = "127.0.0.1", 7890, f"{WORKER_DIR}/gateway.token"
PROVIDER = "playground"
# The variables that the gateway and the tools of a session get. No AWS credentials.
PASSED = ("PATH", "LANG", "LC_ALL", "TZ", "VIS_EXTENSIONS_DIR", "VIS_PYTHON_PYCACHE_PREFIX", "VIS_PYTHON", "JAVA_HOME", "MAVEN_LOCAL_REPO", "CLJ_CONFIG", "VIS_WEB_DIR")


def config(broker: str) -> str:
    """Return the Vis configuration: one provider, which is the broker."""
    models = "".join(f"      - name: {m['name']}\n        context: {m['context']}\n        output_limit: {m['output_limit']}\n        is_tool_calls_supported: true\n" for m in MODELS)
    return (
        "providers:\n"
        f"  - id: {PROVIDER}\n    compatibility: openai\n    base_url: {broker}/v1\n"
        # The broker ignores this value. The real key lives only in the llm function.
        f"    api_key: not-a-key\n    models:\n{models}"
    )


def environment(broker: str) -> dict[str, str]:
    """Return the environment of the gateway and of each tool that a session starts."""
    env = {name: ENV[name] for name in PASSED if name in ENV}
    env.update(
        HOME=HOME,
        TMPDIR="/tmp",
        VIS_PYTHON_NATIVE_PATH="/opt/vis/vis-agent-python/libvispython.so",
        PLAYGROUND_BROKER=broker,
        PLAYGROUND_PRIVATE_TMP=":".join(PRIVATE_TMP),
        # The release directory on EFS has another owner. Git must accept it.
        GIT_CONFIG_COUNT="3",
        GIT_CONFIG_KEY_0="safe.directory",
        GIT_CONFIG_VALUE_0="*",
        GIT_CONFIG_KEY_1="user.name",
        GIT_CONFIG_VALUE_1="Vis Playground",
        GIT_CONFIG_KEY_2="user.email",
        GIT_CONFIG_VALUE_2="playground@blockether.com",
    )
    return env


class Gateway:
    """One gateway process and the calls that the worker makes to it."""

    def __init__(self, project: str, broker: str) -> None:
        self.project, self.broker = project, broker
        self.token = secrets.token_urlsafe(32)
        self.protocol = ""
        self.process: subprocess.Popen[bytes] | None = None

    def start(self) -> None:
        os.makedirs(os.path.join(HOME, ".vis"), exist_ok=True)
        with open(os.path.join(HOME, ".vis", "config.yml"), "w", encoding="utf-8") as file:
            file.write(config(self.broker))
        write_secret(private("gateway.token"), self.token)
        command = [BINARY, f"-Duser.home={HOME}", "gateway", "start", "--host", HOST, "--port", str(PORT)]
        self.process = subprocess.Popen([*command, "--token-file", TOKEN_FILE, "--require-token"], cwd=self.project, env=environment(self.broker))
        deadline = time.time() + 60
        while time.time() < deadline:
            if self.process.poll() is not None:
                raise RuntimeError(f"the gateway stopped with exit code {self.process.returncode}")
            try:
                status, health = self.call("GET", "/healthz", timeout=2)
                if status == 200:
                    self.protocol = str(health["protocol"]["protocol"])
                    log("gateway ready", health["protocol"]["version"])
                    return
            except OSError:
                time.sleep(0.2)
        raise RuntimeError("the gateway did not start in 60 s")

    def headers(self) -> dict[str, str]:
        """Return the headers of a call that the worker makes or relays."""
        head = {"authorization": f"Bearer {self.token}", "content-type": "application/json"}
        return {**head, "x-vis-protocol": self.protocol} if self.protocol else head

    def call(self, method: str, path: str, body: Any = None, timeout: float = 30) -> tuple[int, Any]:
        """Call the gateway and return the status and the JSON answer."""
        connection = http.client.HTTPConnection(HOST, PORT, timeout=timeout)
        try:
            connection.request(method, path, None if body is None else json.dumps(body), self.headers())
            answer = connection.getresponse()
            raw = answer.read()
            return answer.status, (json.loads(raw) if raw else None)
        finally:
            connection.close()

    def cancel(self, session: str) -> None:
        """Cancel the turn that runs in a session. The gateway cancels a turn only by its id."""
        _, row = self.call("GET", f"/v1/sessions/{session}")
        turn = (row or {}).get("current_turn_id")
        if turn:
            self.call("POST", f"/v1/sessions/{session}/turns/{turn}/cancel", {})

    def running(self) -> int:
        """Return the number of sessions with a running turn."""
        count, cursor = 0, ""
        while True:
            _, page = self.call("GET", f"/v1/sessions?limit=200{cursor}")
            rows = (page.get("sessions") or []) + (page.get("awaiting") or [])
            count += sum(1 for row in rows if row.get("status") == "running")
            if not page.get("has_more") or not page.get("next_cursor"):
                return count
            cursor = f"&after={page['next_cursor']}"

    def next_due(self) -> float | None:
        """Return the time of the next scheduled automation."""
        _, page = self.call("GET", "/v1/automations")
        rows = page if isinstance(page, list) else (page or {}).get("automations") or []
        times = [row["next_run_at"] / 1000 for row in rows if row.get("enabled") and row.get("next_run_at")]
        return min(times, default=None)

    def drain(self, timeout_s: float) -> int:
        """Park the running turns for the next worker. Return their number."""
        try:
            body = {"timeout_ms": int(timeout_s * 1000), "grace_ms": 10000}
            _, answer = self.call("POST", "/v1/admin/drain", body, timeout=timeout_s + 15)
            return int((answer or {}).get("parked") or 0)
        except (OSError, ValueError) as failure:
            log("drain failed", repr(failure))
            return 0

    def stop(self, kill: bool = False) -> None:
        """End the process. `kill` gives it no time to write: for a worker that lost the lease."""
        if self.process is None or self.process.poll() is not None:
            return
        self.process.kill() if kill else self.process.terminate()
        try:
            self.process.wait(timeout=20)
        except subprocess.TimeoutExpired:
            self.process.kill()
            self.process.wait(timeout=10)
