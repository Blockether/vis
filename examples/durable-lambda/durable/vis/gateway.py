"""The Vis gateway: the engine process of the worker.

The gateway listens only on the loopback interface with a token that this worker
makes at each start. Its model provider is the broker of this worker, so the gateway
never holds a provider key. Its environment has no AWS credentials.

The worker calls the gateway with the Vis SDK: `Gateway.client` gives the
`GatewayClient` of the calling thread. Two routes are not in the SDK, because they are
not for an application: the health check and the drain. `Gateway.call` is for them.
The relay sends the calls of the browser to the gateway as they are (see `vis/relay.py`).
"""

from __future__ import annotations

import http.client
import json
import os
import secrets
import subprocess
import threading
import time
from typing import Any

from blockether.vis.engine import GatewayClient, GatewayError

from durable import shards
from durable.helpers import log, private, write_secret
from durable.settings import (
    DRAIN_GRACE_S,
    ENV,
    GATEWAY_PORT,
    GATEWAY_START_S,
    GATEWAY_URL,
    GIT_EMAIL,
    GIT_NAME,
    HOME,
    LOOPBACK,
    MAIN,
    MODELS,
    PRIVATE_TMP,
    PROVIDER,
    RUNS_GROUP,
    TEXT_STEPS,
    TEXT_TURNS,
    TOKEN_FILE,
    VIS_BINARY,
    VIS_PYTHON_NATIVE,
    WORK,
)
from durable.worktree import RELEASE_DIR, RUN_DIR

# The ids of the sessions of this home: the list of the gateway does not have a session without a turn or a subagent.
SESSIONS = os.path.join(HOME, ".vis", "durable-sessions.json")
# The variables that the gateway and the tools of a session get. No AWS credentials.
PASSED = ("PATH", "LANG", "LC_ALL", "TZ", "VIS_EXTENSIONS_DIR", "VIS_PYTHON_PYCACHE_PREFIX", "VIS_PYTHON", "VIS_WEB_DIR", "VIS_SPEECH_MODELS_DIR", "VIS_MACHINE_NAME")


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
        VIS_PYTHON_NATIVE_PATH=VIS_PYTHON_NATIVE,
        # `settings.py` reads the first as BROKER. The Vis extension of the image reads the two.
        DURABLE_BROKER=broker,
        DURABLE_PRIVATE_TMP=":".join(PRIVATE_TMP),
        # The release directory on EFS has another owner. Git must accept it.
        GIT_CONFIG_COUNT="3",
        GIT_CONFIG_KEY_0="safe.directory",
        GIT_CONFIG_VALUE_0="*",
        GIT_CONFIG_KEY_1="user.name",
        GIT_CONFIG_VALUE_1=GIT_NAME,
        GIT_CONFIG_KEY_2="user.email",
        GIT_CONFIG_VALUE_2=GIT_EMAIL,
    )
    return env


class Gateway:
    """One gateway process and the calls that the worker makes to it."""

    def __init__(self, project: str, broker: str) -> None:
        """Make the token of the gateway. `project` is its directory and `broker` is the address of its model provider."""
        self.project, self.broker = project, broker
        self.token = secrets.token_urlsafe(32)
        self.protocol = ""
        self.process: subprocess.Popen[bytes] | None = None
        # The SDK client of each thread: one client serves one thread.
        self._clients = threading.local()
        # The sessions whose shard this worker recorded. One thread at a time records.
        self._published: set[str] = set()
        self._lock = threading.RLock()
        self._teams: dict[str, int] = {}
        # The list rows that this worker recorded last.
        self._shown = ""
        # The texts of each session that this worker recorded, the texts of each of its turns, and the last turn that they have.
        self._texts: dict[str, list[list[Any]]] = {}
        self._turns: dict[str, dict[int, list[list[Any]]]] = {}
        self._told: dict[str, tuple[Any, Any]] = {}
        # The ids that `main` gave to the project and to its groups, each with the id of this home (see `vis/machine.py`).
        self.ids: list[tuple[bytes, bytes]] = []

    def start(self) -> None:
        """Write the configuration and the token, start the process and wait for its extensions."""
        os.makedirs(os.path.join(HOME, ".vis"), exist_ok=True)
        with open(os.path.join(HOME, ".vis", "config.yml"), "w", encoding="utf-8") as file:
            file.write(config(self.broker))
        write_secret(private(os.path.basename(TOKEN_FILE)), self.token)
        command = [VIS_BINARY, f"-Duser.home={HOME}", "gateway", "start", "--host", LOOPBACK, "--port", str(GATEWAY_PORT)]
        self.process = subprocess.Popen([*command, "--token-file", TOKEN_FILE, "--require-token"], cwd=self.project, env=environment(self.broker))
        deadline = time.time() + GATEWAY_START_S
        while time.time() < deadline:
            if self.process.poll() is not None:
                raise RuntimeError(f"the gateway stopped with exit code {self.process.returncode}")
            try:
                status, health = self.call("GET", "/healthz", timeout=2)
                if status == 200:
                    self.protocol = str(health["protocol"]["protocol"])
                    # The gateway answers before it loads its extensions. A settings list
                    # from that time lacks their rows, and its revision is then refused.
                    stage = ((self.call("GET", "/v1/admin/status", timeout=2)[1] or {}).get("extensions") or {}).get("stage")
                    if stage != "initializing":
                        log("gateway ready", health["protocol"]["version"], "extensions", stage)
                        return
            except OSError:
                pass
            time.sleep(0.2)
        raise RuntimeError(f"the gateway did not start in {GATEWAY_START_S} s")

    # -- the two ways to call the gateway -----------------------------------------

    def client(self) -> GatewayClient:
        """Return the SDK client of the calling thread.

        A client takes a lease in the gateway only for a live app, so it needs no
        close: the worker and its gateway stop together.
        """
        found = getattr(self._clients, "client", None)
        if found is None:
            found = self._clients.client = GatewayClient(GATEWAY_URL, token=self.token)
        return found

    def headers(self) -> dict[str, str]:
        """Return the headers of a call that the worker makes without the SDK, or relays."""
        head = {"authorization": f"Bearer {self.token}", "content-type": "application/json"}
        return {**head, "x-vis-protocol": self.protocol} if self.protocol else head

    def read(self, method: str, path: str, body: Any = None, timeout: float = 30) -> tuple[int, list[list[str]], bytes]:
        """Call the gateway without the SDK. Return the status, the headers and the bytes of the answer."""
        connection = http.client.HTTPConnection(LOOPBACK, GATEWAY_PORT, timeout=timeout)
        try:
            connection.request(method, path, None if body is None else json.dumps(body), self.headers())
            answer = connection.getresponse()
            return answer.status, [[name, value] for name, value in answer.getheaders()], answer.read()
        finally:
            connection.close()

    def inward(self, data: bytes) -> bytes:
        """Return a call with the ids of this home in the place of the ids of `main`."""
        for known, local in self.ids:
            data = data.replace(known, local)
        return data

    def outward(self, data: bytes) -> bytes:
        """Return an answer with the ids of `main` in the place of the ids of this home."""
        for known, local in self.ids:
            data = data.replace(local, known)
        return data

    def call(self, method: str, path: str, body: Any = None, timeout: float = 30) -> tuple[int, Any]:
        """Call a route that the SDK does not have. Return the status and the JSON answer."""
        status, _, raw = self.read(method, path, body, timeout)
        return status, (json.loads(raw) if raw else None)

    # -- the sessions -------------------------------------------------------------

    def sessions(self, **query: Any) -> list[dict[str, Any]]:
        """Return each session of a list, also the archived sessions. `query` selects the list.

        The SDK gives one page for each call. A failure raises: a part of the list is
        not the list, and a caller removes what no session uses.
        """
        rows: dict[str, dict[str, Any]] = {}
        query = {"limit": 200, "archived": "include", **query}
        while True:
            page = self.client().list_sessions(**query)
            rows.update((row["id"], row) for row in (page.get("sessions") or []) + (page.get("awaiting") or []))
            if not page.get("has_more") or not page.get("next_cursor"):
                return list(rows.values())
            query["after"] = page["next_cursor"]

    def publish(self, shard: str, is_full: bool = True) -> None:
        """Record the shard and the list row of each session that the gateway got. `is_full` also records the texts.

        The front sends the calls of a session to its shard, and shows and searches the sessions without a worker.
        """
        with self._lock:
            try:
                rows = self.sessions()
            except (GatewayError, OSError) as failure:
                return log("no list of the sessions", repr(failure))
            for row in rows:
                self.route(row["id"], shard)
                total = (row.get("team") or {}).get("total", 0)
                if total > self._teams.get(row["id"], 0):
                    try:
                        children = self.children(row["id"])
                    except GatewayError as failure:
                        log("no transcript of the leader", row["id"], repr(failure))
                        continue
                    for child in children:
                        self.route(child, shard)
                    self._teams[row["id"]] = total
            self.show(shard, rows)
            if is_full:
                self.tell(shard, rows)

    def show(self, shard: str, rows: list[dict[str, Any]]) -> None:
        """Record the list rows of the sessions when they changed. Each row gets the directory of the project, and the ids that `main` gave."""
        state = json.dumps([{name: value for name, value in row.items() if name != "server_time_ms"} for row in rows], sort_keys=True)
        if state == self._shown:
            return
        shards.show(shard, json.loads(self.outward(json.dumps([{**row, "project_root": self.project} for row in rows]).encode())))
        self._shown = state

    def tell(self, shard: str, rows: list[dict[str, Any]]) -> None:
        """Record the requests, the replies and the thinking of each session whose last turn changed. The search of the front reads them."""
        changed = bool(set(self._texts) - {row["id"] for row in rows})
        for row in rows:
            mark = (row.get("modified_at"), row.get("turn_count"))
            if self._told.get(row["id"]) == mark or row.get("status") == "running":
                continue
            try:
                turns = self.turns(row["id"], len(self._turns.get(row["id"]) or ()))
            except GatewayError as failure:
                log("no transcript of the session", row["id"], repr(failure))
                continue
            held = self._turns.setdefault(row["id"], {})
            for turn in turns:
                reply = "\n".join(block.get("markdown") or "" for block in turn.get("content") or [] if block.get("type") == "prose")
                steps = turn.get("iterations") or [{}]
                sides = [("request", turn.get("created_at"), turn.get("request") or ""), ("reply", steps[-1].get("created_at") or turn.get("created_at"), reply)]
                sides += [("thinking", step.get("created_at"), step.get("thinking") or "") for step in steps]
                held[turn["position"]] = [[side, at, text] for side, at, text in sides if text.strip()]
            self._texts[row["id"]] = [text for position in sorted(held) for text in held[position]]
            self._told[row["id"]], changed = mark, True
        if changed:
            self._texts = {row["id"]: self._texts[row["id"]] for row in rows if row["id"] in self._texts}
            shards.tell(shard, self._texts)

    def turns(self, session: str, known: int) -> list[dict[str, Any]]:
        """Return the turns of a session after its first `known` turns, each with each of its iterations.

        The gateway gives the newest turns first and can give fewer turns than a call asks for.
        The last known turn comes again: it can have a new reply.
        """
        found: list[dict[str, Any]] = []
        end, first = None, max(0, known - 1)
        while end is None or end > first:
            window = {} if end is None else {"offset": max(first, end - TEXT_TURNS), "limit": end - max(first, end - TEXT_TURNS)}
            page = self.client().get_session_transcript(session, query={"limit": TEXT_TURNS, "iteration_limit": TEXT_STEPS, **window})
            if not page.get("turns"):
                break
            found, end = page["turns"] + found, page.get("offset") or 0
        return [turn for turn in found if turn.get("position", 0) > first]

    def route(self, session: str, shard: str) -> None:
        """Record the shard of a session one time. The home of another shard than `main` keeps the id of the session."""
        with self._lock:
            if session in self._published:
                return
            shards.assign(session, shard)
            self._published.add(session)
            known = self.mine()
            if shard != MAIN and session not in known:
                with open(SESSIONS, "w", encoding="utf-8") as file:
                    json.dump(sorted(known | {session}), file)

    def mine(self) -> set[str]:
        """Return the id of each session that a worker of this home recorded."""
        try:
            with open(SESSIONS, encoding="utf-8") as file:
                return set(json.load(file))
        except FileNotFoundError:
            return set()

    def vacant(self) -> bool:
        """Return True when the gateway says that it has no session: its list is empty and each recorded session is gone."""
        try:
            return not self.sessions() and all(self.is_gone(session) for session in self.mine())
        except (GatewayError, OSError) as failure:
            log("no look at the sessions", repr(failure))
            return False

    def children(self, leader: str) -> list[str]:
        """Return the subagent sessions of a leader. The list of the sessions does not have them."""
        turns = self.client().get_session_transcript(leader).get("turns") or []
        return [agent["session_id"] for turn in turns for agent in turn.get("agents") or []]

    def roots(self) -> set[str]:
        """Return the directory of each session that has a turn. The gateway does not list an empty session."""
        return {os.path.realpath((row.get("workspace") or {}).get("root") or self.project) for row in self.sessions()}

    def soul(self, session: str) -> dict[str, Any] | None:
        """Return a session, or None when the gateway does not give it."""
        try:
            return self.client().get_session(session)
        except GatewayError:
            return None

    def is_gone(self, session: str) -> bool:
        """Return True when the gateway says that it does not have a session."""
        try:
            self.client().get_session(session)
        except GatewayError as failure:
            return failure.status == 404
        return False

    def busy(self, session: str) -> bool:
        """Return True while a turn runs in a session."""
        row = self.soul(session) or {}
        return row.get("status") == "running" or bool(row.get("current_turn_id"))

    def cancel(self, session: str) -> None:
        """Cancel the turn that runs in a session. The gateway cancels a turn only by its id."""
        turn = (self.soul(session) or {}).get("current_turn_id")
        if turn:
            try:
                self.client().post_session_turn_cancel(session, turn)
            except GatewayError as failure:  # the turn ended before the call
                log("no cancel of the turn", session, repr(failure))

    # -- the one project and its groups -------------------------------------------

    def group(self, root: str, name: str) -> str:
        """Return the id of a session group of the project. Make the group when the project does not have it."""
        client = self.client()
        listed = client.get_session_groups(query={"archived": "include", "root": root})
        found = next((row["id"] for row in (listed or {}).get("groups") or [] if row["name"] == name), None)
        return found or client.post_session_groups(body={"name": name, "root": root})["id"]

    def organize(self, root: str, name: str) -> str:
        """Make the one project and its group of workflow runs. Return the id of the group.

        The name is only for a new project, so a rename in Vis stays. Earlier versions
        made a project for each release and for each run. Their sessions move here: the
        session of a run joins the group and keeps its directory, and each other session
        moves to the project directory. A project that has no session left goes.
        """
        client = self.client()
        client.post_projects_ensure(body={"root": root, "name": name})
        group = self.group(root, RUNS_GROUP)
        for project in (client.get_projects() or {}).get("projects") or []:
            folder, parent = os.path.basename(str(project.get("workspace_root"))), os.path.dirname(str(project.get("workspace_root")))
            is_run = bool(RUN_DIR.fullmatch(folder))
            if parent != WORK or not (is_run or RELEASE_DIR.fullmatch(folder)):
                continue
            try:
                for session in self.sessions(project_id=project["id"]):
                    if is_run:
                        client.put_session_group(session["id"], body={"group_id": group})
                        continue
                    # The gateway answers 200 also when it did not move the session, so read the answer.
                    moved = client.patch_session_workspace_root(session["id"], body={"path": root})
                    if ((moved or {}).get("workspace") or {}).get("root") != root:
                        log("session stays in its earlier project", session["id"])
                if not self.sessions(project_id=project["id"]):
                    client.delete_project(project["id"])
            except GatewayError as failure:
                # The project stays, so the next worker tries again.
                log("an earlier project stays", project["id"], repr(failure))
        return group

    # -- what the worker loop asks ------------------------------------------------

    def running(self) -> int:
        """Return the number of sessions with a running turn. A gateway that refuses the list reports none."""
        try:
            return sum(1 for row in self.sessions() if row.get("status") == "running")
        except GatewayError as failure:
            log("no list of the sessions", repr(failure))
            return 0

    def next_due(self) -> float | None:
        """Return the time of the next scheduled automation, or None."""
        try:
            page = self.client().get_automations()
        except GatewayError as failure:
            log("no list of the automations", repr(failure))
            return None
        rows = page if isinstance(page, list) else (page or {}).get("automations") or []
        times = [row["next_run_at"] / 1000 for row in rows if row.get("enabled") and row.get("next_run_at")]
        return min(times, default=None)

    # -- the end of the process ---------------------------------------------------

    def drain(self, timeout_s: float) -> int:
        """Park the running turns for the next worker. Return their number."""
        try:
            body = {"timeout_ms": int(timeout_s * 1000), "grace_ms": DRAIN_GRACE_S * 1000}
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
