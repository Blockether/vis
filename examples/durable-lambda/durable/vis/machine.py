"""The state of Vis that each worker shares: the machine store, the one project and its session groups.

Each worker has its own Vis home. The worker of the shard `main` is the one writer of this state: the front sends it
each call that changes the machine, the project or a group, and it records the state in one S3 object. A worker of
another shard copies the object into its home before it answers a call. The settings of the project are a file in
the project directory, which each worker reads on EFS. Vis gives a project and a group a random id, so a worker keeps
the id that `main` gave next to its own id, and the relay changes one to the other in each call and in each answer.
"""

from __future__ import annotations

import json
import os
import re
import threading
import time
from typing import Any
from urllib.parse import urlencode

from durable import aws, shards
from durable.settings import GATEWAY_START_S, HOME, MACHINE, MAIN
from durable.vis.gateway import Gateway

# The calls that can change a group, its settings or its MCP servers.
CHANGES = re.compile(r"/v1/(settings|session-groups|mcp/servers)(/.*)?")
# The machine store of Vis, and the ids of `main` with the ids of this home.
STORE, IDS = os.path.join(HOME, ".vis", "state.yml"), os.path.join(HOME, ".vis", "durable-ids.json")
# What the row of an MCP server says besides its definition.
STATUS = ("name", "tools", "status", "source", "enabled", "error", "is_override", "is_managed", "is_connected")


def store() -> str:
    """Return the machine store of this home."""
    try:
        with open(STORE, encoding="utf-8") as file:
            return file.read()
    except FileNotFoundError:
        return ""


class Machine:
    """The shared state in the home of one worker."""

    def __init__(self, engine: Gateway, root: str, shard: str) -> None:
        """Start without a record. `engine` is the gateway of this home and `root` is the project directory."""
        self.engine, self.root, self.is_main = engine, root, shard == MAIN
        self._lock = threading.Lock()
        # In `main`: the project and the groups of the last record. In another shard: the groups of the last copy.
        self._project, self._groups = "", {}
        # In `main`: the state of the last record. In another shard: the version of the last copy.
        self._version: str | None = None

    def ask(self, method: str, path: str, body: Any = None) -> Any:
        """Call the gateway and return its answer. A refused call raises."""
        status, answer = self.engine.call(method, path, body)
        if status >= 300:
            raise RuntimeError(f"the gateway refused {method} {path}: {status}")
        return answer

    def own(self, group: str) -> dict[str, Any]:
        """Return what a group sets itself: the value of each setting, and each MCP server with its switch."""
        query = urlencode({"channel": "all", "scope": "group", "target_id": group})
        rows = [row for part in self.ask("GET", f"/v1/settings?{query}")["groups"] for row in part["toggles"]]
        values: dict[str, Any] = {}
        while rows:
            row = rows.pop()
            rows += row.get("children") or []
            if row.get("is_override"):
                values[row["id"]] = row.get("own_value")
        servers = self.ask("GET", f"/v1/mcp/servers?{query}")["servers"]
        return {"settings": values, "servers": {row["name"]: [{name: value for name, value in row.items() if name not in STATUS}, row.get("enabled", True)] for row in servers if row.get("source") == "group"}}

    def groups(self) -> dict[str, Any]:
        """Return the list of the groups of the project in this home, the archived groups also."""
        return self.engine.client().get_session_groups(query={"archived": "include", "root": self.root})

    def record(self, is_full: bool = True) -> None:
        """Record the state of `main` when it changed. `is_full` also reads the groups: at the start and after a call that can change them."""
        with self._lock:
            if is_full:
                listed = self.groups()
                self._project = listed["project_id"]
                self._groups = {row["id"]: {"name": row["name"], "color": row.get("color"), "is_archived": bool(row.get("archived_at")), **self.own(row["id"])} for row in listed["groups"]}
            state = json.dumps({"store": store(), "project": self._project, "groups": self._groups}, sort_keys=True)
            if state != self._version:
                aws.put(MACHINE, state.encode())
                self._version = state

    def follow(self) -> None:
        """Copy the record of `main` into this home when it changed.

        A stack that has no record gets it from the first worker of `main`, so this worker starts that worker and waits.
        """
        with self._lock:
            found, deadline = aws.head(MACHINE), time.time() + GATEWAY_START_S
            while not found:
                if time.time() > deadline:
                    raise RuntimeError("the worker of `main` did not record the state of the machine")
                shards.wake(MAIN)
                time.sleep(1)
                found = aws.head(MACHINE)
            if found["etag"] == self._version:
                return
            raw, _ = aws.get(MACHINE)
            state = json.loads(raw or b"{}")
            if state["store"] != store():
                # The gateway reads the file again at its next use. A rename gives it the complete file.
                with open(f"{STORE}.new", "w", encoding="utf-8") as file:
                    file.write(state["store"])
                os.replace(f"{STORE}.new", STORE)
            try:
                with open(IDS, encoding="utf-8") as file:
                    ids: dict[str, str] = json.load(file)
            except FileNotFoundError:
                ids = {}
            listed, is_complete = self.groups(), True
            mine = {row["id"]: row for row in listed["groups"]}
            # A group of this home that has no id of `main` takes the id of the group of `main` that has its name.
            free = {row["name"]: row["id"] for row in listed["groups"] if row["id"] not in ids.values()}
            for known in set(ids) - set(state["groups"]):
                # `main` deleted the group. Its sessions stay, as in `main`.
                gone = ids.pop(known)
                if gone in mine:
                    self.ask("DELETE", f"/v1/session-groups/{gone}")
            for known, group in state["groups"].items():
                local = ids.get(known)
                if local not in mine:
                    local = free.pop(group["name"], None) or self.ask("POST", "/v1/session-groups", {"name": group["name"], "root": self.root})["id"]
                row = mine.get(local) or {"name": group["name"]}
                fields = {name: group[name] for name in ("name", "color") if row.get(name) != group[name]}
                if fields:
                    self.ask("PATCH", f"/v1/session-groups/{local}", fields)
                if bool(row.get("archived_at")) != group["is_archived"]:
                    # The gateway refuses the archive of a group while one of its sessions works: the next call tries again.
                    is_complete = self.engine.call("PATCH", f"/v1/session-groups/{local}", {"archived": group["is_archived"]})[0] < 300 and is_complete
                if self._groups.get(known) != group:
                    self.settle(local, group)
                ids[known] = local
            with open(IDS, "w", encoding="utf-8") as file:
                json.dump(ids, file)
            self.engine.ids = [(known.encode(), local.encode()) for known, local in {**ids, state["project"]: listed["project_id"]}.items()]
            self._groups, self._version = state["groups"], found["etag"] if is_complete else None

    def settle(self, local: str, group: dict[str, Any]) -> None:
        """Give a group of this home the settings and the MCP servers that `main` recorded for it."""
        have, target = self.own(local), {"scope": "group", "target_id": local}
        for name in set(have["settings"]) - set(group["settings"]):
            self.ask("POST", "/v1/settings", {"id": name, "action": "inherit", **target})
        for name, value in group["settings"].items():
            if name not in have["settings"] or have["settings"][name] != value:
                self.ask("POST", "/v1/settings", {"id": name, "action": "value", "value": value, **target})
        for name in set(have["servers"]) - set(group["servers"]):
            self.ask("DELETE", f"/v1/mcp/servers/{name}?{urlencode(target)}")
        for name, (server, is_on) in group["servers"].items():
            if have["servers"].get(name) != [server, is_on]:
                self.ask("PUT", f"/v1/mcp/servers/{name}", {"server": server, **target})
                self.ask("POST", f"/v1/mcp/servers/{name}/actions/enable", {"enabled": is_on, **target})
