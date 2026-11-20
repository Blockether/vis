"""Drive one real vis-python-worker without the JVM and time each start phase.

Usage:
    python3 drive_worker.py WORKER --guest GUEST_DIR --sources ROOT [ROOT ...]
                            [--runs N] [--modules OUT.json] [--json]

The worker gets the same argv as worker.clj `child-argv`, but no jail and no
host: a `host` call from the child gets an error. Linux only, because memory
comes from /proc/<pid>/smaps_rollup.
"""

import argparse
import json
import os
import socket
import statistics
import subprocess
import tempfile
import time

FIELDS = (
    "Rss",
    "Pss",
    "Shared_Clean",
    "Shared_Dirty",
    "Private_Clean",
    "Private_Dirty",
)


def smaps(pid):
    """The memory split of `pid` in KiB, from /proc/<pid>/smaps_rollup."""
    out = {}
    with open(f"/proc/{pid}/smaps_rollup") as f:
        for line in f:
            key, _, rest = line.partition(":")
            if key in FIELDS:
                out[key] = int(rest.split()[0])
    return out


def median_of(rows, key):
    return statistics.median(r[key] for r in rows)


class Peer:
    """One JSON line per message, both ways, as in worker_peer.clj."""

    def __init__(self, conn):
        self.reader = conn.makefile("r", encoding="utf-8")
        self.writer = conn.makefile("w", encoding="utf-8")
        self.next_id = 0
        self.host_calls = 0

    def send(self, message):
        self.writer.write(json.dumps(message) + "\n")
        self.writer.flush()

    def request(self, message):
        self.next_id += 1
        mine = self.next_id
        self.send({**message, "id": mine})
        while True:
            line = self.reader.readline()
            if not line:
                raise RuntimeError(f"the worker closed during {message['op']}")
            reply = json.loads(line)
            if "op" in reply:
                self.host_calls += 1
                self.send({"id": reply["id"], "error": "the spike harness has no host"})
                continue
            if reply.get("id") == mine:
                if "error" in reply:
                    raise RuntimeError(f"{message['op']}: {reply['error']}")
                return reply.get("value")


def one_run(worker, guest, sources, modules_out):
    """Start one worker, time its phases and read its memory after each one."""
    work = tempfile.mkdtemp(prefix="vis-fork-spike-")
    path = os.path.join(work, "c.sock")
    server = socket.socket(socket.AF_UNIX)
    server.bind(path)
    server.listen(1)
    server.settimeout(60)
    log = open(os.path.join(work, "worker.log"), "w")
    home = os.path.expanduser("~")
    t0 = time.perf_counter()
    process = subprocess.Popen(
        [worker, f"-Duser.home={home}", path, "--resolved-sources", *sources, guest],
        stdout=log,
        stderr=subprocess.STDOUT,
        cwd=work,
    )
    try:
        conn, _ = server.accept()
        marks = {"connect": time.perf_counter()}
        memory = {"connect": smaps(process.pid)}
        peer = Peer(conn)
        roots = json.dumps([*sources, guest])
        peer.request(
            {
                "op": "exec",
                "session": "__main__",
                "code": f"import json, sys\nr = json.loads({roots!r})\n"
                "sys.path[:] = r + [p for p in sys.path if p not in r]\n",
            }
        )
        marks["paths"] = time.perf_counter()
        peer.request({"op": "install-runtime", "session": "spike"})
        marks["install_runtime"] = time.perf_counter()
        memory["install_runtime"] = smaps(process.pid)
        answer = peer.request({"op": "eval", "session": "spike", "code": "1 + 1"})
        marks["first_eval"] = time.perf_counter()
        memory["first_eval"] = smaps(process.pid)
        modules = peer.request(
            {"op": "eval", "session": "spike", "code": "len(__import__('sys').modules)"}
        )
        if modules_out:
            peer.request(
                {
                    "op": "exec",
                    "session": "spike",
                    "code": "import json, sys\n"
                    f"open({modules_out!r}, 'w').write(json.dumps(sorted(set(sys.modules) - {{'spike'}})))\n",
                }
            )
        threads = len(os.listdir(f"/proc/{process.pid}/task"))
        conn.close()
    finally:
        try:
            process.wait(10)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
        log.close()
    last, phases = t0, {}
    for name, mark in marks.items():
        phases[name] = (mark - last) * 1000
        last = mark
    phases["total"] = (last - t0) * 1000
    return {
        "phases_ms": phases,
        "memory_kib": memory,
        "answer": answer,
        "modules": modules,
        "threads": threads,
        "host_calls": peer.host_calls,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("worker")
    parser.add_argument("--guest", required=True)
    parser.add_argument("--sources", nargs="+", required=True)
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--modules")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    runs = [
        one_run(args.worker, args.guest, args.sources, args.modules if i == 0 else None)
        for i in range(args.runs)
    ]
    if args.json:
        print(json.dumps(runs))
        return
    print(
        f"runs={len(runs)} answer={runs[0]['answer']!r} modules={runs[0]['modules']} "
        f"threads={runs[0]['threads']} host_calls={runs[0]['host_calls']}"
    )
    for phase in runs[0]["phases_ms"]:
        print(
            f"  {phase:16} median {statistics.median(r['phases_ms'][phase] for r in runs):8.1f} ms"
        )
    for point in runs[0]["memory_kib"]:
        rows = [r["memory_kib"][point] for r in runs]
        print(
            f"  memory after {point:16} "
            + " ".join(f"{k}={median_of(rows, k) / 1024:.1f}M" for k in FIELDS)
        )


if __name__ == "__main__":
    main()
