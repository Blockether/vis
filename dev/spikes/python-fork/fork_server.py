"""Prototype: a warm CPython template that forks one child per session.

Usage:
    python3 fork_server.py --modules modules.json --path ROOT [ROOT ...]
                           [--children N] [--landlock] [--json]

Two ways to get a ready interpreter with the worker's imports are compared:
  spawn: a new CPython process imports them, as each new worker does now.
  fork:  one template imports them once, runs gc.freeze() and forks a child.
For each it reports the time to the first answer and the memory per child:
idle, after a light block, and after a block that touches every module global.
Linux only. With --landlock, each child applies a Landlock ruleset after the
fork (read everywhere, write only in its own directory) and proves it.
"""

import argparse
import ctypes
import gc
import importlib
import json
import os
import random
import statistics
import subprocess
import sys
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


def import_all(names):
    """Import `names`; answer the names that failed."""
    failed = []
    for name in names:
        try:
            importlib.import_module(name)
        except BaseException:
            failed.append(name)
    return failed


def landlock(write_dir):
    """Confine this process: read and run everywhere, write only in `write_dir`."""
    libc = ctypes.CDLL(None, use_errno=True)
    libc.syscall.restype = ctypes.c_long
    abi = libc.syscall(444, None, ctypes.c_size_t(0), ctypes.c_uint32(1))
    if abi < 0:
        return f"unsupported (errno {ctypes.get_errno()})"
    handled = (1 << {1: 13, 2: 14, 3: 15, 4: 15}.get(abi, 16)) - 1

    class Ruleset(ctypes.Structure):
        _fields_ = [("handled_access_fs", ctypes.c_uint64)]

    class PathBeneath(ctypes.Structure):
        _layout_ = "ms"  # the kernel struct is packed: 12 bytes
        _pack_ = 1
        _fields_ = [("allowed_access", ctypes.c_uint64), ("parent_fd", ctypes.c_int32)]

    ruleset = libc.syscall(
        444,
        ctypes.byref(Ruleset(handled)),
        ctypes.c_size_t(ctypes.sizeof(Ruleset)),
        ctypes.c_uint32(0),
    )
    read = 0b1101  # EXECUTE | READ_FILE | READ_DIR
    for path, rights in (("/", read), (write_dir, handled)):
        fd = os.open(path, os.O_PATH)
        if libc.syscall(445, ruleset, 1, ctypes.byref(PathBeneath(rights, fd)), 0) < 0:
            return f"add_rule failed (errno {ctypes.get_errno()})"
        os.close(fd)
    if libc.prctl(38, 1, 0, 0, 0) != 0 or libc.syscall(446, ruleset, 0) < 0:
        return f"restrict_self failed (errno {ctypes.get_errno()})"
    os.close(ruleset)
    try:
        open(
            os.path.join(os.path.expanduser("~"), ".vis-fork-spike-probe"), "w"
        ).close()
        return f"abi {abi}: NOT ENFORCED, a write outside succeeded"
    except PermissionError:
        pass
    with open(os.path.join(write_dir, "probe"), "w") as f:
        f.write("ok")
    return f"abi {abi}: enforced"


BLOCK = "\n".join(
    f"def f{i}(x):\n    return [x * {i} for _ in range(3)]" for i in range(200)
)


def light():
    """A small block: one JSON round trip and one compile."""
    data = [{"i": i, "s": str(i) * 5} for i in range(20000)]
    json.loads(json.dumps(data))
    compile(BLOCK, "<block>", "exec")


def touch():
    """A worst case: read every global of every module, which writes refcounts."""
    for module in list(sys.modules.values()):
        for value in list(getattr(module, "__dict__", {}).values()):
            type(value)


def serve(read, write, jail_dir):
    """The child loop: answer ready, then run the commands of the parent."""
    started = time.perf_counter()
    jail = landlock(jail_dir) if jail_dir else "off"
    jail_ms = (time.perf_counter() - started) * 1000
    write.write(
        json.dumps({"answer": eval("1 + 1"), "jail": jail, "jail_ms": jail_ms}) + "\n"
    )
    write.flush()
    for line in read:
        command = line.strip()
        if command == "exit":
            break
        {"light": light, "touch": touch}[command]()
        write.write("done\n")
        write.flush()


class Child:
    def __init__(self, pid, read, write, started):
        self.pid, self.read, self.write = pid, read, write
        hello = json.loads(read.readline())
        self.ready_ms = (time.perf_counter() - started) * 1000
        self.jail, self.jail_ms = hello["jail"], hello["jail_ms"]

    def run(self, command):
        self.write.write(command + "\n")
        self.write.flush()
        if command != "exit":
            assert self.read.readline().strip() == "done"


def jail_dir_for(args):
    return tempfile.mkdtemp(prefix="vis-fork-child-") if args.landlock else None


def spawn_child(args):
    started = time.perf_counter()
    command = [
        sys.executable,
        os.path.abspath(__file__),
        "--child",
        "--modules",
        args.modules,
        "--path",
        *args.path,
    ]
    jail = jail_dir_for(args)
    if jail:
        command += ["--jail-dir", jail]
    process = subprocess.Popen(
        command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
    )
    return Child(process.pid, process.stdout, process.stdin, started)


def fork_child(args):
    to_child, from_parent = os.pipe()
    to_parent, from_child = os.pipe()
    jail = jail_dir_for(args)
    started = time.perf_counter()
    pid = os.fork()
    if pid == 0:
        os.close(from_parent)
        os.close(to_parent)
        random.seed()
        try:
            serve(os.fdopen(to_child), os.fdopen(from_child, "w"), jail)
        finally:
            os._exit(0)
    os.close(to_child)
    os.close(from_child)
    return Child(pid, os.fdopen(to_parent), os.fdopen(from_parent, "w"), started)


def measure(children, stage):
    return {stage: [smaps(child.pid) for child in children]}


def run_mode(args, make):
    children = [make(args) for _ in range(args.children)]
    memory = measure(children, "idle")
    for stage in ("light", "touch"):
        for child in children:
            child.run(stage)
        memory |= measure(children, stage)
    for child in children:
        child.run("exit")
    for child in children:
        try:
            os.waitpid(child.pid, 0)
        except ChildProcessError:
            pass
    return {
        "ready_ms": [c.ready_ms for c in children],
        "jail": children[0].jail,
        "jail_ms": statistics.median(c.jail_ms for c in children),
        "memory_kib": memory,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--modules", required=True)
    parser.add_argument("--path", nargs="+", required=True)
    parser.add_argument("--children", type=int, default=8)
    parser.add_argument("--landlock", action="store_true")
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--child", action="store_true")
    parser.add_argument("--jail-dir")
    args = parser.parse_args()
    sys.path[:0] = args.path
    names = json.load(open(args.modules))
    if args.child:
        import_all(names)
        serve(sys.stdin, sys.stdout, args.jail_dir)
        return
    spawn = run_mode(args, spawn_child)
    started = time.perf_counter()
    failed = import_all(names)
    gc.collect()
    gc.freeze()
    template_ms = (time.perf_counter() - started) * 1000
    template = smaps(os.getpid())
    forked = run_mode(args, fork_child)
    result = {
        "python": sys.version.split()[0],
        "modules": len(names),
        "failed": failed,
        "template_ms": template_ms,
        "template_kib": template,
        "spawn": spawn,
        "fork": forked,
    }
    if args.json:
        print(json.dumps(result))
        return
    print(
        f"python {result['python']}: {len(names)} modules, {len(failed)} failed "
        f"{failed[:8]}; template ready in {template_ms:.0f} ms, "
        f"Pss={template['Pss'] / 1024:.1f}M"
    )
    for mode in ("spawn", "fork"):
        r = result[mode]
        print(
            f"{mode}: ready median {statistics.median(r['ready_ms']):.2f} ms, "
            f"jail {r['jail']} ({r['jail_ms']:.2f} ms)"
        )
        for stage, rows in r["memory_kib"].items():
            print(
                f"  {stage:6} per child "
                + " ".join(f"{k}={median_of(rows, k) / 1024:.2f}M" for k in FIELDS)
            )


if __name__ == "__main__":
    main()
