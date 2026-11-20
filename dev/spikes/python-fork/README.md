# Spike: fork session Python from a warm template

This spike measures one question: can Vis start a session Python worker by a fork
from a warm template, and does that save memory and start time? Rivet's
code-mode benchmark (`rivet-dev/code-mode-language-benchmark`, 2026-11-19) gives
CPython 3.39 MiB and 16.1 ms for a new process, and 404 KiB and 800 µs for a fork.

This is measurement code only. Nothing in Vis uses it.

## Files

- `drive_worker.py` starts one real `vis-python-worker` without the JVM. It uses the
  argv of `worker.clj` `child-argv` and the JSON-line protocol of `worker_peer.clj`.
  It times each start phase and reads `/proc/<pid>/smaps_rollup` after each phase.
  There is no jail and no host: a `host` call gets an error.
- `fork_server.py` compares two ways to get a ready CPython with the worker's imports:
  `spawn` (a new process imports them) and `fork` (one template imports them, runs
  `gc.freeze()` and forks). With `--landlock`, each child applies Landlock after the
  fork and proves that a write outside its directory fails.
- `modules.json` is the 170 modules of a fresh worker after `install-runtime`.

## Run

On a Linux host with Vis installed (set the paths of that host):

```sh
export VIS_PYTHON_NATIVE_PATH=$REL/libvispython.so VIS_PYTHON_PACKAGES=~/.vis/python/packages
$REL/python/bin/python3 drive_worker.py $REL/vis-python-worker --guest $GUEST --sources $SOURCES --runs 7
$REL/python/bin/python3 fork_server.py --modules modules.json --path $SOURCES $GUEST --children 8
```

`$REL` is the `bin/vis-agent-python` directory of a native release. Copy `$SOURCES` and
`$GUEST` from the `--resolved-sources` argv of a live worker. For the jail test:

```sh
podman run --rm -v $PWD:/s:ro python:3.14-slim \
  python /s/fork_server.py --modules /s/modules.json --path /tmp --children 8 --landlock
```

## Results (2026-11-20)

Hosts: A is Linux x86-64, an LXC container with 2 cores, without Landlock, and Vis
v0.5.24 with CPython 3.14.7. B is a Linux arm64 podman VM with kernel 7.1.3 and Landlock
ABI 9, with `python:3.14-slim` (the Vis modules are not there; the stdlib modules are).

### Start of a real worker (host A, median of 7, worker side only)

| Phase | Time |
| --- | ---: |
| spawn to connect | 2.4 ms |
| first request (CPython starts) | 29.2 ms |
| `install-runtime` | 49.7 ms |
| first `eval` | 0.9 ms |
| total | 83.1 ms |

The JVM side (policy, jail, tools, symbol sync) is not in these numbers.

### Memory of a fresh real worker (host A)

After `install-runtime`: Pss 35.1 MiB.

- 19.8 MiB private clean: file pages of `libpython3.14.so` (10.1 MiB) and
  `vis-python-worker` (9.6 MiB). The page cache shares these between workers.
- 15.0 MiB private dirty: the CPython heap (anonymous memory). Only this part
  costs memory for each worker, and a fork can share it.

Live long-running workers on the same host had 24 to 41 MiB private dirty.

### Spawn and fork, 8 children (median per child)

| | Host A spawn | Host A fork | Host B spawn + jail | Host B fork + jail |
| --- | ---: | ---: | ---: | ---: |
| ready (first answer) | 78 to 127 ms | 0.9 to 1.3 ms | 45 ms | 1.3 ms |
| Pss, idle | 14.9 MiB | 3.1 MiB | 15.8 MiB | 4.5 MiB |
| private dirty, idle | 13.8 MiB | 1.0 MiB | 14.5 MiB | 2.2 MiB |
| private dirty, light block | 18.9 MiB | 6.5 MiB | 19.5 MiB | 7.6 MiB |
| private dirty, every module global read | 18.9 MiB | 11.5 MiB | 19.5 MiB | 12.3 MiB |

"Light block" makes one JSON round trip of 20,000 records and one compile.
"Every module global read" is a worst case: refcount writes copy shared pages.
Landlock after the fork was enforced on host B and took 0.7 to 1.5 ms.

## Findings

1. A fork saves 7 to 13 MiB of private memory for each session, and the start time
   goes from about 80 ms to about 1 ms. With many sessions on one server, that is
   the largest gain. On a desktop, the warm spares already hide most start time.
2. Landlock works after the fork, so the template can stay out of the jail and each
   child can confine itself before it runs model code. Seatbelt (macOS) is not tested.
3. Blocker: `vis-python-worker` is a GraalVM native image. It has 5 threads when it
   is idle. A fork copies only the thread that calls it, so a fork of this process is
   not safe. A fork template must be a process that has one thread at the fork: a
   CPython launcher, or the native worker before it starts a thread.
4. Each child must make its own state again after the fork: the `random` seed, the
   control socket, the log file and the run directory. All children of one template
   have the same hash secret.

## Next step

The change belongs in `~/vis-python-runtime`, not in this repository: a single-thread
template process that forks session workers and keeps the current wire protocol.
