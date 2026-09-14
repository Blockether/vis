# Logs and diagnostics

Use this page to find startup errors, command output and hang reports. Vis writes
diagnostics under `~/.vis/logs/YYYY-MM-DD/` on the machine running the process.
The date is UTC. A process or command keeps the directory chosen when it starts;
a hang report uses its capture date.

## Find the right file

Open `~/.vis/logs/` and choose the date when the process started or the hang
occurred. For a shared gateway, use the gateway service account's home directory
on the gateway machine. Client-side diagnostics use the client's account and
machine.

Paths below are relative to `~/.vis/logs/YYYY-MM-DD/`. Angle brackets mark values
that change between runs.

| What you need | Path | Format |
| --- | --- | --- |
| Gateway diagnostics and extension `vis.log` messages | `gateway-<UTC-start>-pid<PID>.log` | Text |
| Terminal UI diagnostics | `tui-<UTC-start>-pid<PID>.log` | Text |
| Short-lived CLI diagnostics | `vis-<UTC-start>-pid<PID>.log` | Text |
| Gateway launcher output, including startup failures | `gateway-boot-<database-hash>-<time>-<random>.log` | Text |
| A command's stdout and stderr | `shell/<session>/<id>.log` | Text |
| Python worker startup output and errors | `pyext-<worker-id>/worker.log` | Text |
| Python JVM worker fatal-error report | `pyext-<worker-id>/jvm-crash-<PID>.log` | Text |
| Python JVM worker heap dump, when enabled | `pyext-<worker-id>/jvm-heap.hprof` | JVM heap dump |
| Managed Clojure nREPL output | `vis-nrepl-<project>-<id>.log` | Text |
| Standalone Python SDK shell output | `outside/shell-<id>.log` | Text |
| Gateway hang evidence | `gateway-hang-<id>/report.json` | JSON |
| JVM performance recording | `vis-<client-or-gateway>-<PID>-<UTC-start>.jfr` | Java Flight Recorder |

Shell results expose their exact `log_path`; it stays the same after midnight or
a gateway restart. Hang warnings also include the report path. Ordinary process
logs use a start timestamp such as `20260914T153000Z` to distinguish runs.

## Python runtime

Session sandboxes and trusted Python extensions use separate worker processes.
Both write process output to `pyext-<worker-id>/worker.log`, including while the
worker is still running. Packaged native workers and the JVM fallback use the
same dated layout and keep their startup directory after midnight.

Normal Python `print()` output returns with the execution result. Extension
`vis.log` messages go to the gateway log. The embedded runtime does not choose
another log directory: Vis forwards its in-process JSONL diagnostic stream to
the process logger at debug level.

JVM fallback workers write fatal-error reports beside `worker.log`. If JVM heap
dumping is enabled, their heap dumps also go there; Vis does not enable heap
dumping itself. These JVM options are not passed to packaged native workers.
Operating-system crash reports follow the operating system's own reporting policy.

## Hang reports

A gateway watchdog saves a report before cancelling a stalled turn or abandoning
a cancellation that did not finish. This includes turns waiting for a model.
Collection waits at most 500 ms and skips a new capture if one is already running.

`report.json` contains session and turn IDs, the observed phase or cancellation
reason, and up to 256 platform-thread stacks with 64 frames each. The stalled
turn's thread is first. This in-process snapshot does not enumerate JVM virtual
threads.

### When the whole JVM stops answering

A running local Vis CLI client can collect evidence from outside the gateway.
After a successful authenticated health check, a later failed check can trigger
`jcmd` if the same JVM process is still alive and `jcmd` is beside its Java
executable. This collects platform and virtual threads.

The client writes `threads.json`, `attach.log` and `report.json` in a
`gateway-hang-<id>/` directory under **its own** dated log directory. Attempts are
limited to one per minute per client, with a five-second deadline for the helper.
Collection does not signal, stop or restart the gateway.

External collection requires a local JVM gateway and a client that continues
checking the connection. It does not attach to native gateways or remote targets.
Failed or timed-out attempts are recorded in `report.json`. Stopping the helper
cannot cancel a dump the JVM has already accepted.

## Rotation and retention

- At engine startup and every hour while it runs, Vis removes diagnostic files
  whose last modification was more than 14 days ago, then removes empty directories.
  This covers the entire log tree, including shell and Python worker output,
  rotated logs, hang reports, JFR recordings and heap dumps. A running process keeps
  writing to its original date directory; cleanup uses file age, not folder date.
- Ordinary process logs rotate monthly or at 4,000,000 bytes. Rotated parts use
  gzip; the handler limits them to eight parts per interval and six intervals.
  The 14-day age policy also applies to these files.
- Hang-report cleanup keeps the ten newest completed reports across all dates.
  It runs after a report completes. Incomplete captures remain subject to the
  14-day age policy.
- Starting a JFR recording keeps the six newest existing recordings across dates.
  The new recording may add a seventh file. Enable recording with `VIS_JFR=1` or
  `--jfr`; it dumps on exit, with a 128 MiB client or 256 MiB gateway limit.
- Deleting a session also deletes its shell logs across dates.

The standalone Python SDK cleans its own shell logs after the first shell
command starts and every hour while that Python process runs. It uses the same
14-day cutoff. By default these logs share `~/.vis/logs/`; setting
`VIS_OUTSIDE_HOME` moves them to
`<VIS_OUTSIDE_HOME>/logs/YYYY-MM-DD/outside/`, with the same automatic cleanup.

Cleanup runs in the background and needs a running process. A very short-lived
CLI or Python process may exit before its first pass finishes. If both are
stopped, expired logs remain until a later run. Save diagnostic files you need
longer outside the log tree before they expire.

Existing files are not moved into the dated layout. New writers use it; the
engine's age cleanup still removes old files beneath `~/.vis/logs/`.

## Review before sharing

Read the relevant files and share only the lines needed to show the problem.
Command output and ordinary logs can contain anything the command or extension
prints, including credentials, private code and local paths. JVM fatal-error
reports can include environment and memory details. Heap dumps contain process
memory; do not share them without a separate privacy review.

Hang reports omit prompts, tool arguments, HTTP bodies and credentials from
their metadata. Thread names and stacks can still expose paths and application
details. On POSIX filesystems, hang-report directories use mode `700` and files
use `600`; other filesystems use the account's inherited access controls. These
permissions apply to hang reports, not every file in the log tree.

## Session state is separate

`~/.vis/gateway/events/<session-id>.ndjson` is the durable gateway event journal
used to replay a session. It contains conversation and tool data, is not a
diagnostic log, and is not covered by log cleanup. Configuration, credentials and
session databases also stay outside the log tree.

Use a reviewed [session export](exporting-sessions.md) when a bug needs transcript
context. Do not share raw event journals, databases or credential files.

## See also

- [Reporting a bug](reporting-bugs.md) — prepare a minimal reproduction and safe excerpts.
- [Running a gateway](gateway-service.md) — service setup, health checks and shutdown.
- [Extension API](extension-api.md#logging-and-notifications) — write log messages and notifications.
