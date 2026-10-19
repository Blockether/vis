# Logs and diagnostics

Use this page to debug Vis: find startup errors, command output and hang reports.
Vis writes log files and other diagnostics under `~/.vis/logs/YYYY-MM-DD/` on the
machine running the process.
The date is UTC. A process or command keeps the directory chosen when it starts.
A hang report uses its capture date.

## When to use

- **Vis or the gateway crashes or does not start.** [Find the
  log](#find-the-right-file) for that day and process.
- **A command or extension printed output that you did not see.** Check the [log
  files](#find-the-right-file) and the [Python worker log](#python-runtime).
- **A task stopped making progress, or the whole gateway stopped answering.** Read
  the [hang reports](#hang-reports) and the [evidence collected from outside the
  gateway](#when-the-whole-jvm-stops-answering).
- **A turn failed with a provider error, and you need the cause.** [Find a provider
  error](#find-a-provider-error) by its session, turn or request ID.
- **You are about to attach logs to a bug report.** [Review them
  first](#review-before-sharing). Logs can contain anything a command printed.
- **You want to know how long logs are kept.** See [Rotation and
  retention](#rotation-and-retention).

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
| Warnings and errors of the gateway, TUI or CLI process | `<role>-<UTC-start>-pid<PID>.problems.jsonl` | JSON lines |
| Gateway launcher output, including startup failures | `gateway-boot-<database-hash>-<time>-<random>.log` | Text |
| A command's stdout and stderr | `shell/<session>/<id>.log` | Text |
| Python worker startup output and errors | `pyext-<worker-id>/worker.log` | Text |
| Python JVM worker fatal-error report | `pyext-<worker-id>/jvm-crash-<PID>.log` | Text |
| Python JVM worker heap dump, when enabled | `pyext-<worker-id>/jvm-heap.hprof` | JVM heap dump |
| Managed Clojure nREPL output | `vis-nrepl-<project>-<id>.log` | Text |
| Standalone Python SDK shell output | `outside/shell-<id>.log` | Text |
| Gateway hang evidence | `gateway-hang-<id>/report.json` | JSON |
| JVM performance recording | `vis-<client-or-gateway>-<PID>-<UTC-start>.jfr` | Java Flight Recorder |

Shell results expose their exact `log_path`. It stays the same after midnight or
a gateway restart. Hang warnings also include the report path. Ordinary process
logs use a start timestamp such as `20260914T153000Z` to distinguish runs.

## Find a provider error

A provider error is a failure of the model call: a rate limit, a rejected credential, an
HTTP error or a closed stream. Vis records each warning and error of one turn in three
places. Look in this order:

1. The error card in the chat names the provider, the HTTP status and the request ID.
2. The problem log keeps every warning and error of the process as JSON lines.
3. The session database keeps the same signals in its `log` table.

### Problem log

Each process writes `<role>-<UTC-start>-pid<PID>.problems.jsonl` next to its text log.
`<role>` is `gateway`, `tui` or `vis`. Each line is one JSON object, for example:

```json
{"ts":"2026-10-12T03:28:07.499Z","level":"warn","id":"com.blockether.svar.internal.llm/retry-declined","msg":"not retrying provider failure","session_id":"36c4556d-f07e-4a40-851a-62dc18cc8caf","turn_id":"7e28b0fb-3349-405c-9d98-4cace35baed7","turn":3,"iteration":1,"query_id":"fcbaf266-5119-45a4-b7e7-8505e38886fe","request_id":"req_42","status":429,"category":"quota-exhausted","error":"Exceptional status code: 429"}
```

The main fields are:

| Field | Meaning |
| --- | --- |
| `level` | `warn`, `error` or `fatal`. |
| `id` | The signal name, for example `retry-declined`, `auth-provider-fallback` or `credential-refresh-failed`. |
| `session_id`, `turn_id`, `turn` | The session and the turn that made the call. |
| `iteration`, `query_id` | The model call inside the turn. Vis gives each call one query ID. |
| `request_id` | The ID that the provider gave the HTTP request. Give it to the provider's support. |
| `status`, `category`, `error` | The HTTP status, the failure class and the error message. |
| `data` | Other details of the signal. |

To list the problems of one session, run this command:

```bash
grep -h '"session_id":"<session-id>"' ~/.vis/logs/*/*.problems.jsonl
```

The error card in the TUI and in the Companion app shows a `Log id`. It is the
`query_id` of the failed call. To find its lines, run this command:

```bash
grep -h '"query_id":"<log-id>"' ~/.vis/logs/*/*.problems.jsonl
```
Vis redacts labelled credentials, such as `api_key` or `Authorization` values, and limits
each text value to 2,000 characters. Review the lines before you share them.

### Session database

The `log` table in `~/.vis/vis.mdb/vis.db` keeps the `info`, `warn` and `error` signals
of each session. Its `data` column holds the same message and details as JSON. Open the
database read-only, so that a running gateway is not changed:

```bash
sqlite3 -readonly ~/.vis/vis.mdb/vis.db \
  "select datetime(created_at/1000,'unixepoch'), level, event, data from log
   where session_soul_id = '<session-id>' and level in ('warn','error')
   order by created_at"
```

`session_turn_soul_id` and `session_turn_iteration_id` filter one turn or one step.

### From inside a session

An agent reads the same evidence with `read_session()`. The `failures` and `diagnosis`
fields summarize the failed steps of a session. For a past turn, search the problem log
by `turn_id` or `query_id`.

## Python runtime

Session sandboxes and trusted Python extensions run in separate worker processes. Each worker
writes its output to `pyext-<worker-id>/worker.log` while it runs. Python `print()` output comes
back with the execution result. Extension `vis.log` messages go to the gateway log.

A source build without a packaged Python worker runs the worker on the JVM. That worker writes
fatal-error reports beside `worker.log`, and heap dumps if you enable them. Operating-system crash
reports follow the operating system's own policy.

## Hang reports

A gateway watchdog saves a report before cancelling a stalled turn or abandoning
a cancellation that did not finish. This includes turns waiting for a model.
Collection waits at most 500 ms and skips a new capture if one is already running.

`report.json` contains session and turn IDs, the observed phase or cancellation
reason, and up to 256 platform-thread stacks with 64 frames each. The stalled
turn's thread is first. This in-process snapshot does not enumerate JVM virtual
threads.

### When the whole JVM stops answering

A running local Vis CLI client can collect evidence from outside the gateway. First, the client must
get a successful authenticated health check. If a later check fails, the client can run `jcmd` to
collect platform and virtual threads. This needs two conditions: the same JVM process is still
alive, and `jcmd` is next to its Java executable.

The client writes `threads.json`, `attach.log` and `report.json` in a `gateway-hang-<id>/` directory
under **its own** dated log directory. Each client tries at most once per minute, and the helper has
a five-second deadline. Collection does not signal, stop or restart the gateway.

External collection needs a local JVM gateway and a client that continues to check the connection.
It does not attach to native gateways or remote targets. `report.json` records failed and timed-out
tries. If the JVM has already accepted a dump, stopping the helper does not cancel it.

## Rotation and retention

- When the engine starts, and every hour while it runs, Vis removes diagnostic files last changed
  more than 14 days ago. Then it removes empty directories. This covers the full log tree, including
  shell and Python worker output, rotated logs, hang reports, JFR recordings and heap dumps. A
  running process continues to write to its original date directory. Cleanup uses file age, not
  folder date.
- Ordinary process logs rotate monthly or at 4,000,000 bytes. Rotated parts use
  gzip. The handler limits them to eight parts per interval and six intervals.
  The 14-day age policy also applies to these files.
- Hang-report cleanup keeps the ten newest completed reports across all dates.
  It runs after a report completes. Incomplete captures remain subject to the
  14-day age policy.
- Starting a JFR recording keeps the six newest existing recordings across dates.
  The new recording may add a seventh file. Enable recording with `VIS_JFR=1` or
  `--jfr`. It dumps on exit, with a 128 MiB client or 256 MiB gateway limit.
- Deleting a session also deletes its shell logs across dates.

The standalone Python SDK cleans its own shell logs after the first shell
command starts and every hour while that Python process runs. It uses the same
14-day cutoff. By default these logs share `~/.vis/logs/`. Setting
`VIS_OUTSIDE_HOME` moves them to
`<VIS_OUTSIDE_HOME>/logs/YYYY-MM-DD/outside/`, with the same automatic cleanup.

Cleanup runs in the background and needs a running process. A very short-lived
CLI or Python process may exit before its first pass finishes. If both are
stopped, expired logs remain until a later run. Save diagnostic files you need
longer outside the log tree before they expire.

## Review before sharing

Read the relevant files and share only the lines needed to show the problem.
Command output and ordinary logs can contain anything the command or extension
prints, including credentials, private code and local paths. JVM fatal-error
reports can include environment and memory details. Heap dumps contain process
memory. Do not share them without a separate privacy review.

Hang reports omit prompts, tool arguments, HTTP bodies and credentials from
their metadata. Thread names and stacks can still expose paths and application
details. On POSIX filesystems, hang-report directories use mode `700` and files
use `600`. Other filesystems use the account's inherited access controls. These
permissions apply to hang reports, not every file in the log tree.

## Session state is separate

`~/.vis/gateway/events/<session-id>.ndjson` is the durable gateway event journal
used to replay a session. It contains conversation and tool data, is not a
diagnostic log, and is not covered by log cleanup. Configuration, credentials and
session databases also stay outside the log tree.

Use a reviewed [session export](sessions.md#export-a-session) when a bug needs transcript
context. Do not share raw event journals, databases or credential files.

## See also

- [Reporting a bug](reporting-bugs.md) — prepare a minimal reproduction and safe excerpts.
- [Running a gateway](gateway-service.md) — service setup, health checks and shutdown.
- [Extension API](extension-api.md#logging-and-notifications) — write log messages and notifications.
