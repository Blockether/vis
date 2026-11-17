# Logs and diagnostics

To debug Vis, find its startup errors, command output, and hang reports here. Vis writes its log
files and other diagnostics under `~/.vis/logs/YYYY-MM-DD/` on the machine that runs the process.
The date is UTC. A process or a command keeps the directory of its start date. A hang report uses
the date of its capture.

## When to use

- **Vis or the gateway has a crash or does not start.** [Find the log](#find-the-right-file) of
  that process.
- **A command or an extension printed output that you did not see.** Check the
  [log files](#find-the-right-file) and the [Python worker log](#python-runtime).
- **A task made no more progress, or the whole gateway stopped answering.** Read the
  [hang reports](#hang-reports) and the
  [evidence from outside the gateway](#when-the-whole-jvm-stops-answering).
- **A turn failed with a provider error, and you need the cause.**
  [Find a provider error](#find-a-provider-error).
- **You want to attach logs to a bug report.** [Review them first](#review-before-sharing).
- **You know a session, turn, or request ID and need every line about it.**
  [Search the logs](#search-the-logs).
- **You want to know how long Vis keeps logs.** Read
  [Rotation and retention](#rotation-and-retention).

## Find the right file

Choose the date when the process started or the hang occurred. The logs of a shared gateway are in
the home directory of its service account, on the gateway machine. Client diagnostics are on the
client machine, in the account of the client.

The paths in the table are relative to `~/.vis/logs/YYYY-MM-DD/`. Angle brackets mark values that
change between runs. A process creates its log file at its first message, so a run that logs
nothing leaves no file. To use a different directory, start the JVM with `-Dvis.logs.dir=<path>`.
The test suites do so to keep their logs in `target/`.

| What you need | Path | Format |
| --- | --- | --- |
| Gateway diagnostics and extension `vis.log` messages | `gateway-<UTC-start>-pid<PID>.log` | Text |
| Terminal UI diagnostics | `tui-<UTC-start>-pid<PID>.log` | Text |
| Diagnostics of a short CLI run | `vis-<UTC-start>-pid<PID>.log` | Text |
| Warnings and errors of the gateway, TUI, or CLI process | `<role>-<UTC-start>-pid<PID>.problems.jsonl` | JSON lines |
| Gateway launcher output, startup failures included | `gateway-boot-<database-hash>-<time>-<random>.log` | Text |
| The stdout and stderr of a command | `shell/<session>/<id>.log` | Text |
| Python worker startup output and errors | `pyext-<worker-id>/worker.log` | Text |
| Fatal-error report of a Python JVM worker | `pyext-<worker-id>/jvm-crash-<PID>.log` | Text |
| Heap dump of a Python JVM worker, when enabled | `pyext-<worker-id>/jvm-heap.hprof` | JVM heap dump |
| Managed Clojure nREPL output | `vis-nrepl-<project>-<id>.log` | Text |
| Shell output of the standalone Python SDK | `outside/shell-<id>.log` | Text |
| Gateway hang evidence | `gateway-hang-<id>/report.json` | JSON |
| JVM performance recording | `vis-<client-or-gateway>-<PID>-<UTC-start>.jfr` | Java Flight Recorder |

A shell result gives its exact `log_path`. The path stays the same after midnight and after a
gateway restart. A hang warning gives the path of its report. The start timestamp in the name of a
process log, such as `20260914T153000Z`, identifies the run.

## Search the logs

A process log rotates each day and when it reaches its size limit. Vis compresses each rotated
part with gzip. The active file is `<name>.log`. The rotated parts are
`<name>.log-<YYYY-MM-DD>d.<N>.gz`, and part `1` is the newest.

Use `zgrep`, which reads plain and compressed files. `grep` finds no text in the `.gz` parts. Put
`*` after `.log` to include the rotated parts, and search all date directories.

To find every gateway log entry of one session:

```bash
zgrep -h -B 2 '<session-id>' ~/.vis/logs/*/gateway-*.log*
```

A text entry starts with a line that has the UTC time, the level, and the signal name. A `data:`
line and a `ctx:` line can follow it. The `ctx:` line holds the session and turn IDs, so `-B 2`
also prints the first two lines of the entry.

To read one compressed part, run `zless <file>.gz`.

## Find a provider error

A provider error is a failure of the model call: a rate limit, a rejected credential, an HTTP
error, or a closed stream. Look in two places, in this order:

1. The error card in the chat names the provider, the HTTP status, and the request ID.
2. The problem log keeps every warning and error of the process as JSON lines.

The text log of the process also keeps the `info` entries before and after the failure. To read
them, [search the logs](#search-the-logs) for the turn ID.

### Problem log

Each process writes `<role>-<UTC-start>-pid<PID>.problems.jsonl` next to its text log. `<role>` is
`gateway`, `tui`, or `vis`. The problem log rotates as the text log does, so search it with
`zgrep`. Each line is one JSON object, for example:

```json
{"ts":"2026-10-12T03:28:07.499Z","level":"warn","id":"com.blockether.svar.internal.llm/retry-declined","msg":"not retrying provider failure","session_id":"36c4556d-f07e-4a40-851a-62dc18cc8caf","turn_id":"7e28b0fb-3349-405c-9d98-4cace35baed7","turn":3,"iteration":1,"query_id":"fcbaf266-5119-45a4-b7e7-8505e38886fe","request_id":"req_42","status":429,"category":"quota-exhausted","error":"Exceptional status code: 429"}
```

| Field | Meaning |
| --- | --- |
| `level` | `warn`, `error`, or `fatal`. |
| `id` | The signal name, for example `retry-declined`, `auth-provider-fallback`, or `credential-refresh-failed`. |
| `session_id`, `turn_id`, `turn` | The session and the turn that made the call. |
| `iteration`, `query_id` | The model call in the turn. Vis gives each call one query ID. |
| `request_id` | The ID that the provider gave the HTTP request. Give it to the support of the provider. |
| `status`, `category`, `error` | The HTTP status, the failure class, and the error message. |
| `data` | Other details of the signal. |

To list the problems of one session:

```bash
zgrep -h '"session_id":"<session-id>"' ~/.vis/logs/*/*.problems.jsonl*
```

The error card in the TUI and in the Companion app shows a `Log id`, the `query_id` of the failed
call. To find its lines:

```bash
zgrep -h '"query_id":"<log-id>"' ~/.vis/logs/*/*.problems.jsonl*
```
Vis redacts labelled credentials, such as `api_key` or `Authorization` values, and limits each
text value to 2,000 characters.

### From inside a session

An agent reads the same evidence with `read_session()`. Its `failures` and `diagnosis` fields
summarize the failed steps of a session. For a past turn, search the problem log by `turn_id` or
`query_id`.

## Python runtime

Session sandboxes and trusted Python extensions run in separate worker processes. A session has
one sandbox worker and one worker for all its extensions. Each worker writes its output to
`pyext-<worker-id>/worker.log` while it runs. When a worker stops and its files are all empty, Vis
deletes its directory. Python `print()` output comes back with the execution result. Extension
`vis.log` messages go to the gateway log.

A source build without a packaged Python worker runs the worker on the JVM. That worker writes its
fatal-error reports next to `worker.log`, and heap dumps if you enable them. The operating system
keeps its own crash reports by its own policy.

## Hang reports

A gateway watchdog saves a report before it cancels a stalled turn, a turn that waits for a model
included. It also saves one before it abandons a cancellation that did not complete. Collection
waits at most 500 ms, and it skips a new capture while one runs.

`report.json` holds the session and turn IDs, and the observed phase or the cancellation reason.
It also holds up to 256 platform-thread stacks of 64 frames each, with the thread of the stalled
turn first. This snapshot comes from inside the process and does not list JVM virtual threads.

### When the whole JVM stops answering

A running local Vis CLI client can collect evidence from outside the gateway. The client must
first get one successful authenticated health check. If a later check fails, the client runs
`jcmd` to collect the platform threads and the virtual threads. For that, the same JVM process
must still be alive, and `jcmd` must be next to its Java executable.

The client writes `threads.json`, `attach.log`, and `report.json` to a `gateway-hang-<id>/`
directory under its own dated log directory, not that of the gateway. Each client tries at most
one time each minute, and the helper has a deadline of five seconds. Collection does not signal,
stop, or restart the gateway.

External collection needs a local JVM gateway and a client that keeps checking the connection. It
does not attach to a native gateway or a remote target. `report.json` records each try that failed
or timed out. When the JVM has accepted a dump, a stop of the helper does not cancel the dump.

## Rotation and retention

- **Age.** At engine start, and each hour while the engine runs, Vis removes diagnostic files that
  last changed more than 14 days ago. Then it removes empty directories. The cleanup covers the
  full log tree: shell and Python worker output, rotated logs, hang reports, JFR recordings, and
  heap dumps. It uses the age of a file, not the date of its folder.
- **Process logs and problem logs.** They rotate each day, and also at 8,000,000 bytes. The
  handler keeps eight gzip parts for each day, and 14 days.
- **Hang reports.** After a report completes, cleanup keeps the ten newest completed reports
  across all dates. An incomplete capture stays until the 14-day cleanup removes it.
- **JFR recordings.** The start of a recording keeps the six newest existing recordings across all
  dates, so the new recording can be a seventh file. Enable recording with `VIS_JFR=1` or `--jfr`.
  It dumps on exit, with a limit of 128 MiB for a client and 256 MiB for a gateway.
- **Shell logs.** When you delete a session, Vis also deletes its shell logs across all dates.

The standalone Python SDK cleans its own shell logs with the same 14-day limit. It does so after
the first shell command starts, and each hour while that Python process runs. By default, these
logs share `~/.vis/logs/`. `VIS_OUTSIDE_HOME` moves them to
`<VIS_OUTSIDE_HOME>/logs/YYYY-MM-DD/outside/`, with the same cleanup.

Cleanup needs a running process. A short CLI or Python process can exit before its first
background pass completes, and expired logs then stay until a later run. To keep a diagnostic file
for longer, copy it out of the log tree.

## Review before sharing

Share only the lines that show the problem.

| File | What it can hold |
| --- | --- |
| Command output and ordinary logs | Anything that the command or the extension prints: credentials, private code, and local paths |
| JVM fatal-error reports | Environment and memory details |
| Heap dumps | Process memory. Do not share one without a separate privacy review |
| Hang reports | Paths and application details in thread names and stacks. The metadata has no prompts, tool arguments, HTTP bodies, or credentials |

On POSIX filesystems, hang-report directories have mode `700` and their files have mode `600`.
Other filesystems use the access controls that the account inherits. These permissions apply to
hang reports only, not to each file in the log tree.

## Session state is separate

`~/.vis/gateway/events/<session-id>.ndjson` is the permanent event journal of the gateway. Vis
replays a session from it. The journal holds conversation and tool data, and log cleanup does not
remove it. Configuration, credentials, and session databases are
also outside the log tree.

When a bug report needs transcript context, use a reviewed
[session export](sessions.md#export-a-session). Do not share raw event journals, databases, or
credential files.

## See also

- To prepare a minimal reproduction and safe excerpts, read [Reporting a bug](reporting-bugs.md).
- For service setup, health checks, and shutdown, read [Running a gateway](gateway-service.md).
- To write log messages and notifications, read
  [Logging and notifications](extension-api.md#logging-and-notifications) in the Extension API.
