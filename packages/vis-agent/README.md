# vis-agent

The Python half of [Vis](https://github.com/Blockether/vis): the module a Vis
extension imports, packaged so it also runs where Vis is not.

Public imports are `blockether.vis`, `blockether.vis.client`,
`blockether.vis.local`, `blockether.vis.views`, `blockether.vis.activity` and
`blockether.vis_contract`. The two distributions share an implicit PEP 420
`blockether` namespace; neither owns `blockether/__init__.py`.
They do not install or alias the unrelated top-level `vis` package.

SDK records, including `Response`, `Event`, `Session`, `Turn`, View and Activity
projections, are frozen, slotted dataclasses. Event envelope fields (`type`,
`session_id`, `seq`, `cursor`, `turn_id`) are named and validated on both transports.
`Event.view` and `Event.activity` expose validated records with immutable nested
collections. `Event.data` holds the remaining dynamic payload and remains mutable;
frozen records do not promise deep immutability for arbitrary endpoint data.
Extension tools can return frozen/slotted dataclasses; the runtime serializes
their declared fields, including nested records, without requiring `__dict__`.
Both packages ship PEP 561 `py.typed` metadata for their inline annotations.
The remaining endpoint dictionaries are not yet a fully typed model API.

```bash
pip install vis-agent
```

```python
from blockether import vis


def deploy(env):
    """Ship the current build to one environment."""
    spec = [
        vis.heading("Target"),
        vis.select("env", ["staging", "prod"], label="Where", default=env),
        vis.password("token", label="Deploy token", is_required=True),
    ]
    answer = vis.ask("Deploy", spec)
    if not answer:
        return vis.err("cancelled", answer.reason)
    run = vis.shell({"command": "./deploy.sh " + answer["env"]}).wait(600)
    vis.state["last_env"] = answer["env"]
    return vis.ok("deployed " + answer["env"], run["out"])


vis.extension(
    name="deployer",
    description="Ship a build from the session that decided to ship it.",
    alias="dep",
    symbols=[
        vis.symbol(
            deploy,
            tag="mutation",
            activity=vis.Activity(presenter="shell", label="Deploy"),
        )
    ],
)
```

## One file, two hosts

Host operations are declared in
[`python-host.json`](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/python-host.json)
and installed with the other canonical documents in `vis-contract`.

Inside a Vis session the engine seeds those ops and they reach the live agent:
state is the extension's durable state, `vis.ask` opens a dialog on whichever
surface the human is using, `vis.shell` runs in the agent's sandbox.

Installed in an ordinary Python environment there is no agent, so `vis._outside`
serves the same ops the contract says each behaves out here:

| op | outside |
| --- | --- |
| `state`, `log`, `notify`, `shell`, secrets, `host_env` | done locally — a JSON file under `~/.vis/outside`, stderr lines, a real subprocess, a process-local vault |
| `ask` | prompted in the TERMINAL: the same field tree, the same validators, the same `Answer` |
| `jailed_shell`, `jailed_shell_session` | refused by name — a jail is a property of the agent's process boundary, and nothing out here can enforce one |

So an extension file imports, type-checks, unit-tests and runs on a laptop or in
CI, and the code that ships is the code that was tested.

## Answering without a human

```python
from blockether import vis

vis.outside.answer_with({"env": "staging", "token": "hunter2"})
answer = vis.ask("Deploy", [vis.select("env", ["staging", "prod"])])
assert answer["env"] == "staging"
```

`VIS_OUTSIDE_ANSWERS` (a JSON object) primes the same values from the
environment, and `VIS_OUTSIDE_NONINTERACTIVE=1` makes an unanswerable ask return
`undeliverable` — exactly what a session with no surface mounted returns — instead
of blocking a build.

Other environment knobs: `VIS_OUTSIDE_HOME` moves the state file and the shell
logs (default `~/.vis/outside`).

## Testing live extensions

`vis.testing.LiveRecorder` is the shared in-memory host for extension tests. It
records extension envelopes without publishing fixture views into a real session,
materializes open/patch/state/close, and exposes `focus` and `close` for simulated
surface actions. Provider-specific tests keep only their provider snapshots and
assertions; `vis.testing.assert_tree` compares terminal view goldens at the exact
leaf that changed.

## View and Activity are different contracts

**View** is a human interaction: an input form or a live surface with the existing
`view.open`, `view.patch` and `view.close` lifecycle. Extension authors keep using
`vis.ask` and the live builders. Remote and local clients use the same typed
`InputView`, `LiveView`, `LivePatch`, `InputResult` and `LiveResult` records from
`blockether.vis.views`. `session.input_views()` and `session.live_views()` return
records, not unvalidated dictionaries. `session.answer(view_id, values)` submits a
form; `session.view_action(view_id, action, **values)` validates the closed action
shape before transport IO. Malformed incoming views raise `ProtocolError`.
Input projections carry the engine's `created_at`, not channel routing or validator
callbacks. An `InputResult` contains only the close reason: submitted values and
secret handles go to the waiting extension, not the event stream. SSE omits the
already streamed live picture, so `LiveResult.view` is optional; retain the open
View and apply its patches if your application needs a materialized display.

**Activity** is engine-observed execution evidence, not an interactive View and not
model context. An extension can set `vis.Activity(presenter=..., label=...)` on a
symbol or `vis.method`; it cannot forge progress, identity, timing or outcomes.
The tool's observation/mutation tag still determines its effect classification.
Omitting Activity metadata preserves the engine's default presentation.

```python
# The same records are returned by GatewayClient and LocalEngine.
with session.events() as events:
    for event in events:
        if event.activity is not None:
            print(event.activity.state, event.activity.counts)
        if event.view is not None:
            print(event.type, event.view.kind, event.view.view_id)
```

`blockether.vis.activity.ActivityProjection` represents the complete replacement
in a `block.activity` event; it is not a delta to merge. Rows, counts, omission
accounting, evidence and resources are named records. `from_wire` validates the
canonical schema and semantic bounds; `to_wire` produces plain JSON-compatible
data. View records provide the same conversion boundary.

The normative specifications are the packaged
[View contract](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/view.json),
[View schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/view.json),
[Activity contract](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/activity.json)
and [Activity schema](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/schema/activity.json).
SDK, engine, TUI and Companion test against fixtures owned by that contract package,
not separately maintained examples. Retired Activity envelopes are rejected; there
is no compatibility renderer or alternate import alias.

## Remote gateway client

```python
import os
from blockether.vis.client import GatewayClient

with GatewayClient(
    "https://gateway.example.com", token=os.environ["VIS_TOKEN"]
) as client:
    session = client.create_session(title="Python API")
    turn = session.send("Describe this project")
    result = turn.wait(timeout=120)
```

The client uses an explicit origin; it does not discover credentials, download Vis,
start a gateway or stop a user's server. Closing releases its client lease, not
sessions. Idle clients renew their lease every 30 seconds using the canonical
bounded keepalive request. A failed renewal invalidates the client; close it and
create a new instance explicitly. Mutations are never replayed to regain a lease.
This client is not an embedded/local engine.

- `create_session`, `list_sessions` (one cursor page), `session(id)`.
- Session: `read`, `update`, `delete`, `send`, `input_views`, `live_views`,
  `answer`, `view_action`, `events`, `turns`, `artifacts`, `transcript`, `upload`,
  `download_attachment`.
- Turn: `read`, `wait`, `cancel`. Failed/cancelled turns are returned with their
  status; a wait timeout does not cancel the operation.
- Dedicated methods cover non-streaming SDK operations: for example `get_models()`,
  `get_provider_status(provider_id)`, `get_projects()` and `get_settings()`.
  There is no public `call`. Each method's docstring names its canonical operation;
  JSON operations are annotated `JSONValue`; binary/text operations return
  `Response(status, content, headers)`, and empty responses return `None`.
  Endpoint payloads without a canonical schema remain JSON rather than invented models.
  Session SSE uses `session.events()`. `speech_events(job_id)`, `voice_events(job_id)`
  and their `session_speech_events(sid, job_id)` / `session_voice_events(sid, job_id)`
  counterparts expose all four job streams as typed `JobEvent` snapshots.
  Job streams have no cursor: reconnect rereads state, identical snapshots are
  suppressed, and `is_done` terminates the iterator. Buffered methods refuse SSE.
- `events(cursor=0, reconnects=3, retry_delay=0.2)` is a context-managed SSE
  iterator returning named `Event` records with `type` and `seq`. Replay suppresses
  duplicates and honors a `subscription.ready` cursor reset after restart.
  Exhausting the reconnect budget raises `TransportError`.
- `GatewayError` exposes HTTP `status` and `code`; `ProtocolError` reports
  incompatible/malformed protocol data; `VisTimeout` is a `TransportError`.
  Mutations are never retried automatically. Keep the idempotency key if you
  explicitly retry a submission whose outcome is unknown.

Use one calling thread per client. Always close event iterators. Requests and
SSE idle reads use the client's timeout (30 seconds by default); turns have a
separate wait deadline. TLS verification stays enabled and redirects are refused.

## Owned local engine

```python
from blockether.vis.local import LocalEngine

with LocalEngine(executable="/path/to/vis-agent", root="/path/to/project") as engine:
    session = engine.create_session(title="Local Python API")
    print(session.read())
```

Supply a compatible Vis executable that implements `sdk-stdio`. The wheel does not
bundle or download that executable, and older binaries without this command cannot
be used. `LocalEngine` starts a private engine process, not an HTTP gateway and not
an in-process JVM. Linux and macOS are supported by this transport; Windows is not.
It reuses the remote client's dedicated methods, Session, Turn and Event types.
Local session and speech/voice job events poll the same canonical resources over
stdio instead of opening SSE connections. Polling has a bounded idle timeout and
validates an event before advancing its cursor. Closing a stream never cancels a job.
Use one calling thread per engine. Request timeouts close the owned process to
prevent a late response being mistaken for a later request. `startup_timeout`
defaults to 120 seconds; individual requests default to 30 seconds.

The database is temporary and discarded on close. Engine sessions are not durable
across context-manager exits. Project configuration is inherited; agent execution
still needs a configured provider. No gateway discovery or user-server shutdown is
performed.

The integration suite runs both transports against real Vis processes and a
loopback model double, covering extension execution, input/live View, Activity,
completion, cancellation and cleanup. Select it with `VIS_TEST_LOCAL_COMMAND`.
No real-model credentials are required. Release verification and any remaining
external gates are recorded in `PLAN.md`; unit tests do not prove a linked binary.

## Distribution and publishing

Both packages build a wheel from their sdist and are tested after installation
outside the checkout. CI covers CPython 3.11–3.14 and PyPy 3.11 on Linux/macOS;
the engine's embedded interpreter is independently owned by `vis-python-runtime`.
The pinned runtime v0.5.0 is published on GitHub with Linux/macOS x64/arm64 assets.
It is not a second Python SDK or a PyPI alias.

`.github/workflows/python-publish.yml` is an explicit, version-checked publishing
gate, dependent on distribution and real-engine tests. Publishing requires the
protected `pypi` environment and trusted publishers for **both** projects. Building
a wheel or adding this workflow does not publish either package to PyPI.

## Where the real documentation lives

`vis.ask`, the field builders, `vis.extension`, hooks, providers and network
filters are documented where they are defined, in `blockether/vis/__init__.py`, and in the
Vis docs (`doc("extending")` inside a session). Canonical JSON documents live in
`vis-contract`; this package implements their host and gateway contracts.

Apache-2.0. Part of the Vis repository: `packages/vis-agent`.
