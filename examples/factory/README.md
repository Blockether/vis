# Factory: a minimal durable software factory

Factory is one small Python process next to one Vis gateway. It gives a team:

- **Workflows.** A workflow is a Python function. One run is one Vis session, and the
  transcript of the session is the record of the run. A run continues after a restart.
- **Workflow code with versions.** The database keeps the code of each workflow and each
  version of it. The code changes in a Vis project of its own, not on a page.
- **Triggers in the code.** A workflow names its own triggers: a cron expression or
  an event. The code is the one source.
- **A dashboard.** The pages show the current runs and the past runs, the workflows, the
  suggestions and the memory.
- **Chats.** The same address serves Vis web. Each run and each talk is a session that
  you can open and continue.
- **Memory.** A team memory and a personal memory, which each session reads and writes
  with the `vis-optmem` extension.
- **Suggestions.** When a session becomes idle, a workflow reads it and suggests
  follow-up work. A person can discuss, accept or decline each suggestion.

All records and both memories are in one database: SQLite, PostgreSQL or Cloudflare D1.

## Architecture

```
browser ──▶ factory (python -m factory) ──▶ Vis gateway (loopback, with a token)
              │  /factory          the pages and the API
              │  /factory/vis      Vis web
              │  other paths       relayed to the gateway
              ▼
            one database: the workflow code, the runs, the events, the suggestions, the memories
```

The gateway has two projects. Each run and each chat works in the project **Factory**.
The project **Workflows** holds the code of the workflows.

- The factory is the only public address. The gateway listens on a loopback port.
- One secret, `FACTORY_TOKEN`, signs a person in. The run processes, the extension and
  the memory store of a session send the same secret as a bearer token.
- A caller never gets the gateway token. The factory adds it when it relays a request.
- Run only one factory process for each database. The start of the process recovers the
  runs of a stopped process.

## Workflows

The database is the store of the workflow code. Each workflow has numbered versions, and
a run or a trigger uses the newest version. A run keeps the source of its version.

To change the code, open Vis web, select the project **Workflows** and start a session.
The project has one file `<name>.py` for each workflow, and the function has the name of
the file. When no session of that project has a turn in progress, the factory reads the
files:

- A changed file or a new file that loads becomes the next version. The factory reads
  the declarations from the source, and a child process loads the source as a run does.
- A file that does not load makes no version. The Workflows page shows the error, and
  the runs continue with the last version. A session finds the error before that with
  the tool `factory.check`.
- A file that is not there comes back from the database. So an empty directory on a new
  machine gets the current code.

The Workflows page shows the code and does not change it. There a person selects a
version, restores an old version as a new version, starts a run or removes a workflow.
A removed workflow keeps its versions.

```python
from factory.models import Model
from factory.workflows import Context, Decision, Document, activity, cron, on, workflow


class Work(Model, sig="example.work@1"):
    summary: str
    tests_passed: bool


@activity(title="Write the plan")
def specify(ctx: Context, task: str) -> Document:
    return ctx.document("PLAN.md", f"Write a short plan for: {task}")


@activity(title="Approve the plan")
def approve(ctx: Context, plan: Document) -> Decision:
    return ctx.gate("plan", "Approve the plan", document=plan)


@activity(title="Make the change")
def make(ctx: Context) -> Work:
    return ctx.ask("Do the work of PLAN.md and run the tests.", Work)


@workflow(title="Nightly check", triggers=[cron("0 3 * * *"), on("webhook.check")])
def nightly(ctx: Context, task: str = "Find and fix one failing test") -> Work | None:
    plan = specify(ctx, task)
    if approve(ctx, plan).approved:
        return make(ctx)
```

| Call | Result |
| --- | --- |
| `ctx.ask(request)` | The answer of the session as text. |
| `ctx.ask(request, Work)` | A validated model. The session gets a correction request when its answer does not fit. |
| `ctx.document(name, request)` | A document that the session attached. |
| `ctx.delegate(tasks)` | The answer of one subagent for each task. |
| `ctx.gate(key, title)` | The decision of a person. The run has no process while it waits. |
| `ctx.note(text)` | A line in the log of the run. |

A run is durable because each request is one turn of its session. When the process of
a run starts again, a request that the transcript has returns its answer at once. Only
a new request goes to the session. A person can retry a failed run, and
`@activity(retries=2)` retries one activity in a fork of the session.

### The model of a workflow

The code of a workflow names its model: `@workflow(model="claude-haiku-5-5")`. Add
`provider="..."` when two providers of the gateway have a model with that name. Without
`provider`, the model uses the first provider of the gateway that has it. Without
`model`, a run uses the model of the gateway.

The code gives the default. On the Workflows page, the control **Model of the runs**
lists the models of the gateway, and a selection there replaces the model of the code
for the next runs. The selection stays when the code changes. The first row of the
control, **From the code**, removes the selection. A run keeps the model that it
started with, and a run in a session of a person keeps the model of that session.

One request can use another model: `ctx.ask(request, llm="qwen3.8-flash")`.

The factory refuses a run that has no value for an input without a default, and a
value that does not fit an annotation `str`, `int`, `float`, `bool`, `list` or `dict`.
It does not check other annotations.

### Triggers

| Trigger | Starts a run |
| --- | --- |
| `cron("0 3 * * *")` | At each UTC minute that matches. |
| `on("session.idle")` | For each event with that name. |

The fields of an event fill the inputs of the workflow that have the same name. A
trigger can also give fixed values: `on("webhook.check", input={"task": "..."})`. The factory reads the triggers from the source and does not run
the source for that. A trigger starts one run for each minute or event, also after a
restart or two scheduler looks at the same time.

| Event | When |
| --- | --- |
| `session.idle` | A session of the project has new turns and no new turn for `FACTORY_IDLE_S` seconds. Sessions of runs and talks do not count. |
| `run.completed`, `run.failed` | A run ended. A workflow does not start from its own run. |
| `suggestion.created`, `suggestion.accepted`, `suggestion.declined` | A suggestion changed. |
| `webhook.<name>` | `POST /factory/api/hooks/<name>` with the header `X-Factory-Token`. The JSON body is the field `body`. |

### The three workflows

| File | Trigger | Work |
| --- | --- | --- |
| `workflows/suggest.py` | `session.idle` | Reads the session, the declined suggestions in the team memory and the stored suggestions. Stores at most three new suggestions. |
| `workflows/implement.py` | `suggestion.accepted` | Writes `PLAN.md`, waits for a person at a gate, makes the change, runs the checks and reports in a comment. |
| `workflows/remember.py` | `suggestion.declined` | Saves one line about the decision in the team memory, so the factory does not suggest the same work again. |

They are examples, and a new database gets them as its first workflows. Change them in
the project Workflows, or remove them on the Workflows page.

## Suggestions

A suggestion has a title, a body, a kind, an effort and a status: open, accepted or
declined. On the Suggestions page a person can:

- **Discuss** it: the factory opens one talk session in Vis web. A talk does not change
  files.
- **Accept** it or **decline** it, with a comment. The decision is final and makes an
  event, which starts `implement` or `remember`.
- Add a comment.

## Memory

The factory keeps two memories, `personal` and `team`, in its database. The page Memory
shows their entries and searches them. A session uses them through the `memo` tools of
`vis-optmem`: the project file `vis.yml` sets the store of each memory to
`vis_optmem.remote:RemoteStore` with the address `<factory>/factory/api/memory`.

| `FACTORY_DATABASE` | Store |
| --- | --- |
| `sqlite:///<file>` | One SQLite file. The default. |
| `postgres://...` | Any PostgreSQL. Install the `postgres` extra. |
| `d1://<account>/<database>` | Cloudflare D1 through its HTTP API, with `CLOUDFLARE_API_TOKEN`. |

`factory/tables.py` is all the code that knows a database. Each store is one table,
`factory_items`, with conditional writes.

## Tools for a session

`extension/factory.py` is a Vis extension with the alias `factory`. Its tools take and
return the models of `factory/models.py`. Each model has a stable type ID and refuses
unknown fields.

| Tool | Use |
| --- | --- |
| `factory.suggest(title, body, kind, effort)` | Store one suggestion. |
| `factory.suggestions(status=None)` | List the suggestions. |
| `factory.transcript(session, limit=10)` | Read short previews of the last turns of a session. |
| `factory.runs()` | List the recent runs. |
| `factory.start(workflow, input=None)` | Start a run. |
| `factory.check(name, source)` | Check the source of a workflow before the factory reads its file. |

## Install on one Linux machine

You need a Vis release (`vis-agent-native` and `vis-agent-python`), Git, systemd and,
for the build of Vis web, Node.js.

```sh
VIS_BIN=/opt/vis/bin FACTORY_HOST=0.0.0.0 FACTORY_PUBLIC_URL=https://factory.example.com deploy/install.sh
```

The script installs two systemd services, `factory-gateway` and `factory`, in
`/opt/factory` (`FACTORY_ROOT`). The head of the script lists each setting. A second
run with the same values changes nothing, and a later run needs only the values that
change. The token for the sign-in is in `<root>/secret.env`.

A run needs a language model. Open `/factory/vis`, open the settings and add a
provider. The header of each page shows `Gateway: no model` until the gateway has one.
The gateway of the factory has its own Vis home, `<root>/home/.vis`, so it does not use
the providers of another Vis on the machine. `FACTORY_PROVIDER` names the provider of
each model that a workflow names without a provider.

Put a proxy with TLS in front of the factory when people use it from other machines.

## Cloudflare

The database of a factory can be Cloudflare D1: set `FACTORY_DATABASE=d1://...`. The
factory process and the Vis gateway still need a machine or a container, because a
session works on the files of the project. This example has no Cloudflare deployment
script.

## Run it on your computer

```sh
uv sync
VIS_GATEWAY_URL=http://127.0.0.1:7890 FACTORY_HOME=/tmp/factory uv run python -m factory
```

Open `http://127.0.0.1:8090/factory`. Without `FACTORY_TOKEN`, the factory listens only
on a loopback address and has no sign-in.

```sh
uv run pytest
uv run ruff check factory tests workflows extension
```

The PostgreSQL tests run when `FACTORY_TEST_POSTGRES` has the address of a database.

## Files

| Path | Content |
| --- | --- |
| `factory/settings.py` | Each setting. The one module that reads the environment. |
| `factory/tables.py` | The SQLite, PostgreSQL and D1 stores. |
| `factory/records.py`, `runs.py`, `triggers.py`, `suggestions.py`, `memory.py` | The records and their rules. |
| `factory/api.py`, `server.py`, `gateway.py` | The API, the HTTP server with the relay, and the calls to the gateway. |
| `factory/workflows/` | The workflow SDK, the store of the versions and the process of one run. |
| `factory/models.py` | The models of the tools and the workflows. |
| `extension/factory.py` | The Vis extension. |
| `workflows/` | The three first workflows of a new database, and the guide of the project Workflows. |
| `web/` | The pages: plain HTML, CSS and ES modules. |
| `deploy/install.sh` | The installer. |
