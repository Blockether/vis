# One API for sessions and Council

Give extensions, the Python SDK and HTTP one session and Council interface over the existing gateway domain, with portable session snapshots (#405).

## Context

The previous plan, Evaluations on Harbor, stopped with phases 1 to 6 done and verified.
Phase 7 (trajectory judges) is open: a paid judge run, 30 human labels for each rubric and the action type of each tool call.
Phase 8 (ablations and the improvement gate) is not started.
Git history keeps that plan (last state at `8c380c91f`).

Issue #405 asks for managed-child control from trusted extension code.
Its comment adds independent sessions through `vis.run(...)` and a session file that you can restore and continue after a restart.

Current state:

- The domain is in `session/agents.clj` (`run`, `wait-agents`, `resume-agent`, `cancel-agent`, `extend-agent`, `deny-agent`, `grant-files`, `operation!`) and `gateway/state.clj` (`create-session!`, `submit-turn!`, `fork-session!`, `agents-operation!`, `council-operation!`).
- The model has `council.run/wait/resume/cancel/extend/deny/grant/ask*` and `council.publish/read/get/threads/members`.
- HTTP has `/v1/sessions/...`, `/v1/sessions/:sid/council/...` and only `GET`/`POST /agents`, `/agents/cancel` and `/agents/route` (`gateway/server/council.clj`).
- `packages/vis-contract/.../schema/agents.json` already defines `extend`, `deny`, `grant` and `resume`. No route or SDK method uses them.
- The SDK (`blockether.vis.engine`) has `Session`, `Turn`, `Agent.run` and `Council` with `publish_spawn`, `subagents`, `cancel`, `route` and `wake`.
- The extension SDK (`blockether.vis.extension`) has only `vis.council.wake`, `vis.read_session` and `vis.list_sessions`. `vis.tools.council.run(...)` works only inside a callback and only when the model has that tool.
- `LocalEngine` (stdio) uses a temporary database and deletes it on `close`. A session cannot survive that today.
- Nippy (`com.taoensso/nippy`) is already in `deps.edn`.

Problems:

- One operation has four names: `council.run`, `POST /agents`, `publish_spawn` and `"spawn"` in the supplied patch.
- Extensions have no typed session or Council API.
- No artifact can save a session tree and restore it as a working session.

Rejected alternatives:

- `vis.managed_agent(operation, options)` from the supplied patch: a string operation loses types and adds a fifth vocabulary.
- `vis.tools.council.*` as the public extension API: it depends on model tool selection and returns untyped records.
- A second Python implementation of the client for extensions: it duplicates validation and key conversion.
- Independent sessions in the model sandbox, also behind a toggle: the user forbids it. The model uses `council.run` for parallel work.
- Markdown or JSON transcript export as the session file: it does not restore continuation state.

Decisions:

- **Session** is an independent conversation. **Subagent** is a managed child that a parent owns through Council. **Turn** is one unit of work in a session.
- The model tool names are canonical. The SDK and extensions use the same names: `spawn`, `run`, `wait`, `resume`, `cancel`, `extend`, `deny`, `grant`, `route`, `subagents`.
- `Council.publish_spawn` becomes `Council.spawn`. Update all consumers and remove the old name.
- Extensions use the SDK classes `Session`, `Turn` and `Council` over a third execution layer, `HostLayer`. `HostLayer` calls the gateway route handlers in process. It never exports credentials.
- The model sandbox gets no independent-session API and no toggle for it.
- The session file is one recursive artifact: the root session and all its descendants.
- Python reserves the word `import`, so `x.import(...)` is a syntax error. The restore call is `restore`. The HTTP route keeps the word `import`.

## 1. One domain entry point and contract

Rationale: all transports must call one function, so semantics cannot drift.

Data:

- `agents/operation!` handles `:spawn`, `:list`, `:wait`, `:resume`, `:cancel`, `:extend`, `:deny`, `:grant` and `:route`.
- The model tools `council.*` call `operation!`, not the separate functions.
- `agents.json` gets `wait` (`session_ids`, `timeout_secs`) and `record` (the record fields that `council.run` returns today). All layers validate against these schemas.
- Spawn idempotency:
  - Without `key`, the host derives a key. It uses the parent session, the caller, the tool call id and the spawn ordinal in that call.
  - The caller is the model tool or the extension id.
  - With `key`, the scope is (parent session, caller, key). This scope lasts across turns and restarts. Store it durably.
  - The same key with different input fails with `idempotency-conflict`.

Acceptance criteria:

- Every `council.*` model tool gives the same records as before (existing managed-agent tests stay green).
- A repeated spawn with the same derived or explicit key returns the same child and creates no second session.
- A repeated explicit key after a gateway restart returns the same child.

Unknowns:

- Where to store explicit keys durably: a new table or a column on the agent row.

## 2. HTTP routes

Rationale: the SDK and `HostLayer` need one route for each operation.

Data:

- Add `POST /v1/sessions/:sid/agents/run`, `/wait`, `/resume`, `/extend`, `/deny` and `/grant` to the gateway contract route table and `gateway/server/council.clj`.
- `wait` is a long poll. `timeout_secs` is 1 to 1800, default 240, as for the model tool. On timeout it returns the current records with each `status` unchanged.
- `run` and `resume` need an active execution of the parent, like `spawn`.
- Map errors as `agents-handler` does today: `invalid-request` 400, `session-not-found` 404, others 409.

Acceptance criteria:

- Each new route validates its body against `agents.json` before any state change.
- Each route returns the same record shape as the model tool.

## 3. Model catalog and selection

Rationale: a caller must see which models it can use, with prices and limits, and then choose one.

Current state:

- `GET /v1/models` returns provider ids and model names only.
- svar already resolves model metadata from models.dev, provider overlays and the `tokens.pricing` setting. `provider/catalog.clj` uses the same rates for the cost estimate of a turn.
- `GET`/`PATCH /v1/sessions/:sid/model` read and pin the session model. `council.spawn` takes `provider`, `model` and `allowed_models`.

Data:

- `GET /v1/models` returns one object for each model in `catalog[].models`, not a name:
  - `provider`, `name`, `label`, `family`.
  - `pricing`: `input`, `output`, the optional `cache_read` and `cache_write`, and `tiers`, in USD for 1M tokens. A tier applies above `above_tokens` input tokens. `pricing_source` is `provider` (rates that the provider declares) or `catalog` (svar with the `tokens.pricing` setting).
  - `billing`: `metered` (API key), `subscription` (account sign-in) or `local` (no credentials). A subscription model shows its rates, but a turn costs nothing extra.
  - `context`, `input_limit`, `output_limit`.
  - `is_reasoning`, `reasoning_options`, `is_tool_call`, `is_image_input`, `modalities`.
  - `knowledge_cutoff`, `release_date`.
  - `is_available`: the provider is authenticated and serves the model.
  - `is_default`: the router default for new sessions.
- The schema is `$defs/model` in `packages/vis-contract/.../schema/provider.json`, next to the limits `report`. No client used the old name list, so it is removed.
- The listed rates are the rates of the cost estimate. Both read one function.
- An unknown price is `null`, never 0.
- Each model has `limits`: the report of its provider (quotas, use and resets) from `provider/limits.clj`. Models of one provider share one report.
- The catalog reads the provider reports in parallel through the existing limits cache and its time bound. A failed report is an error report, and the model stays listed.
- `GET /v1/providers/:provider-id/limits` stays for a fresh read of one provider.

SDK:

- `ExecutionLayer.models(*, provider=None, is_available=None) -> tuple[Model, ...]`. `Model` is a frozen dataclass from the schema.
- `ExecutionLayer.model_limits(provider) -> dict` reads the limits again without the catalog and validates the report schema.
- `create_session(provider=..., model=...)` pins the model at creation.
- `Session.model() -> SessionModel` (the pin and the default) and `Session.set_model(provider, model)`. `set_model(None, None)` clears the pin.
- `Council.spawn`, `Council.run` task dicts and `Council.route` take the same `provider` and `model`.

Extension:

- `vis.models.list(provider=None, is_available=None)` and `vis.models.get(provider, name)`. Each `Model` carries `limits`.
- `vis.models.limits(provider)` reads the limits again without the catalog.
- In an extension, each `Model` also has `is_allowed` for the bound session. An inherited `allowed_models` list or a human model lock sets it to false.
- `HostLayer` allows `GET /v1/models` and `GET /v1/providers/:provider-id/limits`. It refuses provider authentication, configuration and router changes.

Model sandbox:

- No change in this plan. The subagent prompt tells the model to choose a model only when the user or project names one.

Validation:

- An unknown provider fails with `unknown-provider` before the session or pin is written. A half pair fails with `invalid-request`. The model name stays free, as in `PATCH /v1/sessions/:sid/model`: the live provider catalog can offer more models than the picker.
- A model outside the bound session's `allowed_models` fails with `model-not-allowed`.
- A human model lock wins over `set_model` and `route`, as today.

Acceptance criteria:

- `models()` lists every model that the TUI and Companion pickers show, with the same names.
- For a metered model, the listed rates give the cost of a finished turn from its token counts, within rounding to 1e-6 USD.
- A session created with `provider` and `model` runs its first turn on that pair, as the turn usage shows.

Unknowns:

- The source of `tiers` for providers with long-context prices, for example `input-over-272k` in svar.

## 4. SDK

Rationale: the SDK is the typed reference implementation. Extensions reuse it in phase 5.

Data:

- `Council.spawn(task, *, provider, model, iteration_budget, allowed_models, files, key) -> Subagent` (renamed from `publish_spawn`).
- `Council.run(tasks, *, timeout_secs=240) -> tuple[SubagentRecord, ...]`: spawn all, then wait.
- `Council.wait(session_ids=None, *, timeout_secs=240) -> tuple[SubagentRecord, ...]`.
- `resume(session_id, task, *, timeout_secs=240)`, `cancel(session_id)`, `extend(session_id, iterations)`, `deny(session_id, reason)` and `grant(session_id, files)` return one `SubagentRecord`.
- The HTTP read timeout of a waiting call is `timeout_secs` plus 30 seconds.
- `Council.subagents`, `cancel` and `route` stay.
- `Session.run(request, *, response_model=None, timeout=None)`. `Agent.run` calls it.
- `Session.fork(through_turn_id=None) -> Session` over `POST /v1/sessions/:sid/forks`.
- `SubagentRecord` is a frozen dataclass from the `record` schema.

Acceptance criteria:

- Unit tests cover each method against a fake layer and the contract schemas.
- Pyright CI stays green.
- No remaining reference to `publish_spawn` in code, docs or examples.

## 5. Extension API through `HostLayer`

Rationale: extensions get the SDK interface without credentials and without a second implementation.

Data:

- New host door `__vis_host_request__(method, path, query, body)`. The engine dispatches it to the built-in router (`gateway/server.clj`, `route-precedence`) with the bound session identity.
- `HostLayer(ExecutionLayer)` implements `_request` through that door. `connect` and `close` do nothing.
- `HostLayer` allows only these route groups: sessions, turns, transcript, attachments, forks, snapshot, Council and agents. It refuses all other routes with `forbidden-route`.
- `vis.council` becomes the engine `Council` bound to the calling session. Remove the separate `_Council` class. `wake` keeps its behavior.
- `vis.sessions` gives `create(**options) -> Session`, `get(id) -> Session`, `list(search=None)`, `export(id=None) -> bytes` and `restore(data, *, root=None, group_id=None, key=None) -> Session`.
- `vis.run(request, **options)` creates an independent session, runs one turn and returns the result. It accepts the same options as `Agent.run`.
- Sessions that an extension creates record `origin = {kind: "extension", extension, session_id}`.
- Each new public method has an Activity presentation, for example "Run subagents", "Create session", "Export session" and "Restore session".

Execution contexts:

| Caller | Allowed |
|---|---|
| Active extension callback | All operations. `spawn` needs the parent's complete checkpoint. |
| Extension background thread with a bound session | Reads, `wait`, `cancel`, `extend`, `deny`, `wake`, and `send` or `run` in sessions that this extension created. |
| Model `python_execution` block | Nothing from `vis.sessions`, `vis.run` or `HostLayer`. Existing model tools stay. |
| Registration-only context or outside host | Refused with `session-not-bound`. |

Authorization:

- The parent is always the bound session. No argument selects another parent.
- An extension can mutate only the bound session, its owned children and sessions that the same extension created.
- An extension can read the sessions that `vis.read_session` can read today.
- A created session gets at most the permissions of the bound session. It never gets more.

Acceptance criteria:

- A JVM boundary test calls every `vis.council.*` and `vis.sessions.*` method from a sync and an async callback.
- A test calls `vis.sessions.create` from a `python_execution` block and gets a refusal.
- `apropos("sessions.create")` and `apropos(r"^vis\.run")` find nothing in the model sandbox.
- Activity tests cover registration and the running, success, failure and empty states.

## 6. Session snapshots

Rationale: you can save a session tree, close a stdio engine or restart a gateway, then continue.

Data, file format `.vissnap` version 1:

- Magic bytes `VISSNAP\0`, a 4-byte format version, a 4-byte manifest length and the manifest as UTF-8 JSON.
- The manifest holds `format_version`, `vis_version`, `created_at`, `root_session_id`, the session tree (`id`, `parent_id`, `title`), counts, sizes and `payload_sha256`.
- The payload is a Nippy-frozen map of plain EDN values, with Nippy compression.
- Python reads the manifest without a JVM: `blockether.vis.engine.Snapshot.info(data)`. Python never decodes the payload.

Content for each session in the tree:

- Kept: turns, the model-visible context with fold gists, goal, title, project root, model preference, tool selection, session toggles and attachments (one copy for each sha256).
- Kept for children: task, iteration budget and use, file rules, parent link and the Council team thread between tree members.
- Excluded: credentials, granted permissions, processes, REPLs, sandbox variables, live views, pending replies and messages from sessions outside the tree.

Routes and calls:

- `GET /v1/sessions/:sid/snapshot` returns the artifact. `POST /v1/sessions/import` takes it.
- SDK: `Session.export() -> bytes` and `ExecutionLayer.restore_session(data, *, root=None, group_id=None, key=None) -> Session`.
- Extension: `vis.sessions.export` and `vis.sessions.restore` (phase 5).

Restore rules:

- Each session gets a new id. Parent and child links point to the new ids. Each session records `imported_from = {session_id, snapshot_sha256}`.
- A child that was running at export restores as `interrupted`. The parent continues it with `council.resume`.
- Permissions come from the policy of the caller who restores, never from the file.
- With `key`, a second restore returns the tree from the first restore.
- The write is atomic: all sessions and attachments, or nothing.

Acceptance criteria:

- Export, close a `LocalEngine`, restore in a new `LocalEngine`, then the next turn uses the earlier context.
- The same round trip works on a gateway across a restart, with a parent that has two children.
- The restored parent resumes a restored child with `council.resume`.

Unknowns:

- The default size limit. Start with 1 GiB and make it a setting.

## 7. Documentation

Data:

- `sessions-api.md`: independent sessions from the SDK and extensions, `Session.run`, `fork`, `export` and `restore`.
- `council-api.md`: the managed-child operations with the new names.
- `extension-api.md`: `vis.sessions`, `vis.run`, `vis.council` and the execution-context table.
- `http-api.md`: the new routes.
- `sessions-api.md` and `extension-api.md`: `models()`, prices, limits and model selection.
- Give each example as a Python block, then as an HTTP block (`PAIRED VARIANTS` in `docs/core.clj`).
- Document spawn idempotency, reconciliation after an uncertain write, the budget after `resume`, and that a cancel request does not prove that a child stopped.

Acceptance criteria:

- `docs-page-canon-test`, `docs-modules-test` and `test_api_guides.py` pass.

## 8. End-to-end validation on a separate server

Rationale: JVM tests and a native build do not prove that the shipped binary works.

Environment:

- Use the administered staging server through the `uplink` extension. Keep its host details in `infrastructure`, never here.
- Install the Linux x86_64 native build of the candidate commit and the matching `vis-agent` wheel.
- Run a gateway as a user service with its own database. Do not reuse the developer machine's state.
- Install one test extension, `e2e-sessions`, from `test-native/` fixtures. Its tools call `vis.council.*`, `vis.sessions.*` and `vis.run`.
- Use one cheap model route for all scenarios, and record the route.

Scenarios. Each one must pass on the server:

| # | Scenario | Pass condition |
|---|---|---|
| E1 | SDK: `GatewayClient` creates a session, `Session.run` answers, `Turn.cancel` stops a long turn | Answer is non-empty; cancelled turn has status `cancelled` |
| E2 | SDK: `council.spawn` twice with one `key`, then `wait` | One child exists; record status `completed` |
| E3 | Extension tool calls `vis.council.spawn` without a key; a forced host retry repeats the write | One child exists |
| E4 | Child exhausts a budget of 3 iterations; extension calls `extend`, then `resume` | Child completes; `iterations_used` is above 3 |
| E5 | Extension calls `deny` on a budget request | Child ends without more iterations; parent gets the reason |
| E6 | Extension calls `cancel` on a running child, then `wait` | Record status is `cancelled` or `interrupted`; no turn runs 10 s later |
| E7 | Extension background thread calls `spawn` | Refused; no child exists |
| E8 | Model block calls `vis.sessions.create` and `HostLayer` directly | Both refused; session count unchanged |
| E9 | Extension calls `vis.run` | New independent session with `origin.kind = "extension"` and no parent |
| E10 | Extension calls `HostLayer` on `/v1/settings` | Refused with `forbidden-route` |
| E11 | Extension mutates a session that another extension created | Refused |
| E12 | Snapshot of a parent with two children, `systemctl restart` of the gateway, `restore`, then a follow-up turn that needs earlier context | Answer uses the earlier context; tree shape and new ids are correct |
| E13 | Snapshot taken while a child runs | Restored child is `interrupted`; `council.resume` completes it |
| E14 | stdio: export from `LocalEngine`, `close`, restore in a new `LocalEngine`, continue | Answer uses the earlier context |
| E15 | Restore the same file twice without `key`, then twice with `key` | Two trees, then one tree |
| E16 | An automation runs a prompt that calls the `e2e-sessions` tool | Automation run succeeds; child and independent session exist |
| E17 | SDK and extension call `models()` | Same list as the TUI picker; a metered model has `pricing`; each model has its provider `limits`; an unauthenticated provider has `is_available = false` |
| E18 | Create a session and spawn a child with explicit `provider` and `model`, run one turn each | Turn usage shows that pair; turn cost equals tokens times the listed rates |
| E19 | Create a session with an unknown model; spawn a child with a model outside `allowed_models` | `unknown-model` and `model-not-allowed`; no session or child exists |

Acceptance criteria:

- All of E1 to E19 pass on the candidate build, with the session ids and commit recorded in the plan state.
- The `test-native/` suite covers E3, E7, E8 and E12 against the built image (`-M:test-native`).
- A grep of the server logs and every artifact finds no gateway token, provider key or lease secret.

## Edge cases

Each edge case needs a retained test. "Unit" means a JVM or pytest test, "native" means `test-native/`, "E" means a scenario above.

Council and spawn:

- `spawn` when Council or subagents are off: `disabled`, no child (unit).
- `spawn` when the parent has no complete checkpoint: refused before any write (unit).
- `spawn` with a model outside the inherited `allowed_models`: refused (unit).
- `spawn` with file rules wider than the parent's grants: refused (unit).
- `iteration_budget` of 0, a negative value or above 600: `invalid-request` (unit).
- An empty `tasks` list for `run`: `invalid-request` (unit).
- The same `key` with different input: `idempotency-conflict` (unit).
- `wait`, `resume`, `extend`, `deny` or `grant` on a session that is not an owned child: refused (unit).
- `wait` with an id that is not an owned child: that record has status `unknown`; the other records still answer (unit).
- `wait` with `timeout_secs` 0 or above 1800: `invalid-request` (unit).
- `resume` on a running or a cancelled child: refused (unit).
- `cancel` on a finished child: returns its record unchanged (unit).
- `cancel` cascades to grandchildren (unit, E6).
- The parent is cancelled while an extension waits on a child: `wait` returns, the child is cancelled (unit).
- The gateway restarts during `wait`: the SDK raises `TransportError`; a new `wait` returns the durable records (unit).
- A human holds the queue of the child: `wait` reports `held`, and the extension cannot release the hold (unit).

Extension boundary:

- A call from a registration-only context: `session-not-bound` (unit).
- A nested call: an extension tool calls `vis.tools` that calls back into the waiting extension: refused as today (unit).
- An async callback and a sync callback give the same result (unit).
- A background thread outlives the bound session: later calls fail with `session-not-bound` (unit).
- `/reload` replaces the extension while its background thread runs: old thread calls fail, no crash (unit).
- A large response (1000 children, a long transcript) crosses the door within the descriptor and memory limits (unit).

Snapshots:

- A truncated file, a wrong magic value or a wrong `payload_sha256`: rejected before any write (unit).
- A newer `format_version`: `unsupported-version` (unit).
- A payload with a Java class or record that is not on the Nippy allowlist: rejected (unit).
- A file above the size limit: rejected before decoding (unit).
- A cycle or a duplicate id in the manifest tree: rejected (unit).
- The project root does not exist on the target: `project-not-found` unless `root` is given (unit).
- A tool in the selection is not installed on the target: dropped, with a warning in the result (unit).
- A model route is not configured on the target: preference kept, warning in the result, next turn uses the router (unit).
- A session without turns: exported and restored with its settings (unit).
- A session with a running turn: that turn is excluded and marked `interrupted` (E13).
- An export of a child alone: the child restores as an independent session without a parent link (unit).
- A disk-full or other failure in the middle of a restore: nothing remains (unit).
- Attachments shared by several sessions: stored once, restored to each session (unit).
- Unicode titles and content, including 4-byte characters, survive the round trip (unit).
- Restore by another user or on another machine: no credential is needed and none is restored (E12, E14).

Models:

- A model without a known price: `pricing` fields are `null`, and the cost estimate says "unknown" (unit).
- A subscription model: rates shown, `billing = "subscription"`, no extra cost (unit).
- The same model name under two providers: two entries, selected by the pair (unit).
- A pinned model disappears from the live provider catalog: the next turn fails clearly or falls back by router policy, as today, and `models()` no longer lists it (unit).
- A catalog refresh runs during `models()`: the call returns the last complete catalog (unit).
- A provider loses authentication: its models stay listed with `is_available = false` (unit).
- `set_model` during a running turn: the change applies at the next request boundary (unit).

## Checks

- Clojure: `clj.run_tests` for the affected namespaces, `clj.lint_code` and `clj.format_code`.
- Python: `vis-agent python -m pytest packages/vis-agent/tests`, ruff and pyright.
- Contract: schema tests in `packages/vis-contract`.
- Native: `-M:test-native` on the built image for the cases above.
- End to end: phase 8 on the separate server.

## Plan state

- Phase 1: done. `agents/operation!` is the one entry; it does not need the model tool registry.
- Phase 2: done. Routes for `run`, `wait`, `resume`, `extend`, `deny` and `grant`.
- Phase 3: done. `GET /v1/models` lists model objects; `loop.router/model-rates` gives the listed rates and the cost estimate. SDK `models()`, `model_limits()`, `Model`, `create_session(provider, model)`, `Session.model()`, `Session.set_model()` and `SessionModel`.
- Phase 4: done. `Council` agent methods and `SubagentRecord` stay experimental and hidden from the API reference. `Session.run` shares one implementation with `Agent.run`; `Session.fork` returns the new `Session`.
- Phase 5: done. The host door `__vis_host_request__`, `HostLayer`, the `Sessions` facade (`create`, `get`, `list`), `vis.run`, `vis.session`, `vis.council` and the session `origin`. The `extension` field of each route in `gateway.json` sets the access. A new session starts in the bound session root. Each change through the door has an Activity from `host-activities`; reads have none, because a turn wait polls them. Phase 6 adds `export` and `restore` to `Sessions`.
- Phase 6: not started.
- Phase 7: not started.
- Phase 8: not started. The server is reachable through `uplink`; it has `vis-agent` and Java, but no `vis` binary yet.
