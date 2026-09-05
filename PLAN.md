# PLAN — Core behind, SDK in front, one contract underneath

*Every consumer gets an SDK; only the engine gets internals.*

## Context

**State before.** Vis already has three partial public surfaces, but none is the one boundary every
consumer uses.

- `src/com/blockether/vis/core.clj:1-39` calls itself the only host facade, binary entry point and
  public integration surface at once. It imports gateway client/server, persistence, registries,
  sandbox and execution internals at `core.clj:42-80`; its View exports at `core.clj:792-815` resolve
  a second implementation namespace dynamically.
- `src/com/blockether/vis/view.clj:112-340` separately owns semantic View builders, while
  `internal/view/core.clj:1-15,1720-1950,2382` combines Human Input and Live View lifecycle, secrets,
  materialization, storage, wire and Python entry points in 2 672 lines. The same primitive therefore
  has multiple public entrances and names exposing implementation modes.
- The TUI declares a dependency on the whole engine at
  `extensions/channels/vis-channel-tui/deps.edn:10-12`. Its production code imports
  `com.blockether.vis.core` plus engine internals for attachments, formatting, configuration,
  workspace, View, wire, themes and iteration state; for example `channel_tui/chat.clj:8-13` and
  `channel_tui/human_input.clj:26-28`.
- The Companion implements its own client and protocol mirror. `apps/vis-companion/src/lib/gateway.ts`
  is 4 717 lines (169 KB), `types.ts` 1 056, `live-view.ts` 992, `fleet.ts` 783, `human-input.ts` 434,
  `subscriptions.ts` 392, `relay.ts` 293, `compat.ts` 185 and `endpoints.ts` 183, while
  `App.tsx:19-20,246-249` constructs that private `GatewayClient` directly. Three JVM tests read those
  files by literal path — `channel_tui/test/.../view_cross_channel_test.clj:522,900` for
  `human-input.ts` and `live-view.ts`, `test/.../gateway/server_test.clj:980` for `gateway.ts`, and
  `test/.../gateway/relay_test.clj:341` for `relay.ts` — so moving that source without a successor
  silently disarms the only cross-language drift gates the repository has.
- Python already resembles an SDK but is named as a product half: `packages/vis-agent/pyproject.toml:1-18`
  says its exact `vis/__init__.py` source is both published and executed by the engine. It is released
  as `vis-agent`, imports as `vis`, and only covers extension-host operations; it is not a gateway
  client shared with other consumers. `vis-agent` is also the shipped command — `bin/vis-agent` is
  `clojure -M:vis`, with `bin/install-vis-agent`, the `vis-agent-<os>-<arch>-*` release assets and
  `vis-agent update` — so the distribution name and the product name are not the same decision. The
  source path itself is wired into `deps.edn:6-10`, where `packages/vis-agent/src` is a RESOURCE root
  on `:paths`, plus `build.clj:1092-1094`, `.github/workflows/ci.yml:264-305` whose conftest expects
  `vis-contract` as a sibling directory, and `e2e/run.py`.
- `packages/vis-contract/resources/vis-contract/` now contains the portable JSON documents and
  same-named JSON Schemas for the extracted contract areas. Skjema validates each source document
  before Clojure derives runtime-friendly views; Clojure Spec is no longer a contract layer.
- `internal/gateway/wire.clj:56-184` mixes canonical key/JSON conversion with gateway event sets and
  SSE framing. `internal/gateway/protocol.clj:22-78` mixes pure compatibility declarations with
  release/build/runtime discovery. `gateway/state.clj:1-5536`, `server.clj:1-5019` and
  `client.clj:1-2719` each combine multiple owners.
- `src/com/blockether/vis/internal/loop.clj:1-12120` owns block execution, iterations, turns,
  environment lifetime and session caches. `internal/activity/core.clj:1-406` and its children reduce
  tool observations but sit beside Core even though Activity is execution trace data.
- `resources/META-INF/vis/manifest.edn:2-54` registers 25 namespaces called `foundation`, and
  `src/com/blockether/vis/internal/foundation/` contains 56 implementation files: environment
  discovery, workspace context, editing, shell, shims, Python capture, PTY, MCP, harness,
  introspection and model-facing tool registration. That is the execution environment of Core, not
  a peer layer called Foundation or Base Tooling.
- Production extensions still import engine internals directly. Those concrete edges are migration
  inputs for the SDK phases; they are not encoded as contract data or maintained as an exception ledger.
- Two packaging facts constrain the SDK work.
  `resources/META-INF/native-image/com.blockether/vis/reachability-metadata.json` names no Vis
  namespace at all, so renaming namespaces adds nothing there; the native risk is `build.clj`'s
  manifest-derived entrypoints and symbols resolved from strings at runtime, such as the CLI's
  `requiring-resolve` of `com.blockether.vis.ext.channel-tui.screen/run-chat!`. And there is no root
  `package.json` or npm workspace: `apps/vis-companion` is standalone, and its `scripts/version.mjs` is
  the only thing allowed to write `VIS_VERSION` into package, lock and `pyproject` manifests.
- The Phase 1 diagnostic baseline records source bytes / public Vars for the largest current owners:
  `core` 46 304 / 516, `internal.view.core` 118 995 / 47, `internal.loop` 609 757 / 69, Activity 13 341 /
  11, gateway state 255 276 / 86, server 229 328 / 9, client 122 037 / 104, wire 10 885 / 17 and
  protocol 23 471 / 21. Excluding generated/cache trees, package-source baselines are `vis-contract`
  137 007 bytes, `vis-agent` 437 750, Companion `src/lib` 1 278 122 and TUI `src` 2 254 956. These
  are migration diagnostics, not budgets; Phase 13 measures the final owners again.

**Root problem.** Repository proximity is being mistaken for API access. In-process consumers call
Core internals, the out-of-process Companion hand-builds a second client, Python exposes only one
slice, and contracts follow whichever implementation was written first. `Foundation` then gives
Core's execution environment a misleading identity as another architectural layer. The result is
three drifting public surfaces and no way to prove that a Clojure, JavaScript and Python consumer
mean the same thing.

**Target.** Contract, Core and SDK are distinct roles:

```text
TUI · Companion · Clojure extensions · Python extensions · external clients
                                  │
                                  ▼
             Vis SDK: Clojure · JavaScript · Python
                                  │
                                  ▼
                         packages/vis-contract

internal.gateway.server ─────► internal.core ─────► packages/vis-contract
                                      │
                                      └── internal.core.environment
```

Dependency arrows point toward what may be imported. Consumers import only their language's Vis SDK.
SDKs depend on `vis-contract` and ordinary transport/runtime libraries, never on the engine,
Companion, TUI or one another. Core and the gateway server depend directly on the contract, never on
an SDK. The engine satisfies contract-declared host operations by injecting a host adapter into the
Clojure/Python SDK when it loads an extension. Out-of-process SDK clients reach Core only through the
gateway contract.

`vis-contract` remains the source of declarations, schemas, canonical codecs, route/event
vocabularies and host operations. It performs no IO or lifecycle. `vis-sdk` is the behavioral public
surface: builders, extension registration, host calls, gateway connection/authentication,
subscriptions, commands, replay/materialization and testing helpers.

One versioned SDK product is shipped in three ecosystems:

```text
packages/vis-sdk/
├── clojure/      com.blockether/vis-sdk     → com.blockether.vis.sdk.*
├── python/       vis-sdk                    → import vis
└── javascript/   @blockether/vis-sdk        → ESM JavaScript + .d.ts
```

The JavaScript distribution is runnable JavaScript with TypeScript declarations, not a
Companion-only source folder. All three versions mirror `VIS_VERSION`. Shared semantics are pinned
by contract fixtures; APIs may use idiomatic language spelling.

Core owns Session, Turn, View, execution and Environment. Human Input is a View with a response
contract. Tool Activity is execution trace data, not a View. The complete built-in agent execution
environment has one internal owner:

```text
path:       src/com/blockether/vis/internal/core/environment/
namespace: com.blockether.vis.internal.core.environment.*
```

That owner includes workspace/environment discovery, built-in editing and shell tools, sandbox
shims, Python capture, PTY, MCP, harness and introspection. It is split into cohesive children, not
one file. Cross-owner shapes first move to `vis-contract`; code used only by an extension or channel
moves to that consumer. Optional search and voice remain SDK-based extensions and become
`vis-search`/`vis-voice`.

**Alternatives considered.**

- *Rename `vis-contract` to `vis-sdk` and keep one artifact.* Lost: declarations must remain a
  dependency leaf, while SDKs necessarily perform host dispatch, HTTP/SSE, replay and lifecycle.
  Combining them would make the engine depend on its own client and would force every contract
  consumer to install behavior it does not run.
- *Keep `com.blockether.vis.core` as the Clojure SDK.* Lost: it imports implementation namespaces and
  also owns the native binary entry point. A real SDK must compile and test with no engine on its
  classpath, just as the current Python package runs without a host.
- *Give only the Companion a JavaScript SDK and let the TUI call Core directly.* Lost: same-process
  access is the largest source of current leakage. The TUI should be the strongest Clojure SDK
  conformance consumer, not an exception.
- *Keep the Companion's private `GatewayClient` behind a wrapper package.* Lost: that preserves two
  owners. Client transport, auth, routes, subscriptions and wire models move into the JavaScript SDK;
  the app keeps rendering, native integration and local UI state.
- *Generate one language implementation and transpile it three ways.* Lost: JVM host injection,
  Python extension execution and browser/Capacitor networking are genuinely different runtimes.
  They share contract data and conformance fixtures, not executable source.
- *Move Foundation to `internal.base-tooling`.* Lost: the tools, sandbox and workspace context are
  how a Turn's execution Environment is constructed and retired. A second sibling layer would hide
  that lifecycle ownership and recreate the drawer under a cleaner name.
- *Make the whole Environment an extension.* Lost: it is required Core execution infrastructure;
  optional search and voice already demonstrate the extension boundary. Core owns lifecycle, while
  SDK contracts keep implementations replaceable and testable.
- *Use both `contract` and `spi`.* Lost: two names recreate the same boundary under different labels.
  Host callbacks, channel/provider operations and persistence ports are contracts; no `*.spi`
  namespace is introduced.
- *Merge gateway client and server.* Lost: HTTP/SSE is a real process boundary. The server remains an
  internal Core adapter; clients belong to each public SDK and share only contract declarations.
- *Put Activity into View because both render rows.* Lost: Activity reduces execution events and
  accepts no actions. View has lifecycle, patches and optional responses; only presentation data may
  be shared.
- *Preserve old packages or namespaces as aliases.* Lost: aliases produce two public surfaces and the
  repository deliberately removes obsolete paths. Each vertical slice moves callers and deletes its
  old owner in the same commit.
- *Rewrite behavior while relocating it.* Lost: this is a boundary refactor. Wire, cancellation,
  View, persistence and rendering behavior are characterized first; semantic changes require their
  own failing test and commit.

## Phase 1 — Make the final dependency graph executable

**Rationale.** Three SDKs and a large namespace move require direct tests for the intended public
consumption graph. Architecture policy must describe the destination without preserving obsolete paths.

**Data.** The allowed production edges are:

```text
contract.*                    → Clojure/JDK/leaf libraries only
internal.core.*               → contract.* + internal Core leaf utilities
internal.gateway.server.*     → contract.* + internal.core.* + server children
Clojure/Python/JavaScript SDK → canonical contract documents + language runtime/transport only
TUI, Companion, extensions    → their language SDK + own code + UI/runtime libraries
```

No consumer imports `com.blockether.vis.core`, `com.blockether.vis.internal.*`,
`com.blockether.vis.contract.*`, raw gateway route strings or another SDK implementation. Tests may
import the production namespace they test; contract conformance tests exercise public artifacts.

**Acceptance criteria.**

- Add direct Clojure namespace and JavaScript/Python import gates for the final graph. They report
  current offenders from source rather than storing an allowlist in the contract.
- Pin the current route table, gateway event vocabulary, View open/patch/action/close behavior,
  Human Input validation/secrets, Activity reduction, turn cancellation and cross-session permit
  isolation before moving their owners.
- Inventory direct TUI and extension internal imports, Companion route/header/event literals and
  Python host operations as named migration inputs rather than broad exceptions. The measured inputs
  are 33 distinct engine namespaces across 18 production packs, 21 of them in the TUI, and the
  Companion literals in `gateway.ts`, `compat.ts`, `endpoints.ts` and `types.ts`.
- Characterize the three path-pinned cross-language mirror tests before any of their sources move, and
  name the contract fixture that replaces each. A gate may be replaced, never deleted.
- Record namespace bytes, public vars and package sizes as diagnostics. Final budgets are based on
  the new owners, not arbitrary limits chosen before extraction.
- Require every phase to land in independently green vertical slices with source, consumers,
  contract fixtures and tests moved together; no forwarding namespace, compatibility package or
  fallback import is accepted.
- Keep `packages/vis-contract` independently loadable and all SDK artifacts buildable without the
  engine throughout the plan.

**Unknowns.** Whether the Clojure graph gate should consume clj-kondo analysis or parse `ns` forms
itself. Choose the smaller deterministic implementation in the normal JVM suite; JavaScript and
Python gates should inspect imports and declared wire constants, not introduce another graph tool.

## Phase 2 — Put canonical wire and gateway semantics in `vis-contract`

**Rationale.** SDK clients cannot be independent while canonical encoding and route/event semantics
are owned by the gateway implementation. This is the lowest shared extraction and gives every
language one protocol source.

**Data.** Move by responsibility:

| current symbol/family | target owner |
|---|---|
| `wire-key`, `engine-key`, `->wire`, `->engine`, `canonical`, `json-str`, `parse-json` | `com.blockether.vis.contract.wire` |
| terminal/queue event sets, protocol version, headers, pure compatibility verdict | `com.blockether.vis.contract.gateway` |
| routes, request/response envelopes, journal envelope and event vocabulary | `resources/vis-contract/gateway.json` + `schema/gateway.json` |
| route/event/key readers | each SDK's contract module |
| `sse-frame`, `job-sse-frame` | `internal.gateway.server.transport.sse` |
| bounded diagnostics | the one internal caller or a genuine internal leaf utility |
| release/build identity, checkout inspection, daemon staleness and messages | `internal.gateway.runtime` |

**Acceptance criteria.**

- Core producers, gateway server and all three SDKs use contract-owned canonical wire behavior;
  `internal.gateway.wire` is deleted rather than forwarded.
- Preserve total encoding of non-string keys, NaN/infinities, UUIDs, dates, symbols and keywords,
  plus in-process/JSON round-trip parity through shared fixtures.
- `gateway.json` declares every method/path, protocol header, request/response envelope, event type,
  replay rule and terminal semantic; `schema/gateway.json` validates every shape.
- Routes intended only for administration or one host are marked explicitly rather than omitted.
- SSE byte framing, Ring and concrete HTTP remain implementation; the contract owns semantics only.
- SDK artifacts package or read the canonical JSON documents directly; no aggregate is generated.
- `vis-contract` retains zero engine, SDK, filesystem, network, process or daemon dependencies.

**Unknowns.** Which `/v1/admin/*` routes are intentionally public to SDK clients. Classify every live
route in `gateway.json`; absence from documentation is not a protocol category.

## Phase 3 — Make JSON Schema the executable contract

**Rationale.** Cross-language shapes need one representation that every ecosystem can read and one
validator with identical source data. Clojure-specific declarations create a second authority and hide
wire constraints from Python and JavaScript.

**Data.** Every portable area owns two files:

```text
resources/vis-contract/<area>.json
resources/vis-contract/schema/<area>.json
```

Clojure loaders call `contract.document/load!`, which rejects non-JSON-domain values and validates the
document with Skjema before exposing derived keyword views. Runtime callbacks, atoms, IO and process
handles are not contract data; their owning namespaces use local predicates.

**Acceptance criteria.**

- No production or test namespace depends on Clojure Spec.
- Every contract document has one same-named JSON Schema, and every schema-backed document loads
  through Skjema in the normal test suite.
- View, content, gateway, configuration, Python host operations, toggles, provider limits, language
  surfaces and test-runner results are sourced from JSON documents rather than Clojure literals.
- Raw contract validators reject keyword keys, lists, non-finite numbers and other values that cannot
  cross a JSON boundary.
- No generated `contract.json` aggregate is checked in; the Python wheel packages the canonical documents
  and schemas directly.
- Private implementation shapes remain local predicates instead of expanding the public schema set.
- Obsolete host-boundary inventories, forwarding namespaces and migration exception ledgers are
  deleted rather than represented in the new contract.

**Unknowns.** Which remaining engine-only shapes are genuinely portable. Add a schema only when a
second process or language consumes the shape; otherwise keep the predicate private.

## Phase 4 — Establish one Vis SDK product in three ecosystems

**Rationale.** Consumers need a real destination before `core.clj`, the private Companion client and
Python package are dismantled. The SDK must first reproduce existing behavior from outside the
engine, then later phases can move domains behind it without reopening consumers.

**Data.** Create one versioned product with language-native modules:

```text
packages/vis-sdk/
├ clojure/
│  └ com.blockether.vis.sdk.{extension,client,session,turn,view,resources,testing}
├ python/
│  └ vis.{extension,client,session,turn,view,resources,testing}
└ javascript/
   └ @blockether/vis-sdk: client · session · turn · view · resources · testing
```

Artifact identities are `com.blockether/vis-sdk`, `vis-sdk` (still imported as `vis`) and
`@blockether/vis-sdk`. The top-level APIs are small conveniences over domain modules, not another
hundreds-of-vars facade.

The shipped command keeps its name: `bin/vis-agent`, `bin/install-vis-agent`, the
`vis-agent-<os>-<arch>-*` release assets and `vis-agent update` are untouched — only the Python
distribution and its source path move. SDK modules re-export contract types verbatim: a consumer never
imports `contract.*`, and no SDK invents a second spelling for a contract vocabulary.

**Acceptance criteria.**

- Each SDK depends on canonical `vis-contract` data and ordinary language libraries only; building
  any SDK with the engine, TUI and Companion absent succeeds.
- Clojure and Python extension modules receive a contract-declared Host object/adapter from the
  engine and have deterministic outside/test hosts. They never resolve engine Vars or import
  internals.
- All three client modules implement connection identity, protocol negotiation, auth/lease, sessions,
  turns, View events/actions, cancellation, subscriptions/replay and resources from `gateway.json`.
- Clojure production HTTP uses `babashka.http-client`. JavaScript ships framework-free ESM usable in
  browsers, Node and Capacitor plus `.d.ts`; it has no React or Capacitor dependency. Python remains
  usable on the CPython floor the embedded interpreter ships.
- Move the exact `packages/vis-agent/src/vis` source into the Python SDK and extend it rather than
  creating a hand-synchronized copy. Delete the `vis-agent` distribution/path when all callers move;
  keep the ergonomic `import vis` name as the chosen SDK API, not as an alias package.
- Common golden fixtures exercise protocol negotiation, one session/turn, View replay/action,
  cancellation and resource retrieval against every SDK implementation.
- Add package smoke tests from built artifacts: Clojure with only the SDK dependency, Python from a
  wheel, and JavaScript from the packed npm tarball.
- `VIS_VERSION`, release automation and license audit cover all three artifacts at the same version.
- Move every consumer of the old Python path in the same slice: `deps.edn:6-10`, where
  `packages/vis-agent/src` is a resource root on `:paths`, `build.clj:1092-1094`,
  `.github/workflows/ci.yml:264-305` whose conftest expects `vis-contract` as a sibling directory,
  `e2e/run.py` and the docs. The CLI name, installer assets and update tracks do not change.
- Decide JavaScript packaging explicitly: the repository has no root `package.json`, so the plan adds
  either an npm workspace or a packed-tarball dependency, and extends
  `apps/vis-companion/scripts/version.mjs` to own the new package and lock files.
- A fixture test fails when an SDK declares a node kind, event name, field type, header or route the
  contract does not; re-export is a thin alias, never a wrapper vocabulary.
- The Clojure SDK carries both the out-of-process client and the in-process host adapter, because the
  TUI is registered through `resources/META-INF/vis/manifest.edn` and runs inside the engine's process.

**Unknowns.** None about the public product split: contract and SDK remain separate, and all three
SDK distributions ship. Registry publication occurs through the existing release process after the
packages pass local artifact smoke tests.

## Phase 5 — Make the former Foundation the Core execution Environment

**Rationale.** Editing, shell, workspace context, sandbox shims, MCP and harness are not a sibling
adapter collection. They construct the capabilities in which a Core Turn executes and share that
environment's lifecycle, cancellation and disposal.

**Data.** After Phase 3 extracts contracts, classify the 56 files into this owner:

```text
internal/core/environment/
├ lifecycle.clj
├ workspace/                 # host, repositories, languages, monorepo, rendering
├ tools/                     # editing, shell, introspection, rewind, doctor, slashes
├ sandbox/                   # shims, Python capture, PTY and bridge
├ mcp/                       # client, auth and registered operations
└ harness/                   # discovery and execution
```

The exact file count may shrink where a helper has one caller. `gif` or another channel-only helper
moves to that channel rather than being hidden in Environment.

**Acceptance criteria.**

- Move every Core-owned `internal.foundation.*` implementation, test, apropos resource and manifest
  entry under `internal.core.environment.*`; delete old namespaces in the same slices. The 25 manifest
  registrations carry `:apropos "META-INF/vis/apropos/shim-*.edn"` names, so regenerate those resources
  with `apropos-resource-test/regenerate!` in the same slice or the drift test fails.
- Environment owns creation, workspace context, tool capability assembly, cancellation attachment,
  sandbox handles and disposal. Session/Turn code uses its operations without importing concrete
  tool children.
- Contracts and SDK contract readers leave the old directory first. Environment retains no
  public facade or transport protocol ownership.
- Language packs replace direct `foundation.environment.languages` and `foundation.editing.parse`
  imports with SDK operations/types or extension-local behavior; the shared language-surface result
  contract already moved to `contract.surface` in Phase 3.
- Delete the optional search extension rather than renaming or carrying it through the SDK migration. Rename
  the gateway speech subsystem consistently; neither concern belongs under Environment.
- A tree gate rejects `foundation` and `base-tooling` in production paths, namespace symbols,
  coordinates, manifest entries and active product copy. Historical release prose is not rewritten.
- Lifecycle tests prove a cancelled/condemned Environment cannot be reused, resources are disposed
  once, and one Session's Environment failure cannot affect another.
- `reachability-metadata.json` names no Vis namespace, so this move adds no entry there. The native
  verdict is a green `clojure -T:build native` and `-M:test-native`, because reachability derives from
  the manifest entrypoints and symbols resolved from strings survive a JVM-only run unnoticed.
- JVM and native manifest tests preserve shipped capabilities and registration order apart from
  deliberate names.

**Unknowns.** A helper with both Environment and channel callers exposes contract data or moves to a
true shared leaf only when semantics are owner-neutral. It does not justify a new miscellaneous
layer.

## Phase 6 — Make View one Core primitive exposed only through SDKs

**Rationale.** Human Input and Live View already share implementation but expose separate lifecycle
names and client mirrors. Core should own one semantic state machine; SDKs should be the only way to
construct, observe or act on it.

**Data.** The target ownership is:

```text
contract.view
  nodes · fields · actions · patches · events · snapshots · response schema

internal.core.view
  normalize · lifecycle · response · secrets · materialize · store

vis-sdk/{clojure,python,javascript}.view
  builders · typed snapshots/events · open/patch/action/request/close conveniences

internal.gateway.server.routes.views
  transport adaptation only
```

`com.blockether.vis.view`, public `com.blockether.vis.core` View exports and the
`*-live-view!`/`request-human-input!` split are removed as their callers migrate.

**Acceptance criteria.**

- One Core state machine owns `open`, typed `patch`, user `event` and `close`; a response contract
  adds one accepted submit, validation, cancellation and blocking wait without a second store.
- Secret fields remain redacted from journals, artifacts, SDK diagnostics and fixtures and are
  revealed only through the guarded operation.
- Closing a View, cancelling a response and execution actions such as `interrupt` remain distinct
  contract events.
- SDK builders and event models derive from `contract.view`; Clojure/Python can create Views and all
  three SDKs can observe snapshots and send actions with the same semantics.
- SDK subscription/replay presents a normalized typed snapshot so TUI and Companion do not import
  Core materializers or maintain divergent protocol reducers.
- Channel/app conformance fixtures cover forms, progress/dashboard, grouped tables, repeated
  patches, reconnect/replay and terminal states.
- “Live” remains the unlabelled default while open; renderers show only meaningful exceptional or
  terminal states.
- Gateway projection/query code contains no View validation, response or lifecycle policy.

**Unknowns.** Patch syntax is not redesigned here. Preserve current typed operations unless a
characterization test proves existing producers disagree.

## Phase 7 — Put execution and Tool Activity under Core, then split the loop

**Rationale.** Activity belongs to the execution that produced it, and the 12 120-line loop hides
block, iteration, turn and cache ownership. Establishing these Core modules before gateway
extraction prevents scheduler behavior from being mistaken for domain behavior.

**Data.** The target families are:

```text
internal/core/block.clj
internal/core/iteration.clj
internal/core/turn.clj
internal/core/session.clj
internal/core/environment/*
internal/core/execution/tool_activity/{event,reducer}.clj
```

`contract.execution` owns envelopes and provider/channel/persistence/event ports. SDKs expose
stable commands and read models; they do not expose Core mutable state.

**Acceptance criteria.**

- Move `internal.activity.core`, `activity.event` and `activity.presenter` under
  `internal.core.execution.tool-activity`; collapse the presenter registry if it is only a reducer
  detail.
- Preserve replay, bounding, coalescing, resource references and late-failure behavior; Activity
  gains no View lifecycle or action handling.
- Split `internal.loop` by actual state machines, moving tests and direct callers together and
  deleting the old namespace when empty.
- Core depends on contract ports, never on SDKs, gateway server, TUI, Companion or an extension.
- Preserve Environment condemnation, hard cancellation, terminal landing and cross-session
  isolation with issue #161 regressions green after every relevant slice.
- Distinguish the in-process Core Session/Environment cache from the daemon session registry; only
  the former belongs here.
- Resolve definition order without `declare` and reject cycles in the architecture test.

**Unknowns.** Block and iteration may remain one namespace if cutting dependencies proves they are
one state machine. Ownership, not sample filenames or line count, decides.

## Phase 8 — Split gateway state by the resource it owns

**Rationale.** A stuck worker affected unrelated sessions because capacity, cancellation and session
state met in one process-wide namespace. The immediate fix is tested; explicit resource owners keep
future changes local and give SDK commands stable application operations.

**Data.** Decompose `gateway.state` into cohesive owners:

| owner | state/behavior |
|---|---|
| `gateway.events` | append/replay, subscribers, fan-out and journal sequencing |
| `gateway.turns` | queue, worker lease, permits, watchdogs, cancel/backstop and terminal landing |
| `gateway.sessions` | daemon session/project/workspace registry and current-turn reference |
| `gateway.read-model` | transcript, trace, artifacts and projections |
| `gateway.fleet` | gateway fleet/process membership and health |
| domain integration modules | providers/models, drafts and resources where they own mutation |

A namespace owns its atoms, executor and lifecycle. Server routes call operations; no SDK sees these
implementation values.

**Acceptance criteria.**

- Move one owner at a time with tests and callers; leave no forwarding `gateway.state` facade or
  duplicate registry.
- `gateway.turns` alone owns permits, worker futures, stall phase, watchdog scheduling,
  cancellation reason and terminal backstop. Permit release remains a one-shot lease even if an
  abandoned worker later returns.
- `gateway.events` alone assigns journal sequence and fan-out; projections consume events without
  mutating the journal implementation.
- Session A cancellation, stuck execution or persistence failure cannot cancel, starve or rewrite
  Session B; deterministic multi-session tests exercise every shared resource.
- Server routes never dereference gateway state atoms directly and map contract commands to owner
  operations.
- The gateway suite remains green after each extraction, including replay after non-string/NaN
  payloads and cancellation before provider output.

**Unknowns.** Provider/model catalogs and drafts mix durable data with gateway cache. Classify each
by mutation owner before extraction; do not create a generic `services` namespace.

## Phase 9 — Keep the gateway server internal and move every client into an SDK

**Rationale.** Client and server are opposite sides of a real boundary, but the client is public
behavior, not engine implementation. The current Clojure and Companion clients must become SDK
implementations of one contract while the server splits into route families.

**Data.** The target is asymmetric by design:

```text
internal.gateway.server/                packages/vis-sdk/* client modules
  daemon/lifecycle                        connection/discovery
  transport.http                          auth/lease/compatibility
  transport.sse                           subscriptions/replay
  routes.system                           system
  routes.sessions                         sessions
  routes.turns                            turns
  routes.views                            views
  routes.integrations                     integrations
  routes.resources                        resources
```

The server adapts HTTP/SSE to Core. Clojure, Python and JavaScript SDK clients adapt language-native
calls to HTTP/SSE. Shared executable transport code is neither required nor desired.

**Acceptance criteria.**

- Split server route families from `gateway.server`; each consumes methods, paths and envelopes from
  `contract.gateway` and calls one application owner.
- Move daemon discovery/start, lease, auth, protocol headers, HTTP and subscriptions out of
  `internal.gateway.client` into the Clojure SDK; delete the internal client after all engine/TUI/CLI
  callers migrate.
- Move the Companion's gateway transport, compatibility, subscriptions and protocol-owned types
  from `apps/vis-companion/src/lib` into the JavaScript SDK; app-specific rendering and native state
  remain in the app.
- Implement equivalent Python client modules from the same contract without coupling them to the
  extension host implementation.
- A route parity test proves every public contract route has a server handler and every
  client-callable route has SDK coverage in each declared language; exceptions are contract
  capability metadata.
- Core performs no HTTP. Clojure SDK calls use the canonical `babashka.http-client`; server code owns
  Ring and SSE framing.
- Delete old monolithic client/server namespaces when empty; no route string or protocol header is
  duplicated outside contract-backed SDK modules.

**Unknowns.** Local daemon discovery/start is meaningful for Clojure and Python desktop clients but
not a browser. The contract capability matrix marks it unavailable in browser JavaScript rather
than pretending parity.

## Phase 10 — Extract the TUI as a standalone gateway client

**Rationale.** A terminal UI needs process isolation and terminal libraries, but it does not need a
second copy of the engine, providers, persistence, speech runtime or embedded CPython. Keeping the
TUI inside the engine image made a renderer pull the whole host classpath into every invocation.
The durable boundary is the same one the Companion uses: HTTP/SSE plus the versioned gateway
contract.

**Data.** `apps/vis-tui` owns the Lanterna application and depends on `vis-contract`, HTTP/JSON and
rendering libraries only. `com.blockether.vis.tui.client` is its transport adapter. Gateway state
projects provider errors, session state, artifacts, Human Input and live View data before they cross
the wire; the app never imports `com.blockether.vis.core` or `com.blockether.vis.internal.*`.

**Acceptance criteria.**

- `apps/vis-tui/deps.edn` has no dependency on the Vis engine, provider packs, persistence, speech,
  Svar or embedded CPython; `tui_app_boundary_test.clj` rejects regressions.
- The root manifest and aggregate classpath contain no TUI extension entry. `vis-agent` owns the
  gateway; `vis-tui` connects as a leased protocol client and never starts an in-process engine.
- Visible rendering policy, Lanterna work and screenshot capture remain in `apps/vis-tui`; the app
  suite keeps its terminal-grid and deterministic rendering assertions.
- `clojure -T:build native` under `apps/vis-tui` produces a standalone executable. Releases publish
  `vis-tui-<os>-<arch>.tar.gz` separately from `vis-agent-<os>-<arch>-community.tar.gz`.
- The engine native image carries no Lanterna reachability metadata or PTY first-frame test; the TUI
  executable is smoke-tested through `--version` and `--help` in its own artifact path.

**Unknowns.** Automatic installation and update UX for the optional TUI can be added after native
artifacts are shipping reliably. It must remain a client install concern, not re-enter the agent
bundle.

## Phase 11 — Put Companion transport on the JavaScript SDK

**Rationale.** The Companion owns a second implementation of the protocol — 4 717 lines in the client
alone — and two JVM tests read that source as the drift gate. Transport is the half that is purely
contract behavior, so it moves first and independently of View.

**Data.** Move from `apps/vis-companion/src/lib` into `@blockether/vis-sdk`:

```text
gateway.ts 4 717 · types.ts 1 056 · fleet.ts 783 · subscriptions.ts 392 · relay.ts 293
compat.ts 185 · endpoints.ts 183      (gateway.test.ts 1 135 becomes SDK conformance)
```

**Acceptance criteria.**

- Connection identity, compatibility negotiation, auth/lease, routes, subscriptions/replay,
  cancellation and wire models live in the SDK. The app keeps rendering, local state, notifications and
  native/relay integration, and calls SDK operations for gateway work.
- A Companion gate rejects raw Vis route strings, protocol headers, event vocabularies and direct
  gateway `fetch`/stream construction outside the SDK.
- `test/.../gateway/server_test.clj:980` and `test/.../gateway/relay_test.clj:341` stop reading
  `apps/vis-companion/src/lib/*.ts`; each is replaced in the same slice by a contract-fixture drift test
  against the SDK source, so no gate disappears without its successor.
- `npm run lint`, `npm run test:storybook` and `npm run build` stay green, and CI consumes the packed
  SDK artifact rather than a source path.

**Unknowns.** Whether `fleet.ts` is transport or product state. Classify by the contract: fleet
membership and health are gateway concepts, fleet presentation is not.

## Phase 12 — Put Companion View, every extension and the last facade on the SDKs

**Rationale.** View is the vocabulary every consumer shares, so it moves after transport and after Core
owns one state machine. With it gone the engine keeps no public facade, and the debt mechanism that
Phase 1 froze can be deleted instead of maintained.

**Data.** The remaining consumers are:

```text
apps/vis-companion/src/lib   live-view.ts 992 · human-input.ts 434 · view.ts 39
extensions/**                18 production packs, 12 engine namespaces left after Phase 10
src/com/blockether/vis       core.clj 825 · view.clj 352
```

**Acceptance criteria.**

- The Companion renders View from SDK-typed snapshots and sends actions through SDK operations; no
  local protocol reducer, node table or Human Input vocabulary remains in the app.
- `channel_tui/test/.../view_cross_channel_test.clj:522,900` is replaced by a contract-fixture
  conformance test that the TUI renderer and the JavaScript SDK both run, so the closed vocabularies
  stay pinned across languages.
- Every Clojure extension imports only `com.blockether.vis.sdk.*`, its own code and ordinary libraries;
  every Python extension imports `vis`.
- Move the engine binary entry point out of `com.blockether.vis.core`, which `build.clj:746,1254` names
  as `:main`, then delete `core.clj` and `com.blockether.vis.view` without aliases.
- Update `resources/vis-docs/extending.md:1465-1475,1540,1739`, the `doc()` catalog
  `resources/META-INF/vis/apropos/docs.edn` and `resources/vis-docs/site.edn` in the same slice, so
  `doc("extending")` never describes a deleted facade.
- Delete temporary architecture migration gates once every first-party consumer uses an SDK.
- Built-artifact end-to-end tests run the TUI through the Clojure SDK, the Companion through the packed
  JavaScript SDK and a Python extension/client through the wheel against the same gateway fixtures;
  `e2e/run.py` still drives the real CLI.
- The native binary test loads a Python SDK extension and observes its registered capability through an
  SDK client.

**Unknowns.** Whether the Clojure SDK exposes View builders to extensions only or also to external
clients that register nothing. Decide from the contract capability matrix, not from what is convenient
to export.

## Phase 13 — Rebuild composition on final owners and lock the result

**Rationale.** Once Core, SDK and consumers are clean, the manifest can load only the Environment and
adapters each host needs, release automation can publish the real public surface, and final gates can
prevent the old facades from returning.

**Data.** The one closed `resources/META-INF/vis/manifest.edn` remains the registration-order and
native-reachability source. Final names and host demand are data:

```clojure
{:register com.blockether.vis.internal.core.environment.sandbox.shim-yaml/register!
 :hosts #{:host/gateway :host/cli}
 :moment :moment/on-demand
 :demand :demand/python-sandbox}
```

Loaded namespaces and contract registrations are deterministic acceptance signals; startup timings
remain diagnostics.

**Acceptance criteria.**

- Contract-owned manifest validation requires `:hosts`, `:moment` and a demand key for on-demand
  entries; remove the bare-symbol entry form.
- TUI, gateway and CLI initialize only entries for their host. Provider/language/shim/MCP demand
  loads exactly once under concurrent first use; gateway health and first TUI frame do not force
  unrelated adapters.
- Remove the hard-coded deferred Python argv predicate after manifest data owns loading; registry
  enumeration commands deliberately force completeness or are declared eager.
- Release jobs build, test and publish the same `VIS_VERSION` as `com.blockether/vis-sdk`, `vis-sdk`
  and `@blockether/vis-sdk`, alongside `vis-contract`; package manifests and locks are generated by
  the version sync command.
- Set post-refactor budgets for Core owners, SDK public exports and server composition roots from the
  measured final tree. Contract capability catalogs pin intentional API; budgets reject accidental
  aggregation rather than legitimate domain code.
- The architecture gates have no debt. Production paths and APIs contain no `foundation`,
  `base-tooling`, architectural `spi`, implementation-owned `spec`, broad `com.blockether.vis.core`
  facade, internal gateway client or duplicated route/event vocabulary.
- Update owning docstrings, the `AGENTS.md` ownership table and release rule, docs catalog, examples,
  package artifacts and the release audit to final package and namespace names; the version-sync
  command owns every manifest and lock it writes, and the release commit's file set matches it. Do not
  add migration aliases for removed APIs.
- Finish each slice with its smallest tests and the plan with relevant full JVM tests, `lint_code`,
  `format_code`, native reachability/tests, Python package tests, JavaScript SDK tests, and Companion
  lint, Storybook tests and build.
- Commit and push each independently green vertical slice; update this plan's state in the same
  commit as the work it records.

**Unknowns.** Registry enumeration (`doctor`, provider listing, slash catalog, `apropos`/`doc`) may
need every entry. Decide each command's eager/complete semantics before making a contributor
on-demand; a capability that appears only after unrelated use is not acceptable.

## State of the plan

**IN PROGRESS** — Phases 1 and 2 are complete; Phase 3 is active. Canonical wire behavior lives in
the independently loadable `com.blockether.vis.contract.wire`, while gateway protocol data is sourced
from `gateway.json` and validated by `schema/gateway.json`.

Phase 3 now uses JSON Schema as its only executable contract representation. `contract.document`
rejects values outside the JSON domain and validates each source document with Skjema. JSON documents
and schemas now cover gateway, View, content, configuration, Python host operations, toggles, provider
limits, language surfaces and test-runner results. The superseded source formats, host inventory and
language-specific validation declarations were removed rather than forwarded. Engine-facing namespaces derive
keyword views from validated JSON or keep genuinely private runtime checks as local predicates.

The generated aggregate has been removed. The Python wheel packages the canonical JSON documents and
schemas directly, while its module assembles the existing convenience view in memory. Existing View,
Human Input, Activity, cancellation and cross-session permit suites preserve behavior while the remaining
contract areas and SDK consumers migrate.

Work already available as foundations:

- `vis-contract` exists as an independently testable Clojure/Python artifact with host documents.
- Python's exact published `vis` module is already host-injected and runnable outside the engine,
  proving the SDK/host-adapter model in one language.
- The Companion's private client and the Clojure gateway client provide characterized behavior to
  extract rather than redesign.
- Gateway startup deferral and first-frame work prove selective loading is feasible.
- The obsolete search extension and its entire `extensions/common` category have been removed.
- Issue #161 regressions pin stale-context retirement, abandoned-worker capacity reclamation and
  cross-session cancellation isolation.
- JSON Schema conformance tests enforce document/schema parity, JSON-domain values and Skjema loading.
- `gateway_test` replaces route/event literals as the named fixture for the server, TUI/Companion
  View mirror and future Clojure, JavaScript and Python SDK conformance tests.

The prior plan's contract, wire, View, Activity, gateway and loading work remains in Phases 2-3 and
5-13. Its `internal.base-tooling` destination is rejected. A separately published SDK is no longer out
of scope: Clojure, JavaScript and Python SDK artifacts are Phase 4, clients are Phase 9, and
first-party SDK-only consumption is Phases 10-12.

TODO, in order: finish 2 canonical wire/gateway contract · 3 all executable contracts · 4 three
SDK artifacts · 5 Core Environment and Foundation deletion · 6 Core View · 7 Core execution and Tool
Activity · 8 gateway state owners · 9 internal server/public SDK clients · 11 Companion transport on
the JavaScript SDK · 12 Companion View, extensions and facade deletion · 13 manifest, release,
budgets and final gates. Phase 10 is implemented as the standalone `apps/vis-tui` gateway client,
with an independent native artifact; it intentionally no longer targets an in-process Clojure SDK.
