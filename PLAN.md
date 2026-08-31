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
  `internal/view.clj:1-15,1720-1950,2382` combines Human Input and Live View lifecycle, secrets,
  materialization, storage, wire and Python entry points in 2 672 lines. The same primitive therefore
  has multiple public entrances and names exposing implementation modes.
- The TUI declares a dependency on the whole engine at
  `extensions/channels/vis-channel-tui/deps.edn:10-12`. Its production code imports
  `com.blockether.vis.core` plus engine internals for attachments, formatting, configuration,
  workspace, View, wire, themes and iteration state; for example `channel_tui/chat.clj:8-13` and
  `channel_tui/human_input.clj:26-28`.
- The Companion implements its own client and protocol mirror. `apps/vis-companion/src/lib/gateway.ts`
  is 169 KB, `types.ts` is 33 KB, and `App.tsx:19-20,246-249` constructs that private
  `GatewayClient` directly. View, Human Input, compatibility and subscriptions are additional local
  protocol implementations.
- Python already resembles an SDK but is named as a product half: `packages/vis-agent/pyproject.toml:1-18`
  says its exact `vis/__init__.py` source is both published and executed by the engine. It is released
  as `vis-agent`, imports as `vis`, and only covers extension-host operations; it is not a gateway
  client shared with other consumers.
- `packages/vis-contract/README.md:10-14` currently owns only Clojure/Python host declarations.
  View, gateway, configuration, execution, provider and persistence contracts remain in engine
  implementation files.
- Production Clojure has 19 namespaces and 569 `s/def` forms: 65 are already in
  `packages/vis-contract`, 471 remain under engine `src/`, and 33 describe private TUI transient
  state. The largest hidden contracts are `internal/extension.clj` (147),
  `internal/view/spec.clj` (104), `internal/registry.clj` (44), and
  `internal/config_spec.clj` (43).
- `internal/gateway/wire.clj:56-184` mixes canonical key/JSON conversion with gateway event sets and
  SSE framing. `internal/gateway/protocol.clj:22-78` mixes pure compatibility declarations with
  release/build/runtime discovery. `gateway/state.clj:1-5536`, `server.clj:1-5019` and
  `client.clj:1-2719` each combine multiple owners.
- `src/com/blockether/vis/internal/loop.clj:1-12120` owns block execution, iterations, turns,
  environment lifetime and session caches. `internal/activity.clj:1-337` and its children reduce
  tool observations but sit beside Core even though Activity is execution trace data.
- `resources/META-INF/vis/manifest.edn:2-54` registers 25 namespaces called `foundation`, and
  `src/com/blockether/vis/internal/foundation/` contains 53 implementation files: environment
  discovery, workspace context, editing, shell, shims, Python capture, PTY, MCP, harness,
  introspection and model-facing tool registration. That is the execution environment of Core, not
  a peer layer called Foundation or Base Tooling.
- Production extensions reference 33 distinct `com.blockether.vis.internal.*` namespaces. The
  language packs alone pierce the boundary at `vis-language-clojure/core.clj:18-27`,
  `vis-language-clojure/test_runner.clj:23-25`, `vis-language-python/interpreter.clj:9-11`, and
  `vis-language-python/ruff.clj:25-26`.

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

**Rationale.** Three SDKs and a large namespace move are safe only if CI can distinguish public
consumption from engine implementation. The intended graph must fail before the first extraction,
while a closed debt inventory permits the gate to land against today's violations and only shrink.

**Data.** The allowed production edges are:

```text
contract.*                    → Clojure/JDK/leaf libraries only
internal.core.*               → contract.* + internal Core leaf utilities
internal.gateway.server.*     → contract.* + internal.core.* + server children
Clojure/Python/JavaScript SDK → generated contract + language runtime/transport only
TUI, Companion, extensions    → their language SDK + own code + UI/runtime libraries
```

No consumer imports `com.blockether.vis.core`, `com.blockether.vis.internal.*`,
`com.blockether.vis.contract.*`, raw gateway route strings or another SDK implementation. Tests may
import the production namespace they test; contract conformance tests exercise public artifacts.

**Acceptance criteria.**

- Add one Clojure namespace dependency test and one JavaScript/Python import/wire scan enforcing the
  graph above, initially with a closed exact debt set. A new violation fails; every migration commit
  removes entries until Phase 10 deletes the debt mechanism.
- Pin the current route table, gateway event vocabulary, View open/patch/action/close behavior,
  Human Input validation/secrets, Activity reduction, turn cancellation and cross-session permit
  isolation before moving their owners.
- Inventory direct TUI and extension internal imports, Companion route/header/event literals and
  Python host operations as named migration inputs rather than broad exceptions.
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
| routes, request/response envelopes, journal envelope and event vocabulary | `resources/vis-contract/gateway.edn` |
| generated route/event/key constants | each SDK's generated contract module |
| `sse-frame`, `job-sse-frame` | `internal.gateway.server.transport.sse` |
| bounded diagnostics | the one internal caller or a genuine internal leaf utility |
| release/build identity, checkout inspection, daemon staleness and messages | `internal.gateway.runtime` |

**Acceptance criteria.**

- Core producers, gateway server and all three SDKs use contract-owned canonical wire behavior;
  `internal.gateway.wire` is deleted rather than forwarded.
- Preserve total encoding of non-string keys, NaN/infinities, UUIDs, dates, symbols and keywords,
  plus in-process/JSON round-trip parity through shared fixtures.
- `gateway.edn` declares every method/path, protocol header, request/response envelope, event type,
  replay rule and terminal semantic; server handlers and emitting call sites drift-test against it.
- Routes intended only for administration or one host are marked explicitly rather than omitted.
- SSE byte framing, Ring and concrete HTTP remain implementation; the contract owns semantics only.
- Render deterministic Clojure, Python and JavaScript inputs from the same EDN and pin them
  byte-for-byte.
- `vis-contract` retains zero engine, SDK, filesystem, network, process or daemon dependencies.

**Unknowns.** Which `/v1/admin/*` routes are intentionally public to SDK clients. Classify every live
route in `gateway.edn`; absence from documentation is not a compatibility category.

## Phase 3 — Make `vis-contract` the only home of executable contracts

**Rationale.** The 471 engine `s/def` forms are connection definitions hidden beside
implementations. Moving them before SDK implementation establishes one source for validation,
generation and capability parity.

**Data.** The initial ownership map is:

| current source | target contract |
|---|---|
| `internal/view/spec.clj` | `contract.view` + `resources/vis-contract/view.edn` |
| contract portions of `internal/extension.clj` and `internal/registry.clj` | `contract.extension`, `contract.channel`, `contract.provider`, `contract.persistence` |
| `internal/config_spec.clj` | `contract.config` |
| `internal/provider_limits.clj` | `contract.provider` |
| `internal/loop.clj` envelopes | `contract.execution` |
| `internal/manifest.clj` declarations | `contract.manifest` |
| `internal/content.clj`, theme and toggles | `contract.content`, `contract.theme`, `contract.toggle` |
| `internal/test_contract.clj` and shell log shapes | `contract.test-runner`, `contract.shell` |
| `internal/doc_corpus.clj` | `contract.docs` |
| `internal/foundation/surface_contract.clj` | `contract.surface` |
| editing/hashline inputs and outputs | `contract.editing` |
| TUI `transient/spec.clj` | TUI-local `transient.validation` predicates |

EDN is the source for closed vocabularies/envelopes consumed by multiple languages. Clojure-only
callback contracts may remain executable Clojure specs inside the package; the plan does not invent
a universal schema language.

**Acceptance criteria.**

- Every production `s/def` lives under `packages/vis-contract/src`; a whole-tree test rejects a new
  production `clojure.spec.alpha` require elsewhere.
- Cross-owner namespaces are named `contract.*`; architectural `*.spi`, `*.protocol`, `*.spec`,
  `*_spec` and `*_contract` namespaces disappear. Private validation uses local predicates.
- Extension contributions, slot ids, callbacks, manifest entries, providers, channels, persistence,
  View shapes, environment operations and execution envelopes validate against contract-owned
  definitions.
- `resources/vis-contract/{extension,view,gateway}.edn` and further cross-language documents render
  deterministic inputs for all SDKs with drift tests.
- Runtime registries, atoms, IO, lifecycle, security/path policy and mutation remain outside the
  contract package.
- Replace language-specific host documents with one operation catalog carrying per-language
  availability and idiomatic binding metadata; generated Clojure/Python/JavaScript host surfaces
  must account for every operation.
- Any current spec that proves to validate only a private intermediate becomes a local predicate
  rather than expanding the public contract.

**Unknowns.** Some host operations may be meaningful only in-process or only over the gateway. Mark
that transport/capability explicitly in the operation catalog; do not force false behavioral parity.

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

**Acceptance criteria.**

- Each SDK depends on generated `vis-contract` data and ordinary language libraries only; building
  any SDK with the engine, TUI and Companion absent succeeds.
- Clojure and Python extension modules receive a contract-declared Host object/adapter from the
  engine and have deterministic outside/test hosts. They never resolve engine Vars or import
  internals.
- All three client modules implement connection identity, compatibility negotiation, auth/lease,
  sessions, turns, View events/actions, cancellation, subscriptions/replay and resources from
  `gateway.edn`.
- Clojure production HTTP uses `babashka.http-client`. JavaScript ships framework-free ESM usable in
  browsers, Node and Capacitor plus `.d.ts`; it has no React or Capacitor dependency. Python remains
  usable on the CPython floor supported by GraalPy.
- Move the exact `packages/vis-agent/src/vis` source into the Python SDK and extend it rather than
  creating a hand-synchronized copy. Delete the `vis-agent` distribution/path when all callers move;
  keep the ergonomic `import vis` name as the chosen SDK API, not as an alias package.
- Common golden fixtures exercise protocol negotiation, one session/turn, View replay/action,
  cancellation and resource retrieval against every SDK implementation.
- Add package smoke tests from built artifacts: Clojure with only the SDK dependency, Python from a
  wheel, and JavaScript from the packed npm tarball.
- `VIS_VERSION`, release automation and license audit cover all three artifacts at the same version.

**Unknowns.** None about the public product split: contract and SDK remain separate, and all three
SDK distributions ship. Registry publication occurs through the existing release process after the
packages pass local artifact smoke tests.

## Phase 5 — Make the former Foundation the Core execution Environment

**Rationale.** Editing, shell, workspace context, sandbox shims, MCP and harness are not a sibling
adapter collection. They construct the capabilities in which a Core Turn executes and share that
environment's lifecycle, cancellation and disposal.

**Data.** After Phase 3 extracts contracts, classify the 53 files into this owner:

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

- Move every Core-owned `internal.foundation.*` implementation, test, apropos resource, manifest
  entry and native reachability reference under `internal.core.environment.*`; delete old
  namespaces in the same slices.
- Environment owns creation, workspace context, tool capability assembly, cancellation attachment,
  sandbox handles and disposal. Session/Turn code uses its operations without importing concrete
  tool children.
- Contracts and generated SDK declarations leave the old directory first. Environment contains no
  `s/def`, public facade or transport protocol ownership.
- Language packs replace direct `foundation.environment.languages`, `foundation.editing.parse` and
  `foundation.surface-contract` imports with SDK operations/types or extension-local behavior.
- Rename optional artifacts and namespaces from `vis-foundation-search`/`voice` to
  `vis-search`/`vis-voice`; they remain extensions using the SDK and are not Environment children.
- A tree gate rejects `foundation` and `base-tooling` in production paths, namespace symbols,
  coordinates, manifest entries and active product copy. Historical release prose is not rewritten.
- Lifecycle tests prove a cancelled/condemned Environment cannot be reused, resources are disposed
  once, and one Session's Environment failure cannot affect another.
- JVM and native manifest/reachability tests preserve shipped capabilities and registration order
  apart from deliberate names.

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

- Move `internal.activity`, `activity.event` and `activity.presenter` under
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
  copied outside generated SDK modules.

**Unknowns.** Local daemon discovery/start is meaningful for Clojure and Python desktop clients but
not a browser. The contract capability matrix marks it unavailable in browser JavaScript rather
than pretending parity.

## Phase 10 — Make TUI, Companion and every extension SDK-only consumers

**Rationale.** Publishing SDKs is not success while first-party consumers bypass them. TUI and
Companion should be the conformance applications that make an internal leak or missing SDK operation
impossible to merge.

**Data.** Final allowed dependencies are:

```text
vis-channel-tui          → com.blockether/vis-sdk + Lanterna/UI libraries
vis-companion            → @blockether/vis-sdk + React/Capacitor/UI libraries
Clojure extension packs  → com.blockether/vis-sdk + extension-specific libraries
Python extensions        → vis-sdk (`import vis`)
external clients         → one language SDK
```

`vis-contract` may be installed transitively by an SDK but is not imported directly by these
consumers.

**Acceptance criteria.**

- Change the TUI artifact dependency from the whole engine to `com.blockether/vis-sdk`. Production
  TUI code has zero imports, dynamic resolves or doc references to `com.blockether.vis.core`,
  `com.blockether.vis.internal.*` or direct contract namespaces.
- Replace each TUI internal helper dependency with an SDK operation/type or TUI-owned implementation;
  do not move visible rendering policy into the SDK.
- Add `@blockether/vis-sdk` to the Companion and remove local ownership of gateway transport,
  compatibility, subscriptions, View/Human Input protocol reducers and duplicated wire models.
- A Companion gate rejects raw Vis route strings, protocol headers/event vocabularies and direct
  gateway `fetch`/stream construction outside the SDK. Native platform/relay integration remains app
  code and invokes SDK client operations for gateway work.
- Every Clojure extension imports only `com.blockether.vis.sdk.*`, its own code and ordinary
  libraries. Every Python extension imports `vis`; no extension imports engine internals or contract
  packages directly.
- Move the engine binary entry point out of `com.blockether.vis.core`, migrate all legitimate host
  operations to SDK contracts/adapters or internal owners, then delete `core.clj` and
  `com.blockether.vis.view` without aliases.
- Empty and delete `clojure-host.edn/:internal-debt`; the final architecture gate states the direct
  SDK-only rule.
- Built-artifact end-to-end tests run TUI through the Clojure SDK, Companion through the packed
  JavaScript SDK and a Python extension/client through the wheel against the same gateway fixtures.
- The native binary test loads a Python SDK extension and observes its registered capability through
  an SDK client.

**Unknowns.** Some formatting or file-picker helpers currently shared through engine internals may
be pure UI utilities rather than Vis APIs. They move into the TUI when only TUI uses them; only
cross-consumer semantics earn an SDK operation.

## Phase 11 — Rebuild composition on final owners and lock the result

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
- Update owning docstrings, AGENTS guidance, docs catalog, examples, generated artifacts and release
  audit to final package and namespace names; do not add migration aliases for removed APIs.
- Finish each slice with its smallest tests and the plan with relevant full JVM tests, `lint_code`,
  `format_code`, native reachability/tests, Python package tests, JavaScript SDK tests, and Companion
  lint, Storybook tests and build.
- Commit and push each independently green vertical slice; update this plan's state in the same
  commit as the work it records.

**Unknowns.** Registry enumeration (`doctor`, provider listing, slash catalog, `apropos`/`doc`) may
need every entry. Decide each command's eager/complete semantics before making a contributor
on-demand; a capability that appears only after unrelated use is not acceptable.

## State of the plan

**REQUIRES WORK** — architecture revised; Phase 1 not started. This revision replaces the proposed
`internal.base-tooling` layer with `internal.core.environment` and makes a tri-language Vis SDK a
required boundary rather than deferred work. No production namespace or package moved in the plan
change.

Work already available as foundations:

- `vis-contract` exists as an independently testable Clojure/Python artifact with host documents.
- Python's exact published `vis` module is already host-injected and runnable outside the engine,
  proving the SDK/host-adapter model in one language.
- The Companion's private client and the Clojure gateway client provide characterized behavior to
  extract rather than redesign.
- Gateway startup deferral and first-frame work prove selective loading is feasible.
- Issue #161 regressions pin stale-context retirement, abandoned-worker capacity reclamation and
  cross-session cancellation isolation.

The prior plan's contract, wire, View, Activity, gateway and loading work remains in Phases 2-3 and
5-11. Its `internal.base-tooling` destination is rejected. A separately published SDK is no longer
out of scope: Clojure, JavaScript and Python SDK artifacts are Phase 4, clients are Phase 9, and
first-party SDK-only consumption is Phase 10.

TODO, in order: 1 dependency/behavior gates · 2 canonical wire/gateway contract · 3 all executable
contracts · 4 three SDK artifacts · 5 Core Environment and Foundation deletion · 6 Core View · 7
Core execution and Tool Activity · 8 gateway state owners · 9 internal server/public SDK clients ·
10 SDK-only TUI/Companion/extensions · 11 manifest, release, budgets and final gates.
