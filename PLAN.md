# PLAN — One contract, one Core, boring adapters

*If a boundary is real, it has one owner, one direction and one name.*

## Context

**State before.** Vis has the right concepts, but their ownership is encoded in call sites rather
than in the tree.

- `src/com/blockether/vis/core.clj:792-815` publicly exports both Human Input and Live View by
  resolving functions from `internal.view`; `src/com/blockether/vis/view.clj:112-340` separately
  owns the builders. View is therefore a Core primitive with two public entrances and lifecycle
  names that expose an implementation mode.
- `src/com/blockether/vis/internal/view.clj:1-15,1720-1950,2382` combines parsing, normalization,
  secrets, blocking response policy, live lifecycle, storage, wire conversion and Python entry
  points in 2 672 lines. Its 104 executable schemas live separately in
  `internal/view/spec.clj:1-1202` even though channels and the Companion consume the same shapes.
- `src/com/blockether/vis/internal/activity.clj:1-337` plus its `event` and `presenter` children
  reduce tool observations attached to execution, but the top-level name makes Activity look like
  a peer of Core and View. Activity is execution trace data; it has no user-action lifecycle.
- `src/com/blockether/vis/internal/loop.clj:1-12120` owns block execution, iterations, turns,
  environment lifetime and session caches. `internal/gateway/state.clj:1-5536` owns journal fan-out,
  sessions, projects, read models, queueing, permits, watchdogs, cancellation and workers.
- `internal/gateway/server.clj:1-5019` and `internal/gateway/client.clj:1-2719` are legitimately on
  opposite sides of HTTP/SSE, but each is a monolith. The process boundary should remain; the
  responsibility pile-up should not.
- `internal/gateway/wire.clj:56-184` mixes canonical key/JSON conversion with gateway event sets and
  SSE framing. `internal/gateway/protocol.clj:22-78` mixes pure compatibility declarations with
  release/build/runtime discovery. About thirty non-gateway namespaces consequently require a
  namespace named `gateway.wire` merely to encode data.
- Production Clojure has 19 namespaces and 569 `s/def` forms: 65 are already in
  `packages/vis-contract`, 471 remain under engine `src/`, and 33 describe private TUI transient
  state. The largest hidden contracts are `internal/extension.clj` (147),
  `internal/view/spec.clj` (104), `internal/registry.clj` (44), and
  `internal/config_spec.clj` (43).
- `packages/vis-contract/README.md:10-14` currently calls itself the extension host contract, while
  View, gateway, configuration, execution, provider and persistence contracts remain in the
  engine. The package already carries `charred`, so canonical wire encoding needs no new
  dependency.
- `resources/META-INF/vis/manifest.edn:2-54` registers 25 namespaces called `foundation`, and
  `src/com/blockether/vis/internal/foundation/` contains 53 implementation files. The same word is
  also used by the optional `vis-foundation-search` and `vis-foundation-voice` artifacts, so it
  currently means both internal built-in tooling and independent extensions.
- Production extension source contains references to 33 distinct `com.blockether.vis.internal.*`
  namespaces. In particular, the language packs reach through the host boundary at
  `vis-language-clojure/core.clj:18-27`, `vis-language-clojure/test_runner.clj:23-25`,
  `vis-language-python/interpreter.clj:9-11`, and `vis-language-python/ruff.clj:25-26`.

**Root problem.** The tree does not express the dependency graph. Declarations live beside their
current implementation, Core imports adapters, extensions import internals, and the word
`foundation` pretends that one collection of built-in tools is an architectural layer. This makes
large files the accidental integration boundary and allows a change in one session, adapter or
language mirror to affect unrelated behavior.

**Target.** There are four roles and one dependency direction (arrows mean “may depend on”):

```text
extensions/*   internal.base-tooling   internal.gateway   TUI / Companion
      └───────────────────────┬───────────────────────────────┘
                              ▼
          com.blockether.vis.core + internal.core
                              ▼
                 packages/vis-contract
```

Sibling adapters do not import one another. The Companion reaches Core through the gateway
boundary and generated contract mirror; Clojure extensions reach it through the public facade.

`contract` owns every shape, callback and vocabulary crossing an owner, process or language.
`clojure.spec` is an executable contract mechanism and is used in production only inside
`packages/vis-contract`. A local implementation invariant is not promoted into a system contract;
it uses an ordinary predicate or a namespace named `validation`/`state`.

Core owns Session, Turn, View and execution. Human Input is a View with a response contract. Tool
Activity is execution trace data, not a View. Gateway transports Core commands/events and owns its
daemon resources; it does not define Core semantics. Client and server stay separate adapters of
one gateway contract.

`Foundation` is deleted as a concept. The exact replacement for its remaining built-in tool and
sandbox implementation is:

```text
path:      src/com/blockether/vis/internal/base_tooling/
namespace com.blockether.vis.internal.base-tooling.*
```

`base-tooling` is not an artifact, public API, contract layer or synonym for Core. Before the move,
anything in the old directory that is actually a contract or Core environment behavior leaves for
its real owner. Optional search and voice remain extensions and become `vis-search`/`vis-voice`;
they do not move under `base-tooling`.

**Alternatives considered.**

- *Rename `vis-contract` to `vis-sdk`.* Lost: an SDK performs IO, lifecycle, retries and convenience
  operations; this package must remain a dependency leaf containing declarations, codecs and pure
  validation. A future external SDK may depend on it after the gateway contract is stable.
- *Use both `contract` and `spi`.* Lost: two names recreate the same ambiguity. Every boundary is a
  contract, including host callbacks, channel/provider operations and persistence ports; no
  `*.spi` namespace is introduced.
- *Keep specs beside implementations and publish generated copies.* Lost: implementation remains
  the de facto source, generators acquire reverse dependencies, and Clojure/Python/TypeScript can
  drift before generation. The source belongs in `vis-contract`.
- *Move all current `foundation` files mechanically to `base-tooling`.* Lost: that would merely
  rename the junk drawer. Cross-owner schemas move to `contract.*`, Core environment behavior to
  `internal.core.*`, and only built-in model tooling/sandbox implementation becomes base tooling.
- *Make Foundation an extension artifact.* Lost: shell, editing, shims, harness and sandbox support
  are shipped host implementation, while search and voice already prove what an optional extension
  looks like. Calling both the same layer hides the distinction.
- *Merge gateway client and server.* Lost: HTTP/SSE is a real process boundary. They share route,
  envelope and event declarations, never executable transport code.
- *Put Activity into View because both render rows.* Lost: presentation vocabulary may be shared,
  but Activity is a reducer over execution events and accepts no actions; View has lifecycle,
  patches and optional responses.
- *Preserve old namespaces as aliases during the move.* Lost: compatibility facades create two
  owners and this repository deliberately removes obsolete paths. Each vertical slice moves its
  callers and deletes the old namespace in the same commit.
- *Split files by a line-count limit.* Lost: file size is evidence, not architecture. A namespace
  splits at a state owner, state machine or adapter family, and dependency tests prevent the pieces
  from growing back together.
- *Rewrite behavior while relocating it.* Lost: this is a boundary refactor. Existing wire,
  cancellation, View, persistence and rendering behavior is characterized first; semantic changes
  require their own failing test and commit.

## Phase 1 — Make the dependency direction executable

**Rationale.** Moving files before pinning behavior and imports would turn every regression into an
archaeology exercise. The intended graph must fail in CI before the first namespace moves, while a
closed debt inventory lets the gate land against the current tree and only shrink.

**Data.** The allowed production edges are:

```text
contract.*              → Clojure/JDK/leaf libraries only
internal.core.*         → contract.* + internal leaf utilities
com.blockether.vis.core → contract.* + internal.core.*
internal.base-tooling.* → contract.* + internal.core.* + base-tooling children
internal.gateway.*      → contract.* + internal.core.* + gateway children
extensions/*            → com.blockether.vis.core + contract.* + their own namespaces
```

Tests may depend on the production namespace they exercise. No production rule is inferred from a
folder name alone; the namespace require graph is the evidence.

**Acceptance criteria.**

- Add one architecture test that reads every production `ns` form and enforces the graph above,
  including a closed, exact list of current violations; a new violation fails and every migration
  commit removes entries until the list is empty.
- Pin the current route table, gateway event vocabulary, View open/patch/action/close behavior,
  Human Input validation/secrets, Activity reduction, turn cancellation and cross-session permit
  isolation with existing suite tests before moving their owners.
- Record current namespace/byte/public-var measurements as diagnostics, not as arbitrary design
  limits; final budgets are set only after the new owners exist.
- Require every subsequent phase to land as small vertical slices with its production namespace,
  callers and tests moved together; no alias namespace, fallback require or migration layer is
  accepted.
- Keep `packages/vis-contract` loadable and testable independently throughout the plan.

**Unknowns.** Whether the dependency gate should consume clj-kondo analysis or parse `ns` forms
itself. Choose the smaller deterministic implementation that runs in the normal JVM suite and
reports the exact offending edge; do not add a second graph tool.

## Phase 2 — Put canonical wire and the gateway protocol in `vis-contract`

**Rationale.** Canonical encoding is the lowest shared dependency, but its current gateway name
pulls Core, View, Activity and providers toward an adapter. Splitting pure protocol declarations
from transport/runtime code creates the leaf the rest of the refactor can depend on.

**Data.** Move by responsibility, not by current file:

| current symbol/family | target owner |
|---|---|
| `wire-key`, `engine-key`, `->wire`, `->engine`, `canonical`, `json-str`, `parse-json` | `com.blockether.vis.contract.wire` |
| terminal/queue event sets, protocol version, header names, pure compatibility verdict | `com.blockether.vis.contract.gateway` |
| routes, request/response envelopes, journal envelope and event vocabulary | `resources/vis-contract/gateway.edn` read by `contract.gateway` |
| `sse-frame`, `job-sse-frame` | `internal.gateway.transport.sse` |
| bounded printing/pretty diagnostics | the one internal caller or `internal.util` when genuinely shared |
| release/build identity, checkout inspection, daemon staleness and user messages | `internal.gateway.runtime` |

**Acceptance criteria.**

- Client, server, Core event producers, View, Activity, providers and persistence all use
  `contract.wire`; `internal.gateway.wire` is deleted, not retained as a forwarding namespace.
- `contract.wire` preserves total encoding of non-string keys, NaN/infinities, UUIDs, dates,
  symbols and keywords, plus in-process/JSON round-trip parity.
- `gateway.edn` declares every supported method/path, protocol header, event type and terminal
  semantic; server routes and emitting call sites are drift-tested against that declaration.
- Administrative or intentionally private routes are marked as such in the document rather than
  omitted silently.
- SSE byte framing and HTTP concerns remain under `internal.gateway.transport`; the contract owns
  semantics, never Ring responses or client calls.
- `packages/vis-contract` still has no dependency on any `com.blockether.vis.internal.*` namespace
  and performs no filesystem, network, process or daemon lifecycle work.

**Unknowns.** Which `/v1/admin/*` routes are intentionally private. Resolve by classifying each live
route in `gateway.edn`; “not documented” is not a compatibility class.

## Phase 3 — Make `vis-contract` the only home of executable contracts

**Rationale.** The current 471 engine `s/def` forms are executable connection definitions hidden
beside implementations. Moving them establishes one source for Clojure validation and gives the
cross-language mirrors stable input before Core and adapters are split.

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
| editing/hashline input/output shapes | `contract.editing` |
| TUI `transient/spec.clj` | local `transient.validation` predicates, not a Vis contract |

EDN is the source for closed vocabularies and envelopes consumed by more than one language.
Clojure-only callback contracts may be expressed directly as Clojure specs in the same package;
this plan does not invent a universal schema language.

**Acceptance criteria.**

- Every production `s/def` is under `packages/vis-contract/src`; a whole-tree test rejects a new
  production `clojure.spec.alpha` require elsewhere.
- Cross-owner namespaces are named `contract.*`; architectural `*.spi`, `*.protocol`, `*.spec`,
  `*_spec` and `*_contract` namespaces are removed. Local implementation validation is named for
  its owner and does not use `s/def`.
- Extension registration, contribution points, slot ids, callbacks, manifest entries, provider and
  channel descriptors, persistence operations, View shapes and execution envelopes all validate
  against contract-owned definitions.
- `resources/vis-contract/{extension,view,gateway}.edn` and any other cross-language document render
  deterministic JSON/TypeScript inputs and have byte-for-byte drift tests.
- Runtime registries, atoms, IO, lifecycle, security/path policy and mutation remain outside the
  contract package.
- `clojure-host.edn` lists only the intended `com.blockether.vis.core` facade and contract
  namespaces; its temporary internal-debt set can only shrink and is empty by Phase 9.

**Unknowns.** Some current specs may prove to validate only a private intermediate value. During
migration, classify from callers: if it never crosses an owner, replace it with a local predicate
instead of expanding the public contract.

## Phase 4 — Delete Foundation; keep only internal base tooling

**Rationale.** `Foundation` currently names both 53 internal implementation files and two optional
extensions. After Phase 3 extracts declarations, the remaining implementation can receive a plain,
accurate name without promoting it into a layer.

**Data.** The closed classification is:

| current family | target |
|---|---|
| `surface_contract` and cross-owner language/editing shapes | `contract.surface`, `contract.language`, `contract.editing` |
| session/turn environment behavior used by Core | `internal.core.environment` |
| editing tools, shell, introspection, rewind, doctor, workspace/session tool verbs | `internal.base-tooling.*` |
| sandbox shims, Python capture, PTY, MCP and harness implementation | `internal.base-tooling.*` |
| implementation used only by one channel/extension | that channel/extension, not base tooling |
| `vis-foundation-search`, `ext.foundation-search` | `vis-search`, `ext.search` |
| `vis-foundation-voice`, `ext.foundation-voice` | `vis-voice`, `ext.voice` |

The filesystem spelling is `internal/base_tooling`; the Clojure namespace spelling is
`internal.base-tooling`. There is no `vis-base-tooling` artifact.

**Acceptance criteria.**

- Move each remaining built-in tool/sandbox namespace, its tests, apropos resources, manifest entry
  and native reachability metadata to `internal.base-tooling.*`; delete the old namespace in the
  same commit.
- Move Core environment behavior and contracts out before the mechanical rename so
  `base-tooling` contains no Session/Turn/View ownership and no contract declarations.
- Replace the language-pack reaches into `foundation.environment.languages`,
  `foundation.editing.parse` and `foundation.surface-contract` with contract data, a Core operation
  or an extension-local helper according to the actual owner; no extension imports base tooling.
- Rename both optional artifacts, directories, coordinates, namespaces, extension ids, manifest
  entries, settings copy and tests to `vis-search`/`vis-voice` with no compatibility aliases.
- A tree gate rejects `foundation` in production paths, namespace symbols, dependency coordinates,
  manifest registrations and active product copy. Historical release prose need not be rewritten.
- JVM and native manifest/reachability tests prove that registration order and shipped capability
  remain unchanged apart from the deliberate names.

**Unknowns.** None about the layer name: it is `internal.base-tooling`. For a file with mixed
callers, ownership is decided before moving it; base tooling is never used as the default drawer.

## Phase 5 — Make View one Core primitive

**Rationale.** Human Input and Live View already share implementation but expose separate public
lifecycle names. Core should define one semantic document and lifecycle; blocking submission,
secrets and validation are response policy on that View.

**Data.** The target ownership is:

```text
contract.view
  nodes · fields · actions · patches · events · response schema · closed vocabularies

internal.core.view
  normalize · lifecycle · response · secrets · materialize · store

com.blockether.vis.core
  builders · open-view! · patch-view! · view-action! · request! · close-view! · with-view!

internal.gateway.routes.views / client.views
  HTTP/SSE adaptation only
```

`com.blockether.vis.view` and the public `*-live-view!`/`request-human-input!` split are removed;
there is one public facade through `com.blockether.vis.core`.

**Acceptance criteria.**

- One state machine owns `open`, typed `patch`, user `event` and `close`; a response contract adds
  one accepted submit, validation, cancellation and blocking wait without creating a second store
  or transport.
- Secret fields remain redacted from journals, artifacts and diagnostics and are revealed only
  through the existing guarded operation.
- Closing a View, cancelling a response and invoking an execution action such as `interrupt` remain
  distinct contract events.
- Existing semantic builders move behind `com.blockether.vis.core`; the old public View namespace
  and live/human-input aliases are deleted in the same slice.
- TUI and Companion render the same contract document and send the same action envelope. Channel
  tests cover input, dashboard/progress, table grouping, repeated patches, reconnect/replay and
  terminal states.
- “Live” remains the unlabelled default while a View is open; renderers show only meaningful
  exceptional/terminal state such as reconnecting, paused, completed or failed.
- Gateway projection/query code contains no View validation or lifecycle policy after the move.

**Unknowns.** Patch syntax is not redesigned in this refactor. Preserve the current typed
operations unless a characterization test proves that two existing producers already disagree.

## Phase 6 — Put execution and Tool Activity under Core, then split the loop

**Rationale.** Activity belongs to the execution that produced it, and the 12 120-line loop hides
four state machines. Establishing Core owners before gateway decomposition prevents scheduler and
domain behavior from being split along accidental call boundaries.

**Data.** The target families are:

```text
internal/core/block.clj
internal/core/iteration.clj
internal/core/turn.clj
internal/core/environment.clj
internal/core/session.clj
internal/core/execution/tool_activity/{event,reducer}.clj
```

`contract.execution` owns envelopes and persistence/channel ports. Tool Activity may reuse
contract-owned presentation nodes, but it does not depend on `internal.core.view` lifecycle.

**Acceptance criteria.**

- Move `internal.activity`, `activity.event` and `activity.presenter` under
  `internal.core.execution.tool-activity`; collapse the presenter registry if its only remaining
  role is a small reducer detail.
- Preserve replay, bounding, coalescing, resource references and late-failure behavior with the
  existing Activity suite; no View action or View store is introduced.
- Split `internal.loop` by block, iteration, turn, environment and session-cache ownership, moving
  tests with each slice and deleting the old namespace when its last caller leaves.
- Core depends on contract-declared provider/channel/persistence/event ports, never on gateway
  client/server, base tooling or a concrete extension.
- Preserve environment condemnation, hard cancellation, one-shot permit handoff, terminal landing
  and cross-session isolation with the issue #161 regressions green after every relevant slice.
- Distinguish the in-process Core environment/session cache from the daemon session registry; the
  former belongs here and the latter remains for Phase 7.
- Resolve definition order without `declare` and reject dependency cycles in the architecture test.

**Unknowns.** Whether block and iteration warrant separate files after dependencies are cut. Keep
them together if they form one state machine; the acceptance boundary is ownership, not the sample
filenames.

## Phase 7 — Split gateway state by the resource it owns

**Rationale.** A stuck worker affected unrelated sessions because worker capacity, cancellation and
session state met in one process-wide namespace. The fix is tested, but explicit owners are needed
to keep future changes local.

**Data.** Decompose `gateway.state` into cohesive owners:

| owner | state/behavior |
|---|---|
| `gateway.events` | append/replay, subscribers, fan-out and journal sequencing |
| `gateway.turns` | queue, worker lease, permits, watchdogs, cancel/backstop and terminal landing |
| `gateway.sessions` | daemon session/project/workspace registry and current-turn reference |
| `gateway.read-model` | transcript, trace, artifacts and projections |
| `gateway.fleet` | gateway fleet/process membership and health |
| domain integration modules | providers/models, drafts and resources where they own mutation |

A namespace owns its atoms, executor and lifecycle; callers use operations rather than reaching
into those values.

**Acceptance criteria.**

- Move one owner at a time with its tests and direct callers; do not leave a forwarding
  `gateway.state` facade or duplicate registry.
- `gateway.turns` alone owns concurrency permits, worker futures, stall phase, watchdog scheduling,
  cancellation reason and terminal backstop. Permit release remains a one-shot lease even when an
  abandoned worker later returns.
- `gateway.events` alone assigns journal sequence and fans out; projections consume events without
  mutating the journal implementation.
- Session A cancellation, stuck execution or persistence failure cannot cancel, starve or rewrite
  Session B; deterministic multi-session tests exercise each shared resource.
- Server routes call owner operations and never dereference gateway state atoms directly.
- The full gateway state/route suite remains green after every extraction, including replay after
  non-string/NaN payloads and cancellation before provider output.

**Unknowns.** Provider/model catalogs and drafts currently mix durable data with gateway cache.
Classify each by who mutates it before extraction; do not create a generic `services` namespace.

## Phase 8 — Split gateway server and client around the same contract

**Rationale.** Client and server should mirror route families while retaining independent transport
implementation. This makes missing parity visible and keeps daemon discovery, auth and SSE details
out of domain code.

**Data.** The intended adapter families are:

```text
internal.gateway.server                 internal.gateway.client
  daemon/lifecycle                        connection/discovery
  transport.http                          transport.http
  transport.sse                           subscriptions
  routes.system                           system
  routes.sessions                         sessions
  routes.turns                            turns
  routes.views                            views
  routes.integrations                     integrations
  routes.resources                        resources
```

Small `server` and `client` composition roots may assemble these modules. They do not re-export all
functions as a second monolith.

**Acceptance criteria.**

- Extract server route families and matching client operations vertically from
  `gateway.server`/`gateway.client`; each family consumes methods, paths and envelopes from
  `contract.gateway`.
- Keep daemon discovery/start, lease registration, auth headers and canonical
  `babashka.http-client` transport in the Clojure client adapter; no Core namespace performs HTTP.
- Keep Ring, SSE byte framing and process lifecycle in the server adapter; no contract or Core
  namespace depends on Ring.
- A parity test proves that every public contract route has a server handler and, where the contract
  marks it client-callable, a Clojure client operation; exceptions are explicit contract metadata.
- Companion and TUI integration tests exercise the same View/turn/session event envelopes through
  real adapter boundaries.
- Delete the old monolithic namespaces when empty; no client/server merge and no route-string copy
  remains.

**Unknowns.** None about an external SDK in this phase. The canonical Clojure gateway client remains
internal; a separately published SDK is considered only after this contract and lifecycle settle.

## Phase 9 — Remove extension leaks and generate every cross-language mirror

**Rationale.** A contract is proven only when independent consumers stop importing the engine and
stop hand-copying closed vocabularies. Clojure extensions, sandbox Python and the Companion must all
consume the same declarations.

**Data.** Today 89 production extension files reference 33 distinct internal namespaces, while the
Companion hand-declares protocol, View and event vocabulary and Python receives only the host
portion of `contract.json`. The allowed extension dependencies are exactly its own code,
`com.blockether.vis.core` and `com.blockether.vis.contract.*`.

**Acceptance criteria.**

- Replace every production extension import of `com.blockether.vis.internal.*` with a public Core
  operation, a contract callback/data shape, or extension-owned implementation; a whole-tree test
  enforces zero such imports.
- Empty `clojure-host.edn/:internal-debt` and delete the debt mechanism once the architecture gate
  can state the final rule directly.
- Render Python `contract.json` and thin generated TypeScript constants/types from the same
  cross-language EDN documents; generated files are pinned byte-for-byte and never hand-edited.
- Remove handwritten Companion copies of protocol numbers, View/action/event vocabularies and route
  strings. Handwritten TypeScript retains behavior and ergonomic domain helpers, not duplicated
  closed sets.
- Make Python declarators validate every supported extension contribution against
  `contract.extension`; a Clojure-only contribution carries an explicit reason in the document.
- Add the missing built-binary test: a shipped Python extension registers through the rendered
  contract and its tool/contribution is observable through the gateway.
- Keep native reachability derived from the one root manifest and update Python, Companion and JVM
  parity tests in the same slices as each contract document.

**Unknowns.** Generate only closed vocabularies/envelopes that are truly shared. Rich TypeScript
component/view-model types may remain handwritten when generation would encode UI behavior rather
than a wire contract.

## Phase 10 — Rebuild composition on the new boundaries and lock the result

**Rationale.** Once ownership and consumers are clean, the manifest can describe which adapter a
host needs without naming obsolete layers, and final gates can prevent the monoliths and startup tax
from returning.

**Data.** The one closed `resources/META-INF/vis/manifest.edn` remains the source for registration
order and native reachability. It gains explicit host and demand metadata only after namespaces have
their final owners:

```clojure
{:register com.blockether.vis.internal.base-tooling.shim-yaml/register!
 :hosts #{:host/gateway :host/cli}
 :moment :moment/on-demand
 :demand :demand/python-sandbox}
```

The prior startup measurements remain diagnostics; loaded namespaces and contract registration are
the deterministic acceptance signal.

**Acceptance criteria.**

- Contract-owned manifest validation requires `:hosts`, `:moment` and a demand key for on-demand
  entries; the bare-symbol entry form is removed.
- TUI, gateway and CLI initialize only entries declared for their host. Provider/language/shim/MCP
  demand loads exactly once under concurrent first use; health and first TUI frame do not force
  unrelated adapters.
- Remove the hard-coded deferred Python argv predicate after manifest data owns the decision;
  enumeration commands either deliberately force complete registries or are declared eager.
- Set post-refactor budgets for `core.clj` public vars and each composition root based on the final
  measured tree. The contract pins the intentional public surface; budgets reject accidental
  aggregation, not legitimate domain code.
- The architecture gate has no debt, no old namespace/path remains, and production source contains
  no architectural `foundation`, `spi`, implementation-owned `spec`, or duplicate route/event
  vocabulary.
- Update owning docstrings, docs catalog, examples and generated artifacts to the final names; do
  not add migration notes or compatibility aliases for removed APIs.
- Run the smallest suite on every slice and finish with full relevant JVM tests, `lint_code`,
  `format_code`, native reachability/tests for moved entrypoints, and Companion lint, Storybook tests
  and build for generated TypeScript changes.
- Commit and push each independently green vertical slice; the plan state advances in the same
  commit as the work it records.

**Unknowns.** Registry enumeration (`doctor`, provider listing, slash catalog, `apropos`/`doc`) may
need all entries. Decide each command's eager/complete semantics before making a contributing pack
on-demand; a capability that appears only after unrelated use is not acceptable.

## State of the plan

**REQUIRES WORK** — architecture agreed; Phase 1 not started. This rewrite replaces “Six seams,
three artifacts, one loader.” No production namespace was moved by the planning change.

Work already available as foundations:

- `vis-contract` exists as an independently testable Clojure/Python artifact with host documents.
- Gateway startup deferral and first-frame work already prove selective loading is feasible.
- Issue #161 regressions pin stale-context retirement, abandoned-worker capacity reclamation and
  cross-session cancellation isolation.

The unfinished work from the replaced plan is retained where it still belongs: contribution/slot
contracts and Python parity are Phases 3 and 9; wire/events are Phase 2; host/demand manifest loading
is Phase 10; the final Core budget is Phase 10. A separately published `vis-sdk` is deliberately
removed from this plan because it is behavior above the contract, not part of the boundary cleanup.

TODO, in order: 1 dependency/behavior gates · 2 canonical wire and gateway contract · 3 all specs
and connection contracts · 4 `foundation` deletion and `internal.base-tooling` · 5 Core View · 6
Core execution and Tool Activity · 7 gateway state owners · 8 server/client adapters · 9 extension
and cross-language parity · 10 composition, budgets and final deletion gates.
