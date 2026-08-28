# PLAN — Six seams, three artifacts, one loader

*A seam nobody wrote down is not a boundary; it is a habit.*

## Context

**State before.** Six seams cross this engine. Two are written down. Four are habits.

| # | seam | crosses | state | where it lives today |
|---|---|---|---|---|
| B1 | host ops | extension → engine | frozen | `packages/vis-contract/resources/vis-contract/{python,clojure}-host.edn`, 13 ops, `Host(Protocol)` + `check_host` |
| B2 | contribution surface | extension → registry | free | 29 `s/def :ext/*` forms, `internal/extension.clj:520-772` |
| B3 | slots | extension → another extension | free | 4 keyword ids, no registry |
| B4 | wire | any process → gateway | free | 18 `/v1/*` paths + 7 bare paths, in code only |
| B5 | journal | gateway → any reader | free | `~/.vis/gateway/events/<id>.ndjson`, shape nowhere stated |
| B6 | tool verbs | model → engine | frozen | `:ext.symbol/params` + `extension_test.clj` over every live tool |

- **B2 is 29 keys inside a 143 751-byte file.** Python declares 6 of them — `extension`, `symbol`,
  `slash`, `op_hook`, `network_filter`, `provider` (`packages/vis-agent/src/vis/__init__.py`,
  68 841 B). Nothing enumerates the 29, so nothing notices the missing 23.
- **B3 is four ids in the whole tree**: `:gateway.slot/http-routes` (8 occurrences),
  `:tui.slot/footer-segment` (8), `:tui.slot/header-row` (7), `:tui.slot/commands` (4). The gateway
  reads its own slot at `internal/gateway/server.clj:4073,4087` through
  `extension/channel-contributions-for` (`extension.clj:3035`). There is no registry of ids: a typo
  contributes to nothing and says nothing. `:api.slot/preamble` was in this list one plan ago and
  is gone now — nothing noticed in either direction.
- **B4 has two clients and no document**: `internal/gateway/client.clj` (123 141 B, 2 739 lines —
  daemon lifecycle, lease, auth, proxy) and the companion's TypeScript.
- **B5 is stable in fact and unwritten**: measured over the 12 newest journals, 4 565 events —
  `schema` is `1` in every event; eight envelope keys appear on more than 90 % of them
  (`type schema seq session_id ts _pid _producer _store`), `turn_id` and `iteration` on some; 14
  distinct `type` values. The set of types is journal-dependent, which is exactly why it must be
  enumerated rather than sampled.
- **`fold_session` belongs to B6, not B1.** Its key grammar is parsed at
  `internal/ctx_engine.clj:364`. Put it in the host contract and every foreign host has to know how
  to fold a Vis session before it may be a host at all.
- **The loader knows WHAT, never WHO.** `resources/META-INF/vis/manifest.edn` holds 42 entries —
  22 shims, 11 providers, 9 others — each `{:register …/register!}` with optional
  `:apropos`/`:is-optional`/`:because` (`internal/manifest.clj:24-45`). `initialize!` takes no
  argument (`manifest.clj:182`), has nine call sites, and is idempotent per process
  (`manifest.clj:132`). Cold cost measured by `ff57a3c95` on this machine: **9 439 ms**, of which
  `foundation.core` 4 818, `language-clojure` 2 465, `channel-tui` 769, the shims together 953, the
  11 providers together ~138 — paid identically by the TUI process and by the gateway process. The
  one thing that does know better is `deferred-python-dispatch?` (`internal/main.clj:4146`), a
  literal set of argv prefixes consulted at `main.clj:4530`: the right knowledge in a shape no test
  can read.
- **The build boundary is physical, not stylistic.** Only Python can be added to a built binary:
  GraalPy is compiled in and `.vis/extensions/*.py` is scanned at start and at `/reload`. A native
  library cannot — `reachability-metadata.json` freezes 23 FFM downcall descriptors at build time.
  JVM code cannot — there is no class loading. Every "plug into Vis" story that is not Python
  therefore ends at HTTP, which is what makes B4 and B5 load-bearing rather than documentation.
- **That Python extensibility is pinned by nothing.** `test-native/` is one file,
  `native_binary_test.clj`, with zero occurrences of `python`, `graalpy` or `sandbox`.
- **The core has no floor.** `src/` = 5 126 697 B / 161 files: +381 KB since `6ac932db4` measured
  it, +19 KB in the two days since `ff57a3c95` did. The budget test both plans asked for was never
  written. Of the 23 `com.blockether/*` dependencies, `rift` is required by exactly one namespace
  (`internal/workspace.clj`) and still enters the native image graph.

**Root problem.** The two seams that are written down are the two that were extracted into a
package; the four that are not are the four where the engine still decides by habit — what an
extension may contribute, what a slot id means, what the wire is, what an event is. Underneath
them the loader can only say WHAT initializes, so every capability is a startup tax on every
process, and the only escape hatch is a literal argv set in the engine.

**What we solve.** Every seam becomes a document in `packages/vis-contract`, rendered from EDN and
pinned by a drift test; the manifest gains WHO needs an entrypoint and WHEN; Python reaches every
contribution point Clojure reaches; a third artifact (`vis-sdk`) drives a gateway from outside the
JVM over a wire that is finally written down.

**What we do not solve.** Native-image startup — a separate verdict, `.graalvm-version` locked at
GraalVM CE 25.1.3. Classpath discovery of extensions — `manifest.edn` stays the ONE closed
document. Perceived startup — `c7679bffb` made the TUI paint and accept typing before it talks to a
gateway, and no phase here may regress it.

**Alternatives considered.**

- *One contract document instead of five.* Lost: the readers are disjoint. A team writing an
  extension needs B1–B3 and must never learn the wire; a team driving an agent needs B4–B5 and
  never sees `:ext/*`. One document means every consumer versions against changes that cannot
  affect it.
- *A `defprotocol` facade for `com.blockether.vis.core`.* Lost: 493 public vars, one of them a
  macro, and `defprotocol` carries neither the count nor the macro. The feasible symmetric move is
  the one Python already made — freeze the surface as data.
- *An in-process `libvis` SDK.* Lost: the tree has one entrypoint (`main.clj:4570`) and `start!` is
  internal (`gateway/server.clj:4633`); embedding means a JVM inside the user's process. A client
  over a frozen `/v1` is the same contract in every language and works today.
- *Split `manifest.edn` per host.* Lost: three copies of one ordered dependency list drift, and
  `build.clj` would have to read all of them to union native reachability.
- *Keep `deferred-python-dispatch?` and add one predicate per capability.* Lost: it becomes a
  dispatch table in the engine that no manifest test can see.
- *Make every `register!` lazy and drop eager initialization.* Lost: order in that vector IS
  dependency order; full laziness moves ordering into whoever demands first and multiplies the
  first-load races `275c51a79` had to serialize.
- *Split `foundation.core` before declaring hosts.* Lost: it is the 4 818 ms floor and the hardest
  move; the host split is what proves which families only the gateway wants, so it should choose
  the first family to leave.
- *Do the TypeScript binding now.* Lost for this plan: `vis-language-typescript-bun` left
  `:initialization` in `2d422f84a`, and the documents a TS mirror would drift-test against are
  Phases 4 and 6 here. It is cheap after them and speculative before them.
- *Publish the artifacts first.* Lost: blocked on the publishing-identity decision, and no phase
  here needs a registry.

## Phase 1 — The manifest declares who needs an entrypoint, and when

**Rationale.** Nothing can be deferred while the document says only "these 42, in this order".
Declaring first — every entry at all three hosts, every moment eager — changes no behavior, and
gives Phases 2 and 3 a place to put their answer plus a test that refuses an entry which forgot to
say who needs it.

**Data.** Manifest `:version 2`. The bare-symbol alternative in `::initialization`
(`manifest.clj:38-44`) is removed rather than kept beside the map form.

```clojure
(s/def ::register qualified-var-symbol?)                        ; unchanged
(s/def ::hosts (s/coll-of #{:host/tui :host/gateway :host/cli} :kind set? :min-count 1))
(s/def ::moment #{:moment/eager :moment/on-demand})
(s/def ::demand qualified-keyword?)                             ; what forces an on-demand entry
(s/def ::entry
  (s/and (s/keys :req-un [::register ::hosts ::moment]
                 :opt-un [::apropos ::is-optional ::because ::demand])
         #(or (= :moment/eager (:moment %)) (contains? % :demand))))
```

The closedness check and the `:is-optional`/`:because` pairing stay as they are.

**Acceptance criteria.**

- All 42 entries carry `:hosts #{:host/tui :host/gateway :host/cli}` and `:moment/eager`; order
  unchanged.
- `initialize!` takes a host keyword; each entrypoint runs at most once per process through a delay
  PER ENTRY, so a later on-demand force cannot double-register.
- `build.clj` unions every entry's namespace regardless of host; `native_reachability_test` and the
  built binary stay green.
- `manifest_test` refuses an entry with no `:hosts`, an unknown host, an unknown `:moment`, or
  `:moment/on-demand` without `:demand`.
- No startup number moves. This phase is a declaration.

**Unknowns.** The nine call sites of `initialize!` — does each name its host, or does a default
exist? Current answer: each names it, because a default is how one flat list comes back. Second:
what host does a `--gateway HOST` remote TUI have? Expected `:host/tui` unchanged — remote mode
makes it thinner, not fatter.

## Phase 2 — Each process initializes only its own host

**Rationale.** The measurement says where the tax is. The TUI pays 2 465 ms for a language pack it
never calls, 953 ms for sandbox shims it never evaluates, ~138 ms for providers its own docstring
forbids it to hold (`extensions/channels/vis-channel-tui/…/provider.clj:21-23` — every provider
verb goes through the gateway). The gateway pays 769 ms for a terminal it never draws.

**Data.** The assignment is this phase's content.

| entrypoints | `:hosts` |
|---|---|
| `foundation.core` | tui, gateway, cli |
| `channel-tui.core` | tui |
| 22 `foundation.shim-*`, `foundation.mcp.core`, `foundation.harness.core` | gateway, cli |
| `foundation-search`, `foundation-voice`, `language-clojure`, `language-python`, `persistance-sqlite` | gateway, cli |
| 11 `provider-*` | gateway, cli |

**Acceptance criteria.**

- `initialize-for-dispatch!` (`main.clj:4524`) derives the host from the dispatched command and
  passes it on; `deferred-python-dispatch?` and its argv set are DELETED, the deferral it encoded
  now being `:hosts`/`:moment` data.
- `test/com/blockether/vis/internal/startup_host_test.clj`: in a clean JVM, after `:host/tui`
  initialization `loaded-libs` contains no `…ext.provider-*`, no `…ext.language-clojure…`, no
  `…internal.foundation.shim-…`; after `:host/gateway` it contains no `…ext.channel-tui…`. Loaded
  namespaces, never a stopwatch — the gate must not be timing-flaky.
- Recorded here when measured: TUI-host initialization against the 9 439 ms baseline, gateway-host
  likewise, on the JVM path of this machine.
- First frame still precedes any gateway call; the `screen_test.clj` startup cases stay green.

**Unknowns.** Does anything under `extensions/channels/vis-channel-tui/` reach the provider
registry through the `com.blockether.vis.core` facade rather than the gateway? A grep for
`registered-providers`/`registry/` in that tree found nothing; the facade must be checked before
providers leave the TUI host.

## Phase 3 — Demand, not eagerness, inside the right host

**Rationale.** After the split the gateway still eagerly loads what a session may never touch: 22
shims, 3 language packs, 11 providers. `275c51a79` already proved deferral safe for the largest of
them; this phase states it as data instead of as one argv predicate.

**Data.** `:moment/on-demand` plus the key that forces it — `:demand/provider`, `:demand/language`,
`:demand/python-sandbox`, `:demand/mcp`.

```clojure
{:register com.blockether.vis.ext.provider-anthropic/register!
 :hosts #{:host/gateway :host/cli} :moment :moment/on-demand :demand :demand/provider}
```

**Acceptance criteria.**

- A registry lookup that misses forces every entrypoint carrying that `:demand` exactly once, under
  the first-load serialization introduced by `275c51a79`; concurrent first calls load once.
- `/healthz` answers before any provider or language-pack namespace is loaded; the first turn loads
  only the provider it resolved, asserted through `loaded-libs` as in Phase 2.
- No test asserts a registry is COMPLETE at boot unless its command declared itself eager.

**Unknowns.** `doctor`, `providers list`, the slash catalog and `apropos`/`doc` enumerate
registries — do they force everything and make on-demand a lie? If so those commands stay
`:host/cli` eager and only the daemon gets the lazy path. This must be answered BEFORE any
tool-contributing pack becomes on-demand: a tool that appears only after its pack loaded is a
contract break, not a speedup.

## Phase 4 — The contribution surface and the slots become documents

**Rationale.** B2 and B3 are the seams an extension author actually touches, and both are
invisible: 29 keys inside 143 KB of spec, 4 slot ids with no registry. The failure mode is the
expensive one — a misspelled key or slot id is accepted in silence and shows up as a missing
feature, never as an error.

**Data.** Two documents beside the host contract, EDN as source, JSON rendered and pinned
byte-for-byte, the way `contract.json` already is.

```clojure
;; packages/vis-contract/resources/vis-contract/extension-points.edn
{:version 1
 :points [{:point/key         :ext/symbols
           :point/kind        :contribution          ; :contribution | :callback | :metadata
           :point/cardinality :many
           :point/entry       {:req [:ext.symbol/symbol :ext.symbol/fn
                                     :ext.symbol/doc :ext.symbol/arglists]
                               :opt [:ext.symbol/params :ext.symbol/tag]}
           :point/languages   #{:clojure :python}
           :point/declarator  {:clojure ":ext/symbols" :python "vis.symbol(...)"}
           :point/moment      #{:moment/eager :moment/on-demand}
           :point/gap         nil}]}                 ; a Clojure-only point MUST state why

;; packages/vis-contract/resources/vis-contract/slots.edn
{:version 1
 :slots [{:slot/id :gateway.slot/http-routes
          :slot/owner :gateway :slot/arity 1
          :slot/receives {} :slot/returns {}}]}
```

**Acceptance criteria.**

- The points document is exactly the engine's `:ext/*` keys — a drift test reads
  `internal/extension.clj` and fails on either side of the difference.
- An `:ext/*` key not in the document is REFUSED at registration, naming the key; an
  `:ext/channel-contributions` entry under a slot id not in `slots.edn` is refused, naming the slot.
- Every point without `:python` in `:point/languages` carries a `:point/gap` reason; a new
  Clojure-only point with no reason fails the suite.
- A point whose contributor is `:moment/on-demand` must be enumerable without loading it, or its
  entry is declared eager instead.

**Unknowns.** Can `:ext/channel-contributions` payloads be described per slot in a shape both the
TUI and the gateway accept, or does each slot need its own entry schema? Decide from the four live
slots before inventing a fifth.

## Phase 5 — Python reaches every point, and the binary proves it

**Rationale.** Parity is the test of whether Phase 4 produced a contract or a description of a
Clojure habit: 6 of 29 points reachable from Python means the document would ship with 23 excuses.
And the claim that carries this whole plan — a built binary is extensible in Python — is today
asserted by no test at all.

**Data.** One declarator per data-carrying point, validating its argument against the published
document before the host op is called; no new wire.

**Acceptance criteria.**

- Every point with `:point/languages` containing `:python` has a declarator; every point without it
  has a `:point/gap` reason, and the count of gaps only goes down.
- The shipped example extension adds, from Python, a `:gateway.slot/http-routes` route and a
  `:tui.slot/footer-segment` row, and both appear in a running engine.
- `test-native/` gains the case it never had: that example `.py` beside the built binary, the
  binary started, the tool and the route present.

**Unknowns.** Can a Python-contributed gateway route be served without a JVM callback per request,
or does the route handler have to marshal through the host op boundary each time? Measure before
declaring the slot open to Python.

## Phase 6 — Freeze the process boundary: wire and journal

**Rationale.** Nothing but Python can be added to a built binary, so every other integration is
HTTP plus the journal. Both are stable in practice — `schema` has been `1` for every one of the
4 565 measured events — and neither is written down, so no second implementation can exist without
reading the engine's source.

**Data.** `wire.edn` — one entry per route with method, path, auth mode, request and response
shape, and the version it appeared in. `events.edn` — the envelope (8 always-present keys, plus
`turn_id`/`iteration`) and one entry per `type` with its payload. `NullHost` and `RecordingHost`
join `Host` in `vis_contract`, still with zero dependencies.

**Acceptance criteria.**

- `wire.edn` is drift-tested against the server's own route table, not against a hand-written list;
  a route added in code and missing from the document fails the suite.
- `events.edn` enumerates every `type` the engine emits, checked the same way against the emitting
  call sites.
- `NullHost` satisfies `check_host`, touches no disk and starts no process; a foreign test suite
  can exercise extension code with it.
- `internal/gateway/client.clj` stays the canonical Clojure client and is not rewritten.

**Unknowns.** Which paths stay outside the freeze — `/v1/admin/*` is the obvious candidate, and
"unfrozen" must be a declared property in `wire.edn`, not an omission.

## Phase 7 — `vis-sdk`: drive a gateway from outside the JVM

**Rationale.** "Start an agent from Python" is a client over Phase 6's wire plus a daemon
lifecycle. Whoever wants that does not want the sandbox module, and whoever writes an extension
does not want an HTTP client — so it is a third artifact, depending on `vis-contract` and never on
`vis-agent`.

**Data.** No new wire. Lifecycle only: find a running daemon, start `visgw` if there is none, wait
for `/healthz`, then speak `/v1`.

**Acceptance criteria.**

- `vis-sdk` depends on `vis-contract` alone; importing it pulls in no sandbox module.
- CI starts a gateway, drives one session end-to-end through the SDK, and reads the transcript back
  through the journal document.
- `scripts/version.mjs` stamps the third package, as it already stamps the two.

**Unknowns.** Daemon discovery is engine-private today. Does the SDK get a documented discovery
path in Phase 6, or does it spawn its own gateway and read the address from that process only?

## Phase 8 — Give the floor a budget

**Rationale.** `src/` grew 381 KB since a plan last measured it and no test could notice. Phases
2–3 make `foundation.core` the whole remaining cost of every process, so the floor is worth
watching before the next 381 KB.

**Data.** None. Code moves; no persisted, wire or mirrored shape changes.

**Acceptance criteria.**

- `test/com/blockether/vis/internal/core_budget_test.clj` caps `src/` total bytes and
  `com.blockether.vis.core` public-var count at today's numbers; growth fails the suite, and each
  commit that moves a family out lowers the cap.
- The first family to leave is one the host split proved only the gateway wants; it lands under
  `extensions/common/vis-foundation-*` with `:ext.engine/builtin? true`, the way
  `vis-foundation-search` already does, and gets its own manifest entry.
- `rift`'s single call site (`internal/workspace.clj`) moves behind an extension boundary or the
  dependency leaves the root `deps.edn`; the native image graph loses that edge.
- One family per commit; the suite passes unchanged at every step.

**Unknowns.** Which family first — by size and independence the candidates are
`internal/env_python.clj`, `internal/foundation/editing/core.clj` (265 KB) and attachments. Decide
with the budget test in front of us.

## State of the plan

**REQUIRES WORK** — Phase 1 not started. This plan replaces two plans and keeps both: "The manifest
says who loads what, and when" (`ff57a3c95`) is Phases 1–3, and "Make every Vis capability an
extension declared by one cross-language contract" (`6ac932db4`) is Phases 4–5. Neither had begun.

Built on, and done:

- `93e379fa0`, `4cdcae1a4`, `137d00650` — the host seam as data, `vis-contract` as its own artifact
  in both registries, `typing.Protocol` + `check_host`, and the Python host as an object.
- `275c51a79` — the gateway listens before Python loads; the TUI never loads local GraalPy. The
  first proof that deferral is safe.
- `2d422f84a` — the disabled TypeScript/Bun pack left `:initialization`; the manifest gained its
  regression test.
- `c7679bffb` — the TUI paints and accepts typing before any gateway call, so the remaining wait is
  work rather than a blank screen.

TODO, in order: 1 manifest v2 · 2 per-host initialization · 3 on-demand entrypoints · 4
`extension-points.edn` + `slots.edn` · 5 Python parity and the native extensibility test · 6
`wire.edn` + `events.edn` + the mock hosts · 7 `vis-sdk` · 8 core budget.

Deliberately not on this plan: the TypeScript binding (cheap after Phase 6, speculative before it),
publishing to any registry (waiting on the identity decision, not on code), and `fold_session` in
the host contract (it is a tool verb, `ctx_engine.clj:364`).
