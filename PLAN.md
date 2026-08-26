# PLAN — The manifest says who loads what, and when

*An entrypoint every process must run is not a contribution — it is a tax on every start.*

## Context

**State before.**

- `resources/META-INF/vis/manifest.edn` is one flat vector of 43 initializer symbols, spec'd as
  `(s/coll-of qualified-var-symbol? :kind vector? :distinct true)`
  (`src/com/blockether/vis/internal/manifest.clj:35`) and run by ONE process-wide `defonce` delay
  (`manifest.clj:83-85`). The data says WHAT initializes and in which order. It does not say who
  needs it, or when — so every process runs all 43.
- Measured cost of that vector, cold `clojure -M -e` on this machine (resolve + invoke, ms):
  `foundation.core` **4818**, `language-clojure` **2465**, `channel-tui` **769**, `shim-nippy`
  **739**, the 23 shims together **953**, the 11 providers together **~138**, `mcp` 66,
  `shim-pil` 73, `foundation-search` 52, everything else under 45 — **9439 ms in total**, paid
  identically by the TUI process and by the gateway process.
- The TUI's own docstring already states the contract the manifest breaks: "The TUI therefore needs
  NO provider extension on its own classpath, holds no credential secret at any moment"
  (`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/provider.clj:21-23`)
  — every provider verb goes through the gateway. The TUI JVM loads all 11 anyway. Symmetrically
  the gateway loads `channel-tui` it will never paint with.
- ONE axis was already carved out, in code rather than in data: `deferred-python-dispatch?`
  (`src/com/blockether/vis/internal/main.clj:4343-4346`) is a literal set of argv prefixes,
  `#{["channels" "tui"] ["gateway" "start"]}`, consulted by `initialize-for-dispatch!`
  (`main.clj:4721-4729`). It works — the gateway now listens before Python loads (`275c51a79`) —
  and it is exactly the knowledge that belongs beside the entrypoint it governs.
- Startup latency is no longer hidden by the UI: since `c7679bffb` the TUI paints and accepts
  typing before it talks to a gateway, so what remains is honest work — two JVMs each executing the
  same 43 entrypoints. Cold measurement after that commit: first frame 13.7 s, gateway listening
  24.7 s, session bound 33.7 s.
- The cross-language contract exists but stops at the host seam:
  `packages/vis-contract/resources/vis-contract/` holds `python-host.edn` and `clojure-host.edn`
  and nothing else. `extension-points.edn` and `slots.edn`, declared by the previous plan
  (`6ac932db4`), were never written; the contribution surface is still 28 `s/def :ext/*` forms
  inside `src/com/blockether/vis/internal/extension.clj`.
- The core grew while nothing watched: that plan measured `src/` at 4,745,821 bytes / 155 files;
  today it is **5,107,383 bytes / 160 files** — +361 KB over 550 commits. Its budget test was
  never written.
- `build.clj:737-750` derives native reachability from this same vector, so the split must keep the
  UNION of entrypoints reachable in the image whatever a process loads at runtime.

**Root problem.** The manifest declares a distribution, not a deployment. One list, three very
different processes: a terminal client that owns pixels, a daemon that owns work, a one-shot CLI
that owns a single answer. Because the list cannot express that, every capability is a startup tax
on every process, and the only escape hatch is a literal argv set in the engine — the same shape of
knowledge, written where no test can read it.

**What we solve.** The manifest gains two keys per entrypoint — WHO needs it (`:hosts`) and WHEN
(`:moment`) — the processes initialize their own host, capabilities that nobody demanded stay
unloaded, and a budget test makes the floor visible so `foundation.core` cannot keep absorbing
everything.

**What we do not solve.** Not native-image startup (a separate verdict, `.graalvm-version` locked).
Not a plugin marketplace or classpath discovery: the manifest stays the ONE closed document.
Not the perceived-startup work — `c7679bffb` did that and this plan must not regress it.

**Alternatives considered.**

- *Keep `deferred-python-dispatch?` and add a second predicate per capability.* Lost: it is one
  literal argv set today; three become a dispatch table in the engine that no manifest test can
  see, and `build.clj` still cannot tell which entrypoints exist for the image.
- *Split `manifest.edn` into `manifest-tui.edn` / `manifest-gateway.edn`.* Lost: three copies of an
  ordered dependency list drift, and the union `build.clj` needs would have to be recomputed by
  reading all of them.
- *Make every register! lazy and drop eager initialization entirely.* Lost: order in that vector IS
  dependency order (`manifest.clj:10-11`); a fully lazy graph moves that ordering into whoever
  demands first, and the first-load races point 1 already had to serialize would multiply.
- *Split `foundation.core` first, before declaring hosts.* Lost: it is the 4.8 s floor and the
  hardest move; the host split is what PROVES which families only the gateway wants, so it should
  choose the first family to leave, not the other way round.
- *Do the previous plan's TypeScript binding (`6ac932db4` Phase 4) now.* Lost for this plan:
  `vis-foundation-bridge` is gone, `vis-language-typescript-bun` was excluded from initialization
  by `2d422f84a`, and the decision it waited on was never answered. It is not on this critical
  path; the contribution documents it needs are Phase 5 here.
- *Publish the contract artifacts first (`6ac932db4` Phase 6).* Lost: still blocked on the
  publishing identity decision, and nothing here depends on a registry.

## Phase 1 — The manifest declares hosts and moments

**Rationale.** Nothing can be deferred or skipped while the only thing the document says is
"initialize these 43, in this order". Declaring first, with every host set to all three, changes no
behavior and gives every later phase a place to put its answer — and a test that can refuse a new
entrypoint that forgot to say who needs it.

**Data.** Manifest `:version 2`. No v1 fallback: this repo removes obsolete paths instead of
carrying them.

```clojure
(s/def ::entrypoint qualified-var-symbol?)                       ; the register! Var, as today
(s/def ::hosts (s/coll-of #{:host/tui :host/gateway :host/cli} :kind set? :min-count 1))
(s/def ::moment #{:moment/eager :moment/on-demand})
(s/def ::demand qualified-keyword?)                              ; what forces an on-demand entry
(s/def ::entry
  (s/and (s/keys :req-un [::entrypoint ::hosts ::moment] :opt-un [::demand])
         #(or (= :moment/eager (:moment %)) (contains? % :demand))))
(s/def ::initialization (s/and (s/coll-of ::entry :kind vector? :distinct true) seq))
```

`:apropos` is untouched and the three-key closedness check (`manifest.clj:37-39`) stays closed.

**Acceptance criteria.**

- `resources/META-INF/vis/manifest.edn` — all 43 entries as maps, order unchanged, in this phase
  every one `:hosts #{:host/tui :host/gateway :host/cli}` and `:moment/eager`.
- `src/com/blockether/vis/internal/manifest.clj` — `initialize!` takes a host keyword and runs the
  entries whose `:hosts` contains it; each entrypoint runs at most once per process (a delay PER
  ENTRY, not one process-wide delay), so a later on-demand force cannot double-register.
- `build.clj:737-750` — `manifest-initialization-namespaces` unions EVERY entry's namespace
  regardless of host; `native_reachability_test` and the built binary stay green.
- `test/com/blockether/vis/internal/manifest_test.clj` — an entry without `:hosts`, with an unknown
  host, with an unknown `:moment`, or `:moment/on-demand` without `:demand`, is refused.
- Full suite green and no startup number moves: this phase is a declaration, not an optimization.

**Unknowns.** Eight call sites invoke `initialize!` today (`extension.clj:3370,3395,3426,3456,3472`,
`loop.clj:10661`, `persistance.clj:283,640`, `main.clj:4334`). Does each name its host, or does a
default exist? Current answer: each names it — a default host is how one flat list comes back.

## Phase 2 — Each process initializes only its own host

**Rationale.** The measurement says where the tax is: the TUI pays 2.5 s for language packs it
never calls, ~0.95 s for sandbox shims it never evaluates, ~0.14 s for providers its own docstring
says it must not hold; the gateway pays 0.77 s for a terminal it never draws.

**Data.** The assignment itself is this phase's content.

| entrypoints | `:hosts` |
|---|---|
| `foundation.core` | tui, gateway, cli |
| `channel-tui.core` | tui |
| 23 `foundation.shim-*`, `mcp.core`, `harness.core` | gateway, cli |
| `foundation-search`, `foundation-voice`, `language-clojure`, `language-python`, `persistance-sqlite` | gateway, cli |
| 11 `provider-*` | gateway, cli |

**Acceptance criteria.**

- `src/com/blockether/vis/internal/main.clj` — `initialize-for-dispatch!` derives the host from the
  dispatched command and passes it to `initialize!`; `deferred-python-dispatch?` and its literal
  argv set are DELETED, the Python deferral it encoded now being `:host`/`:moment` data.
- `test/com/blockether/vis/internal/startup_host_test.clj` — in a clean JVM, after a `:host/tui`
  initialization `loaded-libs` contains no `com.blockether.vis.ext.provider-*`, no
  `…ext.language-clojure…`, no `…internal.foundation.shim-…`; after `:host/gateway` it contains no
  `…ext.channel-tui…`. Loaded namespaces, never a stopwatch — the gate must not be timing-flaky.
- Measured and recorded here: TUI-host initialization ~5.7 s against 9.4 s (−39 %), gateway-host
  ~8.7 s (−8 %), on the JVM path of this machine.
- The point-3 behavior is unchanged: first frame still precedes any gateway call
  (`screen_test.clj` startup-first-frame cases stay green).

**Unknowns.** Does anything under `extensions/channels/vis-channel-tui/` reach the provider
registry through the `com.blockether.vis.core` facade rather than the gateway? A direct grep for
`registered-providers`/`registry/` in that source tree found nothing, but the facade must be
checked before providers leave the TUI host. Second: does `--gateway HOST` remote mode change the
host of any entrypoint? Expected no — it makes the TUI thinner, not fatter.

## Phase 3 — Demand, not eagerness, inside the right host

**Rationale.** After the split the gateway still eagerly loads what a session may never touch: 23
shims, 3 language packs, 11 providers — ~3.6 s of its ~8.7 s. Point 1 proved deferral is safe for
the largest of those (Python); this phase states it as data instead of as one argv predicate.

**Data.** `:moment/on-demand` with the key that forces it:

```clojure
{:entrypoint com.blockether.vis.ext.provider-anthropic/register!
 :hosts #{:host/gateway :host/cli} :moment :moment/on-demand :demand :demand/provider}
```

Demand keys: `:demand/provider`, `:demand/language`, `:demand/python-sandbox`, `:demand/mcp`.

**Acceptance criteria.**

- A registry lookup that misses forces the entrypoints carrying that `:demand` exactly once, under
  the same first-load serialization introduced by `275c51a79`; concurrent first calls load once.
- `/healthz` answers before any provider or language pack namespace is loaded; the first turn loads
  only the provider it resolved (asserted by `loaded-libs`, as in Phase 2).
- `test/com/blockether/vis/internal/startup_host_test.clj` grows the demand cases; no test asserts
  a registry is COMPLETE at boot unless its command declared itself eager.

**Unknowns.** `doctor`, `providers list`, the slash catalog and `apropos`/`doc` enumerate registries
— do they force everything and make on-demand a lie? If so those commands stay `:host/cli` eager
and only the daemon gets the lazy path. Answer BEFORE any tool-symbol-contributing pack becomes
on-demand, because a tool that appears only after its pack loaded is a contract break, not a
speedup.

## Phase 4 — Give the floor a budget and start splitting it

**Rationale.** Every host still pays `foundation.core`'s 4818 ms, and that is where growth lands:
+361 KB of `src/` since the previous plan measured it, with no test able to notice. Phases 2–3 make
the floor the whole cost; a budget makes it visible before the next 361 KB.

**Data.** None. Code moves; no persisted, wire or mirrored shape changes.

**Acceptance criteria.**

- `test/com/blockether/vis/internal/core_budget_test.clj` — ceilings on `src/` total bytes and on
  `com.blockether.vis.core` public-var count, recorded at today's numbers and lowered by each
  commit that moves a family out; growth fails the suite.
- The first family to leave is one the host split already proved only the gateway wants; it lands
  under `extensions/common/vis-foundation-*` with `:ext.engine/builtin? true`, the way
  `vis-foundation-search` already does, and gets its own manifest entry with `:hosts` and
  `:moment`.
- One family per commit; the full suite passes unchanged at every step.

**Unknowns.** Which family first — by size and independence the candidates are
`internal/env_python.clj` (177 KB, already deferred at runtime),
`internal/foundation/editing/core.clj` (265 KB), attachments. Decide with the budget test in front
of us, not before.

## Phase 5 — Declare the contribution surface, carried over from `6ac932db4`

**Rationale.** Unchanged from the previous plan: the only description of what an extension may
contribute is 28 `s/def :ext/*` forms in `src/com/blockether/vis/internal/extension.clj` plus 5
ad-hoc `*.slot/*` keywords, and Python reaches 6 of the 28. What changed is the ORDER and the
reason: startup cost is what hurts today, and Phases 1–3 give the contribution documents a
vocabulary they must agree with — a point contributed by an `:moment/on-demand` entrypoint is not
the same promise as one contributed at boot.

**Data.** `packages/vis-contract/resources/vis-contract/extension-points.edn` and `slots.edn`
exactly as specified in `6ac932db4` Phases 1–2, each point additionally stating whether it may be
contributed lazily.

**Acceptance criteria.**

- As in `6ac932db4` Phases 1–3: the points document is exactly the engine's `:ext/*` keys (drift
  test), an undeclared slot id is refused at registration, and every data-carrying point has a
  Python declarator validating against the published document.
- One added criterion from this plan: a point whose contributor is `:moment/on-demand` must be
  enumerable without loading it, or the entrypoint is declared eager instead.

**Unknowns.** Whether lazy contribution is compatible with `doc()`/`apropos` enumeration at all —
the same question Phase 3 must answer first, here as a property of the declared surface rather than
of one registry.

## State of the plan

**REQUIRES WORK** — Phase 1 not started. This plan replaces "Let the gateway own the list, so a
page is a page", which is DONE.

Done, and what this plan builds on:

- `275c51a79` — the gateway listens before Python loads; the TUI never loads local GraalPy; the
  slash catalog comes from the gateway lazily. The first proof that deferral is safe.
- `2d422f84a` — the disabled TypeScript/Bun pack left `:initialization`; the manifest gained its
  regression test.
- `c7679bffb` — the TUI paints and accepts typing before any gateway call, so the remaining wait is
  real work rather than a blank screen.
- From `6ac932db4`: the host seam as data (`python-host.edn`, `clojure-host.edn`), `vis-contract`
  as its own artifact, `typing.Protocol` + `check_host`.

TODO, in order:

1. Phase 1 — manifest v2 with `:hosts` and `:moment`, behavior identical.
2. Phase 2 — per-host initialization, `deferred-python-dispatch?` deleted, `loaded-libs` gate.
3. Phase 3 — on-demand entrypoints behind `:demand` keys.
4. Phase 4 — core budget test, then one family per commit.
5. Phase 5 — `extension-points.edn` + `slots.edn` + Python parity (carried over).

Deliberately not on this plan: the TypeScript binding and publishing the contract artifacts, both
from `6ac932db4` and both still waiting on a decision, not on code.
