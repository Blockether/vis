# PLAN — Make every Vis capability an extension declared by one cross-language contract

*If the engine can do it and an extension cannot, that is a bug in the contract, not a feature of the engine.*

## Context

**State before.**

- The contract now exists as its own artifact: `packages/vis-contract/` ships to Clojars as
  `com.blockether/vis-contract` and to PyPI as `vis-contract` (commit `4cdcae1a4`). It declares
  exactly ONE seam — the 13 host callbacks
  (`packages/vis-contract/resources/vis-contract/python-host.edn`), now a real
  `typing.Protocol` (`packages/vis-contract/python/src/vis_contract/__init__.py`, commit
  `137d00650`). Everything an extension may CONTRIBUTE is still undeclared.
- The contribution vocabulary is 28 `s/def :ext/*` forms buried in the 147,999-byte implementation
  namespace `src/com/blockether/vis/internal/extension.clj:479-916`: `:ext/engine` (tool symbols),
  `:ext/cli`, `:ext/channels`, `:ext/providers`, `:ext/persistance`, `:ext/attachment-storage`,
  `:ext/sandbox-shims`, `:ext/hooks`, `:ext/op-hooks`, `:ext/slash-commands`,
  `:ext/network-filters`, `:ext/settings`, `:ext/env`, `:ext/theme`, `:ext/doctor-fn`,
  `:ext/channel-contributions`, `:ext/requires` and the rest. No document lists them; the only way
  to learn the surface is to read a 3637-line file.
- Slots — the mechanism that already makes the TUI and the GATEWAY extensible — are ad-hoc
  keywords. The whole tree contains 5 of them:
  `:tui.slot/commands` (10 uses), `:tui.slot/footer-segment` (8), `:gateway.slot/http-routes` (8),
  `:tui.slot/header-row` (7), `:api.slot/preamble` (1)
  (`src/com/blockether/vis/internal/gateway/server.clj:3510`,
  `src/com/blockether/vis/internal/foundation/rewind.clj:1311`,
  `extension.clj:654,3062`). Nothing declares a slot's id, its payload shape or its owner, so a
  typo in a slot key contributes silently to nothing.
- The languages are NOT at parity. A Python extension can declare 6 things —
  `extension`, `symbol`, `slash`, `op_hook`, `network_filter`, `provider`, plus `host_env`
  (`packages/vis-agent/src/vis/__init__.py`, 794 lines) — against the Clojure map's 28. Channels,
  CLI commands, persistance backends, attachment storage, sandbox shims, settings, theme, doctor
  checks and EVERY slot contribution (gateway routes, TUI rows) are Clojure-only. `doc("extending")`
  documents the asymmetry as if it were a design.
- The core is not minimal. `src/` is 155 files and 4,745,821 bytes, led by
  `internal/loop.clj` (537,912), `foundation/editing/core.clj` (265,246),
  `gateway/state.clj` (220,425), `internal/main.clj` (196,530), `gateway/server.clj` (196,334),
  `internal/env_python.clj` (177,299). Against that, all 16 extensions together cover providers (8),
  languages (3), channel/TUI (1), persistance (1), search, voice and bridge — the capabilities that
  were EASY to externalize, not the ones a minimal core would demand.
- Two capabilities already prove externalization works end to end:
  `extensions/common/vis-foundation-search` registers tool symbols with
  `:ext.engine/builtin? true`, and `vis-foundation-bridge` registers a whole alias namespace
  (`.../foundation_bridge/core.clj:1110`).

**Root problem.** Vis is extensible by accident of implementation. The engine and an extension are
written in the same language against the same in-process maps, so every capability the engine kept
for itself is unreachable from anywhere else — and a second language, a second host or a third
ecosystem can only ever have the subset somebody remembered to bridge. There is no artifact that
answers "what can an extension contribute?" without reading the engine.

**What we solve.** One declared document per SEAM — host ops (done), contribution points, slots —
shipped in `vis-contract` to every registry; every language binding generated from and tested
against those documents; the parity itself pinned by a test that fails when Clojure gains a
contribution point Python cannot reach; and the capabilities that need not live in the engine moved
out behind those same documents, so `core` shrinks by measurement rather than by taste.

**What we do not solve.** Not a plugin marketplace or remote/sandboxed extension loading. Not
`fold_session` and the other agent tool VERBS — they are the third surface (parsed in
`src/com/blockether/vis/internal/ctx_engine.clj:421`), not host ops, and a foreign host must never
be required to fold a Vis session. Not a rewrite of `loop.clj`. Not a second schema library:
`clojure.spec.alpha` only.

**Alternatives considered.**

- *A `defprotocol` facade for the whole engine.* Lost: `com.blockether.vis.core` has 493 public
  vars, one of them a macro (measured in the managed REPL). A protocol carries neither.
- *Let each language binding hand-write its own declaration API.* Lost: that is exactly the drift
  the three `__vis_host_*` copies caused before Phase 1 of the previous plan.
- *Keep slots as free keywords and document them in `extending.md`.* Lost: prose cannot fail CI, and
  the 1-use `:api.slot/preamble` shows how a slot dies unnoticed.
- *Move capabilities out of `src/` first, declare later.* Lost: an extension point discovered by
  moving code is shaped by the code that moved; the document has to be able to refuse a shape.
- *Node/TypeScript as the third binding vs. a JVM-free Clojure (Babashka) one.* TypeScript wins if
  we do it: the companion already mirrors Vis contracts in TS, so the drift test has precedent.

## Phase 1 — Declare the contribution surface as data

**Rationale.** Without it "everything is an extension" is unverifiable: nobody can list what an
extension may contribute, and no test can notice that a new `:ext/*` key reached Clojure only.

**Data.**

```clojure
(s/def :point/key qualified-keyword?)            ; :ext/channels
(s/def :point/summary non-blank-string?)
(s/def :point/cardinality #{:point/one :point/many})
(s/def :point/carries #{:carries/data :carries/fn})   ; fn-valued points need a host callback
(s/def :point/languages (s/coll-of #{:lang/clojure :lang/python :lang/typescript}
                                   :kind set? :min-count 1))
(s/def :point/gap non-blank-string?)             ; required when a language is absent: why, or the issue
(s/def :contract/point
  (s/and (s/keys :req [:point/key :point/summary :point/cardinality :point/carries
                       :point/languages]
                 :opt [:point/gap])
         #(or (= (:point/languages %) #{:lang/clojure :lang/python :lang/typescript})
              (contains? % :point/gap))))
(s/def :contract/points (s/coll-of :contract/point :kind vector? :distinct true :min-count 1))
(s/def :contract/extension-points (s/keys :req [:contract/version :contract/points]))
```

**Acceptance criteria.**

- `packages/vis-contract/resources/vis-contract/extension-points.edn` — all 28 points, each with
  cardinality, what it carries and which languages reach it today.
- `packages/vis-contract/src/com/blockether/vis/contract/extension_points.clj` — reads, validates,
  exposes `points`, `point-keys`, `points-for-language`.
- `packages/vis-contract/python/src/vis_contract/__init__.py` — `POINTS` rendered into
  `contract.json`, pinned byte-for-byte like `OPS`.
- `test/com/blockether/vis/contract/extension_points_test.clj` — the document's `:point/key` set is
  EXACTLY the `:ext/*` `s/def`s in `src/com/blockether/vis/internal/extension.clj`; a new key in the
  engine fails until it is declared with its languages or its `:point/gap`.

**Unknowns.** Do `:ext/requires` and `:ext/version` count as contribution points or as manifest
metadata? Current answer: manifest, declared in the document with `:point/carries :carries/data`
so the set stays exactly the engine's.

## Phase 2 — Make slots a declared registry, not a keyword convention

**Rationale.** Slots are the only mechanism by which the TUI and the gateway are already extensible;
undeclared, a slot id typo contributes nothing silently, and no language other than Clojure can
discover that `:gateway.slot/http-routes` exists.

**Data.**

```clojure
(s/def :slot/id qualified-keyword?)              ; :gateway.slot/http-routes
(s/def :slot/host #{:host/tui :host/gateway :host/api})
(s/def :slot/summary non-blank-string?)
(s/def :slot/payload #{:payload/http-route :payload/command :payload/segment
                       :payload/row :payload/text})
(s/def :slot/ordered? boolean?)
(s/def :contract/slot (s/keys :req [:slot/id :slot/host :slot/summary :slot/payload :slot/ordered?]))
(s/def :contract/slots (s/coll-of :contract/slot :kind vector? :distinct true :min-count 1))
```

**Acceptance criteria.**

- `packages/vis-contract/resources/vis-contract/slots.edn` — the 5 existing slots, typed.
- `src/com/blockether/vis/internal/extension.clj` — `:ext/channel-contributions` validates its keys
  against the document and REFUSES an undeclared slot id at registration.
- `src/com/blockether/vis/internal/gateway/server.clj`, `internal/foundation/rewind.clj` — consume
  the declared id, no literal keyword.
- `test/com/blockether/vis/contract/slots_test.clj` — every `*.slot/*` keyword occurring anywhere in
  `src/` and `extensions/` is declared; an extension contributing to `:tui.slot/typo` is refused.

**Unknowns.** Does refusing an unknown slot break any extension in flight? None found: the 5 ids are
all engine-side.

## Phase 3 — Close the Python parity gap against the document

**Rationale.** Today a Python author cannot add a gateway route, a TUI row, a CLI command, a
settings entry or a persistance backend. Until they can, "same things from both languages" is
false and Phase 4 has nothing to copy.

**Data.** None. Phases 1 and 2 declare the shapes; this phase implements the bindings against them.

**Acceptance criteria.**

- `packages/vis-agent/src/vis/__init__.py` — one declarator per data-carrying point
  (`vis.contribute(slot, …)`, `vis.channel`, `vis.cli`, `vis.setting`, `vis.theme`,
  `vis.persistance`, `vis.attachment_storage`, `vis.doctor`), each validating against
  `vis_contract.POINTS`/`SLOTS` and refusing an undeclared key by name.
- `src/com/blockether/vis/internal/python_extensions.clj` — collects the new declarations into the
  same `:ext/*` map a Clojure extension returns; no second registration path.
- `resources/examples/python-extensions/` — one example that adds a gateway route and a TUI footer
  segment from Python.
- `test/com/blockether/vis/internal/python_extensions_test.clj` — that example registers and its
  route answers; `packages/vis-agent/tests/` — every declarator refuses an unknown point.
- `test/com/blockether/vis/contract/extension_points_test.clj` — after this phase every point
  carrying DATA lists `:lang/python`; the remaining `:point/gap`s are exactly the fn-valued ones.

**Unknowns.** Can a fn-carrying point (`:ext/doctor-fn`, `:ext/activation-fn`) be reached from
Python at all, or does it need a host op to call back in? Answer during Phase 3 by trying
`:ext/doctor-fn` first — it is the smallest.

## Phase 4 — Prove the contract with a third language

**Rationale.** Two bindings written by the same people in the same repo can share a hidden Clojure
assumption. A third, written only from the published documents, is the test of whether the contract
is a contract.

**Data.** None. The documents are already declared; this phase only reads them.

**Acceptance criteria.**

- `packages/vis-contract/typescript/` — npm package `@blockether/vis-contract`: `contract.json`,
  `slots.json`, `extension-points.json` and generated types, built from the SAME EDN by
  `packages/vis-contract/src/com/blockether/vis/contract/typescript_host.clj`.
- `test/com/blockether/vis/contract/typescript_host_test.clj` — the emitted TS is byte-identical to
  the committed file (the drift test pattern of `human_input_cross_channel_test.clj`).
- One TypeScript extension under `extensions/` registering a tool symbol through the existing
  `vis-language-typescript-bun` runtime, contributing to one slot.

**Unknowns.** Does the Bun language extension already have a registration channel back into the
engine, or does this need a new transport? Must be answered BEFORE the phase starts — it decides
whether Phase 4 is a week or a month.

## Phase 5 — Shrink the core to what only the engine can own

**Rationale.** The documents make externalization possible; nothing yet makes it happen. `src/` is
4.75 MB and holds capabilities — search, editing, attachments, voice, doctor checks — that the
extension points now express.

**Data.** None. Code moves; no persisted, wire or mirrored shape changes.

**Acceptance criteria.**

- A `core` that keeps only: the iteration loop, the context engine, the sandbox, the extension
  registry, the gateway transport and the contract readers. Every other capability lands under
  `extensions/common/vis-foundation-*` with `:ext.engine/builtin? true`, the way
  `vis-foundation-search` already does.
- `test/com/blockether/vis/internal/core_surface_test.clj` — a BUDGET test: `src/` byte count and
  `core` public-var count may not grow; each phase records the new ceiling.
- No behavior change visible to a session: the full suite passes unchanged.

**Unknowns.** Which family moves first? Candidate order by size and independence: attachments
(47 KB + 13 KB storage), editing (265 KB), doctor (23 KB). Decide with the budget test in front of
us, one family per commit.

## Phase 6 — Publish the contract and its bindings

**Rationale.** Every phase above is unusable by anyone outside this repo until the artifacts are on
the registries; `vis-contract`, `vis-agent` and `@blockether/vis-contract` are all unpublished, and
the PyPI names are still free.

**Data.** None. Packaging and identity only.

**Acceptance criteria.**

- Clojars `com.blockether/vis-contract`, PyPI `vis-contract` + `vis-agent`, npm
  `@blockether/vis-contract`, all stamped from `VIS_VERSION` by
  `apps/vis-companion/scripts/version.mjs`.
- `.github/workflows/` — one release job per registry, triggered by the `vX.Y.Z` tag.
- `.agents/skills/release-vis/SKILL.md` — the publish steps, in order.

**Unknowns.** The publishing IDENTITY (owner account, org, license header) — the user's decision,
outstanding since the previous plan and still blocking.

## State of the plan

**REQUIRES WORK** — awaiting the user's answer on Phase 4 (do we do the TypeScript binding now?)
and on Phase 6's publishing identity.

Done:

- Predecessor plan, fully landed: Python host contract as data, the `vis` module as a real package,
  the outside-Vis host, the Clojure host debt gate — commits `93e379fa0`, `67d8ffcc8`, `82a3220ff`.
- Contract extracted into `packages/vis-contract`, released in both ecosystems' build paths —
  commit `4cdcae1a4`.
- Python host boundary declared as `typing.Protocol` with `check_host` — commit `137d00650`.

TODO, in order:

1. Phase 1 — `extension-points.edn` + reader + drift test.
2. Phase 2 — `slots.edn`, registration refuses undeclared slots.
3. Phase 3 — Python declarators for every data-carrying point, parity test flips.
4. Phase 4 — TypeScript binding (pending decision).
5. Phase 5 — core budget test, then one capability family per commit.
6. Phase 6 — publish (pending identity decision).
