# Vis neurosymbolic roadmap

Phases adding symbolic capabilities to the Vis engine. Each phase has:
- **GOAL** — what symbolic capability lands
- **PAPER** — literature backing (see HANDOFF.md for reading list)
- **TESTS** — gate as lazytest specs (`clojure -M:test -n <ns>`)
- **CLI VERIFY** — repeatable smoke run against a real model so the
  capability is actually used by the LLM, not just stubbed by tests

Use `git log --grep="Phase [A-Z]:"` to find landed work.

---

## Phase A — Failed-proof archive ✅ LANDED (`ffc28e34`)

**Goal.** When a `:validator-fn` rejects a proof, append a rejection
entry to the task's `:archived-proofs` vec. Model surfaces it via
`(introspect-failed-proofs :K)` and reads a render-time anchor line.
Pattern: Reflexion verbal reinforcement (Shinn NeurIPS 2023).

**Paper.** Reflexion (arxiv 2303.11366); LOGIC-LM++ (NLRSE 2024).

**Tests.** `com.blockether.vis.internal.ctx-engine-test`:
- `archive-failed-task-proofs-test` — entry written, idempotent, lifecycle preserved
- `archived-proofs-cap-test` — FIFO at 10
- `reconcile-hook-task-archives-on-revert-test` — hook-task revert archives
- `introspect-failed-proofs-test` — SCI surface returns archive

**CLI verify.**
```bash
# any session that flips a hook-task with a wrong proof scope works.
# Easiest repro: ask for a deliverable that triggers session-title hook,
# then submit a bogus :proof scope and re-read introspect-failed-proofs.
./bin/vis --provider openai-codex --model gpt-5.5 \
  'introspect-failed-proofs zwraca pustą listę dla świeżego tasku'
```

---

## Phase B — Universal `:depends-on` + relations ✅ LANDED

**Goal.** Extend `:depends-on` from tasks-only to specs and facts.
This subsumes the "knowledge-graph relations" idea: instead of a
parallel triple store, relations ARE typed depends-on edges across
entity classes. Two new fns: `(spec-depends! :K1 [:K2 …])` and
`(fact-depends! :K1 [:K2 …])`. Cycle detection generalized across all
three subtrees so `:spec → :task → :fact → :spec` chains can't loop.

Engine derives a unified `:session/dep-graph` for introspection +
warnings ("orphaned spec — no proof and no inbound dep").

**Paper.** Drools / CLIPS production-rule working memory; Bader &
Hitzler 2005 (graph structure as relational substrate); Soufflé
(bottom-up evaluation over relations) — knowledge graphs without a
separate KG table.

**Tests.** `ctx-engine-test`:
- `universal-depends-on-test` — set on each entity class; engine
  preserves; introspect returns deps
- `cross-entity-cycle-rejection-test` — spec→task→fact→spec rejected as
  hard error, same shape as existing task cycle test
- `dep-graph-derivation-test` — `derive-dep-graph` returns unified map
  `{:spec/K #{[:spec :K2] [:fact :F]}}`
- `orphaned-entity-warning-test` — fact with no incoming/outgoing dep
  emits soft warning `;; ⚠ orphaned-fact`

**CLI verify.**
```bash
./bin/vis --provider openai-codex --model gpt-5.5 \
  'Zdefiniuj spec :auth z dwoma req, task :impl który zależy od spec :auth,
   oraz fact :baseline zależny od task :impl. Pokaż graf via
   (introspect-dep-graph).'
# Expect: model emits (spec-set!) (task-set! :depends-on [:auth])
# (fact-set! :baseline) (fact-depends! :baseline [:impl])
# rendered ctx shows the chain; (introspect-dep-graph) returns it.
```

---

## Phase C — Contradiction detection ✅ LANDED

**Goal.** When two `:active` facts have semantically opposing content
(detector starts simple: `:K1` and `:K2` both `:active` and
`:K1`'s `:content` contains a `(contradicts :K2)` declaration via a
new `(fact-contradicts! :K1 :K2)` engine fn) → emit a soft warning
`;; ⚠ contradicting-facts :K1 ↔ :K2`. Engine doesn't auto-resolve
(would need NLI); model decides which to flip `:superseded`.

**Paper.** Classical SAT/SMT consistency checking; PRISMA 2024
contradiction handling is a flagged open problem.

**Tests.**
- `fact-contradicts-symmetric-test` — declaring on one side also
  surfaces on the other
- `contradicting-facts-warning-test` — both `:active` → warning;
  one `:superseded` → silent
- `transitive-contradiction-test` — `:A ↔ :B` and `:B ↔ :C` does NOT
  imply `:A ↔ :C` (predicate is explicit, not transitive — avoid
  inference cascade)

**CLI verify.**
```bash
./bin/vis 'Najpierw fact-set! :db-engine sqlite, potem
fact-set! :db-engine postgres, zadeklaruj kontradykcję. Pokaż ⚠.'
```

---

## Phase D — Reactive triggers (forward chaining) ✅ LANDED (v1)

**v1 status.** Observation-only watchpoints. Rule fires emit
`:rule-fired` soft warnings carrying the declared `:message`. Rule-
driven MUTATIONS deferred to v2; cycle protection then becomes
mandatory (max reentrancy depth, e.g. 3, with `;; ⚠ rule-cycle`).


**Goal.** Engine fires user-declared rules on state change, not just
at hook tick. Three triggers:
- `(rule-set! :K {:when [:on-fact-change :F] :then (fn [old new] …)})`
- `(rule-set! :K {:when [:on-validator-fail :spec/K :req-id] :then …})`
- `(rule-set! :K {:when [:on-task-status :K :doing] :then …})`

The `:then` is an SCI source string. Engine evaluates at relevant
mutation in a bounded sandbox (50 ms, same as validator-fn). Rules
appear under `:session/rules`. No RETE — naive scan over rules per
mutation. Vis working-memory is small (≤ hundreds of entities), naive
is fine.

**Paper.** Forgy 1982 RETE; Drools/CLIPS production-rule semantics;
Soufflé bottom-up evaluation; ENVISIONS environment feedback loop.

**Tests.**
- `rule-fires-on-fact-change-test` — set rule, mutate fact, observe
  the `:then` side effect (e.g. task auto-flip)
- `rule-fires-on-validator-fail-test` — failed proof triggers rule
  before reconcile runs
- `rule-bounded-sandbox-test` — 50 ms timeout enforced;
  `;; ⚠ rule-timeout` emitted
- `rule-cycle-protection-test` — rule A fires rule B fires rule A →
  detect and warn after N=3 reentrancy

**CLI verify.**
```bash
./bin/vis 'Zdefiniuj rule :ban-bcrypt: gdy fact :crypto
zmienia content na coś z "md5", auto fact-set! :crypto-warning
:active. Potem zaktualizuj fact :crypto. Pokaż że warning powstał.'
```

---

## Phase E — Proof composition ✅ LANDED

**Goal.** Proof shape extended to support composition. Today:
`{:requirement :r1 :proof "tN/iM/fK"}`. Add:
`{:requirement :r1 :proof-compose ["tN/iM/fK" "tN/iM/fL"]}` where
ALL referenced scopes must pass their own `:validator-fn`. Engine
runs each, AND-s the result. Optional `:proof-rule {:type :and|:or}`
for OR-composition. Modus ponens via cross-req composition deferred
to a follow-up iteration of this phase.

**Paper.** Neural Theorem Provers (Rocktäschel & Riedel 2017); NTP
shows proofs as DAG; LOGIC-LM symbolic reasoning stage; LLM-Modulo
generate-test-critique with composable critics.

**Tests.**
- `proof-compose-and-test` — both sub-proofs pass → req satisfied
- `proof-compose-or-test` — one sub-proof passes, other fails → req
  satisfied with `:or`
- `proof-compose-archive-on-partial-fail-test` — `:and` with one
  failed sub-proof archives the FAILED sub-proof, not the whole compose
- `proof-compose-schema-test` — invalid `:proof-rule` rejected at
  `proof-add!` time

**CLI verify.**
```bash
./bin/vis 'Spec :api-secure z jednym req :defense-in-depth.
Stwórz dwa proof scopes: jeden testuje TLS, drugi testuje rate-limit.
Połącz je proof-compose AND. Done re-validates oba.'
```

---

## Phase F — Trailer FTS index ✅ LANDED

**Goal.** New SCI fn `(trailer-find {…filter})` over the existing
SQLite `search` FTS5 virtual table. The `search` table already
indexes `session_turn_iteration.code` per iteration; extend with:
- per-form `:src` indexed as `field='form-src'`
- per-form `:result` (truncated to FTS_RESULT_CAP) as `field='form-result'`

Query keys:
- `:src-matches "regex|fts-query"`
- `:tag :observation|:mutation`
- `:scope-after "tN/iM"`
- `:limit N` (default 20)

NO embeddings. SQLite FTS5 BM25 is fast, deterministic, free.

**Paper.** Voyager skill library retrieval pattern (NeurIPS 2023) —
but implemented via FTS5 instead of vector cosine because (a) we
already have the index, (b) determinism, (c) zero embedding-API
dependency.

**Tests.**
- `trailer-find-by-src-test` — fts5 query returns matching form scopes
- `trailer-find-by-tag-test` — filter on observation/mutation
- `trailer-find-scope-after-test` — temporal filter
- `trailer-find-empty-result-test` — clean nil/empty handling
- migration test: `search` table gets new `field='form-src'` /
  `form-result'` rows on iter insert

**CLI verify.**
```bash
./bin/vis 'Po 3 iteracjach v/rg / v/cat zapytaj
(trailer-find {:src-matches "v/cat"}) i pokaż scope listę.'
```

---

## Phase G — Self-Discover reasoning structure — DROPPED

**Status.** Dropped after design review. Vis prompt already carries a
full FSM (TURN LIFECYCLE: STATES / EVENTS / PRED / TRANSITIONS /
ACTIONS) plus a deterministic `:session/next-actions` scheduler.
Self-Discover stage 1 ("pick reasoning structure") would duplicate
what the FSM + scheduler already declare. Skipped.

## Phase G' — Coherent projection (timeline + orphans) ✅ LANDED

**Goal.** Render-time projection layer that replaces the raw
`:session/{specs,tasks,facts}` maps with a task-rooted timeline and
an orphans bucket. Storage (`session_turn_state.ctx` snapshots, the
live ctx atom, `introspect-*`) keeps full raw fidelity.

Per-entity drops in projection:
  - `:validator-fn` source on each req (engine still runs it;
    `(introspect-spec :K)` returns raw)
  - `:archived-proofs` full vec on tasks (replaced by `:rejected-count`)
  - `:validated?` flag on tasks (engine-internal)

Per-entity derived in projection:
  - `:progress "N/M (P%) state"`, `:missing [...]`, `:validators "N/M"`
    on specs
  - flat `:proofs {:spec/req scope}` on tasks (drops nested `:specs`
    map shape)

Timeline ordering: task roots = tasks with no inbound task-dep.
Not priority-driven; topological by the model's declared dep graph.
Each root expands its connected specs+facts (BFS, deduped across
roots). Live-only — archived entities are excluded.

## Phase H' — Done bulk archive + `:status :archived` ✅ LANDED

**Goal.** `:archived` becomes a new terminal status across
fact/spec/task. `(done {:archive {:facts […] :specs […] :tasks […]}})`
bulk-flips listed entities to `:archived`. Engine GC removes them at
the next turn boundary (skips TTL wait). Snapshots keep raw;
`(introspect-archived :tasks|:specs|:facts)` returns them.

No granular `(fact-archive! …)` mutators — bulk via done is the only
surface, keeping end-of-turn as the single authoritative close.


**Goal.** Model declares reasoning structure for the turn on iter 1
via `(reasoning-structure-set! {:modules […]})`. Engine renders
structure under `:session/reasoning-structure`. Next iter compares
emitted form pattern against the declared structure; mismatches
emit `;; ⚠ structure-skip :module-id` advisory.

39 modules from Self-Discover (critical-thinking, step-by-step,
decomposition, …) seeded as defaults but user/extension can extend.

**Paper.** Self-Discover (Zhou NeurIPS 2024, arxiv 2402.03620).

**Tests.**
- `reasoning-structure-set-test` — stored on ctx, rendered
- `structure-skip-warning-test` — module declared but no form
  matched its pattern → advisory
- `unknown-module-rejection-test` — declaring an unknown module id →
  hard reject with valid-module suggestion

**CLI verify.**
```bash
./bin/vis 'Na początku tury zadeklaruj 3-modułową
strukturę rozumowania: decomposition → critical-thinking →
step-by-step. Następnie rozwiąż problem komputujący FizzBuzz dla
n=15. Pokaż że każdy moduł ma odpowiadającą formę.'
```

---

## Phase H — Secondary-model consultation — SUPERSEDED

**Status.** v1 sync `consult-fast/balanced/deep` landed at `1d9677ba`,
reworked to always-map at `ece24f3c`. SUPERSEDED by Phase H' — the v1
bindings are REMOVED (no legacy).

## Phase H' — Async cross-iter consult mini-engine + scope-aware bindings ⏳ NEXT

### GOAL

Replace the sync, primary-blocking consult-fast/balanced/deep surface
with an ASYNC cross-iter request/result protocol. Consult becomes a
separate **single-iter mini-SCI engine** running on a side thread with
a DIFFERENT set of SCI bindings than primary (scope-aware). Primary
declares an intent inside its fence; engine fires every pending intent
in parallel between turns; results materialise as a NEW dedicated ctx
section `:session/consult-results` (NOT in trailer). Primary decides
what to do with each result.

Motivation:
- Sync consult-* blocked the primary fence on a slow secondary LLM
  call. Async cross-iter removes that block.
- Multiple consults per primary turn run in parallel without futures.
- Consult's reasoning is structurally separated from primary's: own
  mini-SCI, own bindings, no shared ctx mutation.
- Trailer becomes pure observation log (engine mutators don't pin).

### SPEC

#### Consult lifecycle

```
[Primary turn T iter N]
  (consult-request! :research :deep
    {:focus    ["q1" "q2"]
     :question "...prose question..."})
  ;; → {:ok? true :consult-id :research :status :pending}
  ;; pure-data effect on env: pushes intent to :engine/pending-consults
  ;; primary fence completes without blocking

[End of turn T — engine processes pending consults]
  spawn ONE future per intent (parallel)
  each future:
    1. fresh mini-SCI ctx, consult-scope bindings ONLY
    2. build consultant messages: system + user question + ctx snapshot
    3. single LLM call → single fence → single SCI eval
    4. capture (answer {...}) emission
  await all futures (timeout 60s/intent)
  materialise each result onto :session/consult-results vec

[Primary turn T+1 iter 0]
  ctx renders :session/consult-results with new entries from T
  primary reads, decides:
    - (consult-promote! :research :K)   ; copy as fact :K, drop entry
    - (consult-dismiss! :research)      ; drop entry without promoting
    - ignore (auto-archived after 3 turns)
```

#### Entry shape (consult-result)

Minimal required, plus optional engine-stamped audit:

```
;; SUCCESS
{:consult-id  :research              ; primary's identifier (kw)
 :status      :active                ; engine-set
 :content     string                 ; consultant's prose synthesis (REQUIRED)
 :citations   [{:type :paper :url string? :title string? :excerpt string?}]
 :confidence  :high|:medium|:low
 :focus       [string ...]?          ; vec of focus points from request (preserved)
 :focus-met?  [boolean ...]?         ; vec aligned to :focus order; consultant assessment
 :preference  :fast|:balanced|:deep  ; engine-stamped from request
 :born        "tN"                   ; engine-stamped: turn declared
 :duration-ms long                   ; engine-stamped: consult wall time
 :retries     0|1}                   ; audit; 0 = fit first try, 1 = needed compress retry

;; FAILURE
{:consult-id  :research
 :status      :failed
 :error       :timeout|:provider-error|:malformed-answer|:budget-exhausted|:exceeds-cap
 :reason      string                 ; human explanation
 :focus       [string ...]?          ; preserved from request
 :preference  :deep
 :born        "tN"
 :duration-ms long
 :retries     int?}                  ; only when :error :exceeds-cap
```

#### Citation min schema

```
{:type :paper|:web|:code|:doc       ; REQUIRED
 :url?     string?                    ; either :url
 :title?   string?                    ; OR :title required
 :excerpt? string?                    ; max 500 chars}
```

Engine drops malformed entries + warns (`:malformed-citation N dropped`).

#### Token caps (per preference)

Measured via `internal/tokens.clj` (jtokkit cl100k_base). Chars cap
is a fiction — LLMs bill on tokens, primary's ctx is bounded by
tokens, so all caps live in token units.

| Preference | `:content` token cap | Use case |
|---|---|---|
| `:fast`     |  1 000 tokens | quick critique, sanity, 1–3 sentence reply |
| `:balanced` |  4 000 tokens | solid analysis, several paragraphs |
| `:deep`     | 12 000 tokens | full research synthesis with citations |

Per citation:
- `:excerpt` max **500 tokens** (~one paper abstract)
- max **15 citations** per consult

Safety belt on `:session/consult-results` SUM: **50 000 tokens hard
cap**. When summed entries exceed cap, engine drops oldest entry
(FIFO) regardless of 3-turn TTL and emits a renderer warning:

```
;; ⚠ consult-results overflow: dropped :K to fit 50 000-tok budget
```

On per-entry overflow: engine **never truncates**. Instead it
re-prompts the consult LLM in the same side thread (same SCI ctx) with
a compression instruction carrying THREE explicit numbers:

```
"Your previous answer was N tokens; cap for :<preference> is M tokens
 (over by X tokens, X = N - M). Compress :content to fit within M.
 Keep most-cited findings and critical citations; drop tangents,
 adjectives, repetition. Same :focus / :citations spirit, tighter prose.
 Re-emit (answer {…})."
```

- N = actual token count of consult's `:content` (jtokkit cl100k_base)
- M = cap for the consult's preference (1000 / 4000 / 12000)
- X = N - M, the explicit overage so the model can SIZE THE CUT

Max retries = 1 (so worst case = 2 LLM calls per consult intent).
If the second attempt still overflows, the entry lands as
`:status :failed :error :exceeds-cap` with `:reason` listing both
attempt sizes: `"first N1 / cap M / second N2 / cap M (still over)"`.
Primary then decides whether to re-issue with a narrower `:focus` or
different `:preference`.

Citations beyond 15 dropped + warned (NOT retried — the consult LLM
had N citations and the engine slices the tail; not worth re-prompting
for citation count alone). Excerpts above 500 tokens behave the same
way: drop tail of the excerpt with no marker (excerpts are abstracts;
a sliced abstract is still useful).

#### Engine scope filter (`:ext.symbol/engine-scope`)

Every extension symbol gains an OPTIONAL field:

```
{:ext.symbol/symbol 'v/patch
 :ext.symbol/engine-scope #{:main}}        ; primary only

{:ext.symbol/symbol 'search/web
 :ext.symbol/engine-scope #{:consult}}     ; consult only

{:ext.symbol/symbol 'v/cat
 :ext.symbol/engine-scope #{:main :consult}} ; both (read-only ops)
```

Default when field missing: `#{:main}` (backward compat).

#### Trailer pin rule (NEW)

A form is pinned in `:session/trailer` IFF its result is **NOT**
`:vis/silent` AND it's not `(done …)`. Engine mutators that return
`:vis/silent` (spec/task/fact/proof/req/depends/contradicts/rule/
consult-request/promote/dismiss) DISAPPEAR from the trailer entirely
(no `:src`, no `:result`, no entry). Their effect is visible only via
ctx mutations (new specs/tasks/facts/results in the next iter).

#### `future` available ONLY in consult SCI

Primary's SCI sandbox no longer binds `future` / `future?` / `deref`
/ `realized?`. Consult's mini-SCI sandbox binds them (so a single
consult fence can parallel `search/web` + `search/papers` calls).

### REQUIREMENTS

#### R1 — `:ext.symbol/engine-scope` filter + per-scope prompt assembly

**Scope per symbol** (single source of truth, no cross-ext deps):

- Spec field `:ext.symbol/engine-scope` on each symbol entry
- Default `#{:main}` (backward compat — every existing symbol primary-only)
- Conventional scope assignments:
  - read-only file/search ops (`v/cat`, `v/ls`, `v/rg`): `#{:main :consult}`
  - mutating ops (`v/patch`, `v/write`, `v/delete`): `#{:main}`
  - research ops (`search/web`, `search/papers`): `#{:consult}`
  - engine fns (spec-set!, task-set!, fact-set!, all mutators): `#{:main}`
  - introspection (`introspect-*`): `#{:main}` (consult sees ctx via
    embedded snapshot, not via fn calls)
- Helper `(extension/ext-symbols-in-scope ext scope)` filters per scope
- Primary SCI bindings build filters with `:main` scope
- Consult mini-SCI bindings build filters with `:consult` scope
- Test: an extension with mixed-scope symbols projects correctly into
  each scope; an extension whose symbols are ALL `:main` contributes
  nothing to consult

**Per-scope prompt assembly** (different prompts for primary vs consult):

- Engine builds:
  - **primary system prompt** = `CORE_SYSTEM_PROMPT` + concat of every
    active extension's `:ext/prompt` (existing path; unchanged)
  - **consult system prompt** = `CONSULT_BASE_PROMPT` (NEW, short,
    dedicated to research+synthesis) + auto-generated docs from
    `:ext.symbol/doc` of every symbol with `:consult` in its scope
- Consult system prompt deliberately drops:
  - the primary `CORE_SYSTEM_PROMPT` (engine FSM, spec/task/fact docs,
    consult own docs — all irrelevant to a single-iter research call)
  - any `:ext/prompt` from extensions (those are for primary)
- Consult LLM sees ONLY: consult role + cap rules + answer schema +
  available symbol docs (auto-gen). Different mental model = different
  prompt.
- Test: rendered consult system prompt does NOT contain any text from
  `CORE_SYSTEM_PROMPT` (e.g. "TURN LIFECYCLE", "spec-set!", "REASON"
  must NOT appear); does contain consult-base headers + symbol docs

#### R2 — Trailer pin filter

- `advance-iter` in `ctx-engine` checks each candidate form's `:result`
- Forms with `:result :vis/silent` are dropped before the pin lands
- Existing `(done …)` exclusion stays; `:vis/silent` filter is
  additional, not replacement
- Engine-internal warnings: NONE — silent drop is the intended behaviour
- Test: a fence containing `(spec-set!)`, `(task-set!)`, `(v/cat)`
  pins ONLY the `v/cat` form in the trailer

#### R3 — `future` scope restriction

- `env/create-sci-context` for primary scope excludes `future`,
  `future?`, `realized?`, `deref` from the sandbox bindings
- Consult mini-SCI ctx INCLUDES them
- Test: primary fence attempting `(future …)` raises
  `Unable to resolve symbol: future`; consult fence resolves it

#### R4 — `consult-request!` async push + `await-consult!` sync wait

**Async path — `consult-request!`:**

- Mutator binding in primary SCI scope (`:main`)
- Accepts `(consult-request! :id :preference "question")` OR
  `(consult-request! :id :preference {:focus […] :question "…"})`
- Registers intent under `:engine/pending-consults` on ctx-atom AND
  **fires a future immediately** on a side thread (consult-engine/run)
- Returns `:vis/silent` (NOT a string — to keep it OUT of trailer)
- Synchronously validates id/preference/question; rejects with soft
  warning via `:engine/warnings` when invalid (no intent enqueued)
- Budget check: when consult-budget-atom exhausted, warns + skips
  enqueue with `:consult-budget-exhausted` soft warning

**Fetch path — `await-consult!`:**

- `(await-consult! :id) → entry-map` reads the materialised entry from
  `:session/consult-results` and PINS the full map in `:session/trailer`
  as the form's `:result`
- Same-iter as the `consult-request!` → returns
  `{:error :consult-not-resolved-yet :reason "..."}` because engine
  awaits unresolved futures at iter boundary, not mid-iter. Primary
  MUST split declaration and fetch across iters.
- Unknown id (never declared) → `{:error :unknown-consult-id}`
- Entry already promoted/dismissed → `{:error :consult-already-resolved}`
- Successful fetch: same map shape as `:session/consult-results` entry;
  trailer pin scope tracked under `:engine/consult-trailer-scopes`
  for later scrub on promote/dismiss

**Iter-boundary contract:**

- consult-request! IMMEDIATELY fires the future on a side thread
- At iter end, engine **awaits all unresolved futures** (block primary's
  iter N+1 start until all consults resolve or hit 60s/each timeout)
- Iter N+1 ALWAYS sees consults declared in iter N as materialised in
  `:session/consult-results`
- Primary therefore never "loses" a consult — fetch is always available
  the iter after declaration

**Done gate:**

- `(done {…})` REJECTED when any consult declared in the current iter
  is still pending (i.e. `(seq (:engine/pending-consults ctx))` > 0
  measured at done-form eval time)
- Engine refuses the done emission and emits soft warning:
  `;; ⚠ done blocked: <N> consult(s) pending this iter (:K, :K2);
   await/decide before close`
- Primary MUST issue a fresh iter to fetch + decide before retrying done
- Rationale: closing a turn while research is still mid-flight wastes
  the consult call; engine forces primary to integrate results

- Test: declaring N intents fires N parallel futures; same-iter
  await-consult! returns :consult-not-resolved-yet; same-iter done
  rejected; iter-end blocks until remaining futures resolve; iter N+1
  await-consult! succeeds and pins trailer; done in iter N+1 succeeds
  when no fresh pending consults

#### R5 — Mini-SCI consult engine

- New ns `com.blockether.vis.internal.consult-engine`
- `(run-consult! parent-env intent) → entry-map`
  - builds fresh SCI ctx with consult-scope bindings
  - includes `search/*` + `future` + minimal `clojure.core`
  - builds consultant messages: system + USER (focus + question +
    ctx snapshot text) — NO primary system prompt
  - calls router with preference → provider+model from
    `:consult-config` (or `DEFAULT_PREFERENCE_MAP`)
  - parses returned fence, eval'es ONCE on side thread
  - captures `(answer {…})` emission via side-thread atom
  - returns the structured entry map
- Bounded: 60s hard wall via existing svar timeouts
- Test: mocked router returns canonical fence; engine eval'es; entry
  map carries `:content`, `:citations`, `:confidence`, `:focus-met?`

#### R6 — `process-pending-consults!` parallel runner

- Replaces existing v1 impl in `ctx-loop`
- Reads `:engine/pending-consults`, drains the vec
- For each intent: spawn `(future (consult-engine/run-consult! env intent))`
- Await all (default 60s each via consult-engine's internal timeout)
- Materialise each result onto `:session/consult-results` (append vec)
- Clear `:engine/pending-consults` atomically with the materialisation
- Test: 3 intents declared in one turn run in parallel; wall time
  ≈ max(durations); all 3 entries land on results vec

#### R7 — Engine-side validation + retry-to-fit

- After consult-engine returns its raw map, validate against schema
- Token-count `:content` via `internal/tokens.clj` (jtokkit cl100k_base)
- If `:content` exceeds preference cap (1k / 4k / 12k tokens),
  RE-PROMPT consult in the same side thread + same SCI ctx. The
  re-prompt text carries THREE numbers: N (actual tokens), M (cap),
  X = N - M (overage). Engine constructs the re-prompt; consult LLM
  just sees the message.
- Max retries = 1 (worst case 2 LLM calls per consult intent)
- After retry attempt: if still over cap, emit `:status :failed`
  `:error :exceeds-cap` `:reason "first N1 / cap M / second N2"`
  `:retries 1`
- Successful entry stamps `:retries 0` (fit first try) or `:retries 1`
- Citations beyond index 15: tail dropped + soft warn `:too-many-citations`
  (no retry for this — not worth a round trip for citation count)
- Excerpts above 500 tokens: tail-clipped per citation (excerpts ARE
  abstracts; a sliced abstract still useful, no marker needed)
- Drop malformed citations (no `:type`, no `:url`/`:title`) + warn
- When `:content` missing/blank: emit `:status :failed`
  `:error :malformed-answer`
- Test: oversized content triggers retry; second-attempt fit lands
  as :active :retries 1; second-attempt overflow lands as
  :failed :exceeds-cap; malformed citations dropped + warned

#### R8 — `:session/consult-results` ctx section

- New top-level ctx section, vec of entry maps
- Engine appends at consult processing time
- Renderer emits as new section between `:session/trailer` and
  `:session/next-actions`
- Compact preview annotation per entry (similar style to
  `;; rejected-proofs`):
  ```
  ;; consult-results (3 entries; 18.4k / 50k tokens)
  ;;   :research :high — Reflexion (arxiv 2303.11366)
  ;;   :critique :medium — 2 issues found
  ;;   :review-A :failed (:timeout)
  ```
- GC: entries older than 3 turns dropped at turn boundary; primary
  can dismiss/promote earlier
- Safety belt: if summed token count exceeds 50 000, drop oldest
  entry FIFO + warn `;; ⚠ consult-results overflow: dropped :K`
- Test: render shows new section with each entry compact; GC drops
  entries past TTL; oversized sum drops oldest entry

#### R9 — `consult-promote!` / `consult-dismiss!` + trailer scrub

- `(consult-promote! :consult-id :fact-K)`:
  1. Copies `:content`, `:citations`, `:focus`, `:confidence` into a
     NEW fact under `:K`
  2. Removes the entry from `:session/consult-results`
  3. **Scrubs the trailer pin** that landed when `(await-consult! :id)`
     ran (any iter — retroactive across the session's trailer)
- `(consult-dismiss! :consult-id)`:
  1. Removes the entry from `:session/consult-results`
  2. **Scrubs the trailer pin** for that consult-id
- Both mutators return `:vis/silent` themselves (do NOT pin in trailer)

**Trailer scrub mechanism:**

- When `(await-consult! :id)` pins in trailer, engine records the form
  scope under `:engine/consult-trailer-scopes {:id #{"tN/iM/fK" …}}`
- `consult-promote!` / `consult-dismiss!` read this map, remove every
  matching scope from `:session/trailer` (across iters), clean empty
  iter entries (where all forms got scrubbed), and drop the
  `:engine/consult-trailer-scopes` entry for that id
- Rationale: once primary has DECIDED on a consult (promote or
  dismiss), the trailer pin is redundant — promoted data lives in
  `:session/facts` (canonical); dismissed data was rejected (noise).
  Keeps trailer tight after the decision.
- If primary never decides, the await pin lives in trailer normally
  until natural trailer-summarize / GC

- Test: await pin scrubbed after promote (no trace in `:session/trailer`);
  await pin scrubbed after dismiss; scrub works retroactively across
  iters (await in iter N, promote in iter N+2 still scrubs N's pin);
  empty iter entries dropped after scrub

#### R10 — `search/*` extension (renamed from `exa/`)

- New extension namespace `search/` with scope `#{:consult}`
- `(search/web query)` — wraps existing exa.ai web search
- `(search/papers query)` — direct arxiv API (Atom XML parse to maps)
- `(search/code query)` — future, deferred
- Each search fn returns vec of `{:title :url :excerpt :source …}`
  maps so consultant can stitch them into `:citations` easily
- Existing `exa/*` symbols REMOVED from `:main` scope (breaking)
- Test: arxiv API returns parseable Atom; search/papers translates to
  citation-friendly maps; symbols not visible in primary's SCI

#### R11 — V2 prompt update

- DROP `consult-fast/balanced/deep` sync section
- DROP `future` from documented primary SCI symbols
- ADD `consult-request!` async section with `:focus` array semantics
- ADD minimal entry schema docs
- ADD `consult-promote!` / `consult-dismiss!` docs
- ADD `:session/consult-results` to MEMORY LAYERS
- ADD note: trailer no longer pins engine mutators (silent results)
- Test: prompt-core test pins the new shape (consult-request! mentioned,
  consult-fast NOT mentioned)

#### R12 — Tests + CLI verify

- Engine unit tests per R1-R10 above
- ctx-loop integration test: N consults in one turn → N entries in
  results next turn
- CLI verify:
  ```bash
  ./bin/vis --provider anthropic-coding-plan --model claude-sonnet-4-6 \
    'Use consult-request! to research the Reflexion paper. In the next
     turn read the result and promote it to a fact called :reflexion-paper.'
  ```
  Expect: at iter 1 model emits `(consult-request! :reflexion :deep
  {:focus [‘91% claim’] :question “…”})`; at iter 2+ model sees
  `:session/consult-results` and `(consult-promote! :reflexion
  :reflexion-paper)`; final `:session/facts` carries the promoted fact.

### GATES (verification protocol)

Each requirement R1–R12 has its own lazytest spec(s) that must pass
before the phase is considered landed.

1. **Unit tests pass.**
   `clojure -M:test -n com.blockether.vis.internal.consult-engine-test
    -n com.blockether.vis.internal.consult-test
    -n com.blockether.vis.internal.ctx-engine-test
    -n com.blockether.vis.internal.ctx-renderer-test
    -n com.blockether.vis.internal.ctx-loop-test
    -n com.blockether.vis.internal.prompt-test`
2. **Thread isolation gate.** During an active consult execution:
   - primary's ctx-atom value MUST NOT mutate from consult side
   - consult's mini-SCI ctx MUST be on a different thread than
     primary's eval thread (assert via Thread/currentThread identity)
   - the ONLY allowed cross-boundary write is the atomic swap that
     appends to `:session/consult-results` AFTER the future resolves
   - Test: while consult future is pending, mutate primary ctx-atom
     via spec-set! and verify consult side does not observe it; after
     consult resolves, verify the materialisation path swapped
     atomically into the same primary ctx-atom
3. **Real-model smoke run** (CLI verify above) succeeds across two
   providers (anthropic + openai-codex minimum).
4. **4Clojure regression** does not regress > 2pp pass rate vs baseline
   (likely improves on tasks where critique helps).
5. **Prompt cache check.** After landing, run a 2-iter probe and
   inspect `llm_cached_tokens` on iter 2. Cache hit ratio should not
   drop > 30% vs baseline — V2 prompt changes are additive, not
   structural.
6. **Token budget on a typical turn.** Compare ctx render before/after
   Phase H' on the SAME 6-turn session. Phase H' should be NEUTRAL or
   smaller (because trailer pin filter drops engine mutators).
7. **No-legacy gate.** After landing:
   - `(consult-fast …)`, `(consult-balanced …)`, `(consult-deep …)`
     MUST NOT resolve in primary SCI sandbox (no v1 surface remains)
   - V2 prompt MUST NOT mention `consult-fast/balanced/deep`
   - `com.blockether.vis.internal.consult` ns is REMOVED or fully
     rewritten (no zombie sync API)

### NON-GOALS (explicit)

- NOT a sub-agent. Consult has no persistent ctx, no own iter loop
  across turns, no DB writes for consult state. One-shot eval per
  request.
- NOT provider tool-calling. We do NOT use Anthropic/OpenAI native
  `tools` parameter. Consult uses our own SCI-bindings sandbox.
- NOT a fix-on-promotion. `consult-promote!` copies data as a fact;
  it does NOT auto-add proofs, auto-set tasks, or otherwise mutate
  primary's symbolic state beyond the named fact.
- NOT multi-iter consult. Each consult is ONE LLM call + ONE SCI
  eval. If model wants iteration, it issues another
  `consult-request!` in a later turn.

### IMPLEMENTATION ORDER (handoff)

Work the R's in this order. Each is its own commit; each must pass its
own gate (per-R tests) plus the running global gates (full suite green,
thread isolation, no-legacy when complete).

```
R0  PRE-CONDITION  consolidate scope-aware plumbing
     | R1 (scope filter + per-scope prompts)
     | R2 (trailer pin filter for :vis/silent)
     | R3 (drop future from primary SCI)
R4  CORE PUSH       consult-request! + await-consult! + done gate
R5  CORE EXEC       consult-engine mini-SCI runner
R6  CORE BATCH      process-pending-consults! parallel + iter-end await
R7  VALIDATE        token caps + retry-to-fit
R8  RENDER          :session/consult-results section + annotations
R9  DECISION        consult-promote! / consult-dismiss! + trailer scrub
R10 EXTENSION       search/web + search/papers (consult scope)
R11 PROMPT          V2 update + CONSULT_BASE_PROMPT
R12 INTEGRATION     end-to-end tests + CLI verify
```

A pre-condition R0 commit lands R1+R2+R3 together because they are
minor independent changes that the rest of Phase H' depends on. After
R0 the trailer is already cleaner, primary already has no `future`,
and bindings are scope-aware — the new consult layer (R4+) plugs into
this foundation.

### GLOBAL GATES (must hold after every Phase H' commit)

1. **All tests green.** `clojure -M:test` exits 0. Per-R unit suites
   above listed under each requirement.
2. **No-legacy gate.** `(consult-fast …)`, `(consult-balanced …)`,
   `(consult-deep …)` MUST NOT resolve in primary SCI sandbox
   anywhere after R4 commits. CI grep:
   `rg "(consult-fast|consult-balanced|consult-deep)" src test` returns
   only stripped references (e.g. dead-code comments removed). V2
   prompt must NOT mention v1 names. `com.blockether.vis.internal.consult`
   ns is rewritten or removed; no zombie sync API.
3. **Thread isolation gate.** Side-thread consult execution MUST NOT
   share mutable state with primary's eval thread. Only allowed
   cross-boundary mutation: atomic swap that appends to
   `:session/consult-results`. Test: while consult future is pending,
   primary mutates ctx-atom via spec-set!; consult side does not see
   the new spec in its embedded ctx snapshot (snapshot was taken at
   request time).
4. **Done gate enforcement.** `(done {…})` rejected when any consult
   declared in the current iter is pending. Soft warning emitted via
   `:engine/warnings`. Primary forced into a fresh iter to fetch +
   decide. Test: same-iter `consult-request!` + `(done {…})` results
   in done refusal + warning `:done-blocked-by-pending-consults`.
5. **Same-iter await refusal.** `(await-consult! :id)` in the same iter
   as the matching `(consult-request! :id …)` returns
   `{:error :consult-not-resolved-yet}` instead of blocking. Test as
   per R4.
6. **Trailer scrub semantics.** `(consult-promote! :id :K)` and
   `(consult-dismiss! :id)` retroactively remove the await-consult!
   trailer pin for that id across any iters. Test: await in iter N,
   promote in iter N+2; iter N's pin is gone in iter N+2's rendered ctx.
7. **Token cap enforcement (retry-to-fit).** Engine NEVER truncates
   consult `:content`. When over cap, re-prompts consult once with a
   compression instruction. Test: mocked router returning oversized
   first attempt + compliant second → entry lands with `:retries 1`;
   mocked router returning oversized BOTH attempts → entry lands as
   `:status :failed :error :exceeds-cap`.
8. **Prompt cache check.** After R11 lands, run a 2-iter probe and
   inspect `llm_cached_tokens` on iter 2. Cache hit ratio should not
   drop > 30% vs baseline.
9. **Token budget check.** Compare ctx render before/after Phase H'
   on the SAME 6-turn session. Phase H' should be NEUTRAL or smaller
   (because trailer pin filter drops engine mutators).
10. **4Clojure regression.** No regression > 2pp pass rate vs baseline.
    Likely improves on tasks where critique helps.
11. **Real-model smoke run.** CLI verify (below) succeeds across two
    providers minimum (anthropic-coding-plan + openai-codex).

### CLI VERIFY (end-to-end)

```bash
./bin/vis --provider anthropic-coding-plan --model claude-sonnet-4-6 \
  'Use consult-request! :reflexion :deep with focus ["verify 91% HumanEval
   claim"] and a question about the Reflexion paper. In the next turn
   await-consult! :reflexion, check :confidence, and consult-promote!
   the result to fact :reflexion-paper. Then done with a summary.'
```

Expected:
- Iter 1: `(consult-request! :reflexion :deep {:focus [‘91% claim’]
  :question “…”})` → :vis/silent. `(done …)` REJECTED if attempted.
- Iter 2: `:session/consult-results` carries `:reflexion`.
  `(await-consult! :reflexion)` pins entry in trailer, returns map.
  `(consult-promote! :reflexion :reflexion-paper)` scrubs trailer pin,
  creates fact. `(done …)` succeeds.
- Final `:session/facts` carries `:reflexion-paper` with content +
  citations.
- No `consult-fast/balanced/deep` ever appears in any iteration source.

---

## Phase I — Fact semantic search (Voyager) — DROPPED

**Status.** FTS5 (Phase F) covers semantic-ish recall via BM25 over
the already-indexed iter code. Adding per-fact embedding +
`(fact-search)` would multiply plumbing (embedding provider, cache,
rebuild on fact-set!) without proportional gain. Closed.

---

## Phase J — Knowledge-graph triples — MERGED INTO B

**Status.** Subsumed by universal `:depends-on` (Phase B). Relations
are typed edges in the dep-graph; no separate triple store needed.

---

## Phase K — Probabilistic validators — DROPPED

**Status.** Validators in the coding-agent use case are typically
deterministic (regex match, file exists, test passes). Probabilistic
surface ({:ok? bool :score double}) would add API + render complexity
for a use case the model can already express with a boolean predicate
on a score-bearing payload. Vis is inference-only — no gradient signal
to consume the score either. Closed unless a concrete need emerges.

**Goal.** Validators return `{:ok? bool :score 0..1 :reason}` instead
of pure boolean. Engine accepts `:score ≥ threshold` (default 0.5,
per-spec override). `;; progression` lines report `proven 4/6 score 0.83`
instead of just `4/6`.

DeepProbLog showed end-to-end probabilistic semantics in NeSy work;
we steal the probabilistic-predicate idea without the gradient
machinery (we're inference-only).

**Paper.** DeepProbLog (Manhaeve NeurIPS 2018, arxiv 1805.10872).

**Tests.**
- `score-validator-pass-threshold-test` — score 0.8 with default
  threshold → ok
- `score-validator-below-threshold-archive-test` — score 0.3 archives
  with reason `:score-below-threshold`
- `legacy-boolean-validator-compat-test` — boolean `true` still
  accepted as `score=1.0`
- `progression-score-render-test` — render shows aggregate score

**CLI verify.**
```bash
./bin/vis 'Zdefiniuj spec :code-quality z validatorem
score-style (zwracającym :score 0..1 na podstawie liczby lint
warnings). Pokaż :session/specs z progression score.'
```

---

## Long-horizon — Trajectory training

**Status.** Backlog, post-stable-engine.

When Phases A-K stable, accumulated `:archived-proofs` + their
`:reflection`s + revised proofs across many sessions form a
**trajectory corpus**. That corpus can finetune a dedicated reasoning
model (Constitutional AI SL+RL pattern, Bai et al 2022): SL on
critique→revision pairs, then RLAIF using engine's symbolic
verifier as ground truth.

Not implemented now. Engineering prerequisite: stable schema for
trajectory export (`vis trajectory dump --session-id X`).

---

## Verification protocol per phase

For every phase:

1. **Unit tests pass.** `clojure -M:test -n com.blockether.vis.internal.ctx-engine-test
   -n com.blockether.vis.internal.ctx-renderer-test`. New tests
   listed under TESTS above must be added in the same commit as the
   implementation.

2. **Real-model smoke run.** The CLI VERIFY block above is the
   reproducible probe. Run with two providers minimum (codex+zai or
   anthropic+copilot) to catch provider-specific quirks. Capture
   output to `dev/benches/phase-<X>/<ts>.txt` for the PR.

3. **Benchmark regression check.** 4Clojure subset (`./dev/benches/4clojure/run_subset.sh`)
   must not regress > 2pp on pass rate vs baseline. New capability
   should ideally help — but the bar is "doesn't hurt".

4. **Prompt cache check.** After change, run a 2-iter probe and
   inspect `llm_cached_tokens` on iter 2 from the DB. If it drops > 30%
   vs baseline, prompt prefix shifted accidentally and needs review.

5. **Backlog hygiene.** Move phase from "❌ backlog" to
   "✅ LANDED `<commit>`" in this file; if dropped, mark "— DEFERRED"
   or "— DROPPED" with rationale.

## Phase L — `(introspect-changes "tN")` turn delta ✅ LANDED

**Goal.** Lazy delta projection over the persisted snapshots. The model
asks `(introspect-changes "t5")` to get a vec of per-entity change
records between end-of-turn-4 and end-of-turn-5 ctx snapshots.

Shape per record:
```
{:kind  :spec|:task|:fact|:rule
 :K     <entity-key>
 :change :added                            ; new entity
       | :removed                          ; entity dropped
       | [[field before after] …]          ; field-level transitions
       | [… [:rejected-proofs N M] …]      ; archived-proofs growth as count delta
}
```

Tracked fields: `:status`, `:title`, `:content`, `:born`, `:done-born`,
`:depends-on`, `:proof`, `:validator-fn`, `:hook-id`, `:importance`,
`:source`, `:contradicts`, plus the derived `:rejected-proofs` count
delta. Engine-internal `:validated?` is dropped from the diff surface.

Trailer is intentionally NOT included in the diff — model already reads
it inline; diffing 30-form pins per iter would saturate the response
without adding signal.

**Tests.**
- `diff-ctx-test`: :added, :status flip, :done-born stamp,
  rejected-proofs delta, fact :content change
- `introspect-changes-test`: full snapshot path + nil when N or N-1
  is missing
