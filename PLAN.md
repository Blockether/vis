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

## Phase D — Reactive triggers — DROPPED

**Status.** Specs + requirements + `:validator-fn` already give the
watchpoint mechanism end-to-end (declarative + verifiable + auto-
warning on failure). A parallel rule channel was duplicate signal
for zero new capability. Removed entirely; `:session/rules`,
`rule-set!`, `rule-remove!`, `detect-rule-fires`, and the
`:rule-fired` warning are gone.

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

## Phase H — Secondary-model consultation ✅ LANDED

The consult subsystem.

### SURFACE (primary SCI bindings)

```clojure
(consult-request! :id :preference {:focus [strings] :question "…"})
(consult-promote! :id :fact-key)   ;; copy resolved pin → fact, scrub pin
(consult-dismiss! :id)             ;; drop pin without promoting
```

- `:preference` ∈ `#{:fast :balanced :deep}`
- Map-only signature. `:question` REQUIRED non-blank string;
  `:focus` OPTIONAL vec of strings (consultant returns `:focus-met?`
  vec aligned to it).
- `consult-request!` returns `:vis/silent`; intent is enqueued under
  `:engine/pending-consults` and a side-thread future fires.
- The engine pins the ctx snapshot onto the intent at request time so
  a mutation between request and future-eval is invisible to the
  consult side (thread-isolation gate).

### STORAGE

Resolved consult entries land on `:session/trailer` as synthetic form
pins at iter end:

```
{:scope       "tN/iM/c-K"
 :tag         :consult
 :consult-id  :K
 :src         "(consult-resolved :K)"
 :result      <entry-map>}
```

The pin renders inline with the model's own forms. The model reads
`:result` to inspect `:content` / `:citations` / `:confidence` /
`:focus-met?` etc., then calls `consult-promote!` or
`consult-dismiss!`. No separate ctx section. No `await-consult!`.

### CONSULT MINI-ENGINE

- Lives in `internal/consult_engine.clj`.
- Per intent (on a side thread): fresh SCI ctx with `:consult` scope
  bindings (no engine mutators; `search/web` + `search/papers` +
  read-only `v/cat`/`v/ls`/`v/rg`; `future` for parallel research);
  consult-facing system prompt (`CONSULT_BASE_PROMPT` + auto-gen
  `BINDINGS` block) — NOT `CORE_SYSTEM_PROMPT`; one LLM call via
  `svar/ask-code!`; parse fence; SCI eval; capture single `(done {…})`.
- Same emission point as primary (`done`); only the SCHEMA differs.
- Token-cap retry-to-fit: when `:content` exceeds preference cap
  (`:fast` 1k / `:balanced` 4k / `:deep` 12k tokens) the engine
  re-prompts the same provider ONCE with N (actual)/M (cap)/X
  (overage) numbers. Worst-case 2 LLM calls per intent. Second
  overflow → `:status :failed :error :exceeds-cap :retries 1`.
- Citation caps: max 15 entries; per-citation `:excerpt` tail-clipped
  at 500 tokens.

### DONE GATE

`(done {…})` is REFUSED while `:engine/pending-consults` is non-empty.
`apply-done` returns `{:ctx ctx :blocked? true :warnings […]}` with a
`:done-blocked-by-pending-consults` warning. The loop suppresses
answer shipping and forces a fresh iter so the primary can
promote/dismiss before retrying.

### SCOPE-AWARE BINDINGS

- Every `:ext.sci/symbols` entry carries `:ext.symbol/engine-scope`
  (default `#{:main}`). Primary SCI is wired with `:main`-scoped
  symbols only; consult mini-SCI sees `:consult`-scoped only.
- `exa/*` (web search, code context) is `:consult`-only. Primary
  cannot bind it directly; research routes through `consult-request!`.
- `search/web` + `search/papers` (arxiv Atom) are `:consult`-only via
  `extensions/common/vis-foundation-search`.
- `future` / `realized?` / `deref-of-future` live in `:consult` SCI
  only. Primary is async-by-design; no in-fence futures.

### TRAILER PIN FILTER

Forms whose `:result` is `:vis/silent` do NOT pin. Engine mutators
(`spec-set!`, `task-set!`, `fact-set!`, `consult-request!`, …) become
invisible in the trailer; their effect lives in the ctx subtree.

### LANDED COMMITS

- `8f344b51` scope-aware bindings + trailer pin filter + drop primary future
- `62e9856c` consult-request! + done gate
- `f85bab75` consult-engine mini-SCI runner + unified `(done)` emission
- `577af70f` parallel runner + request-time ctx snapshot pin
- `027f3fc5` token caps + retry-to-fit
- `9572d529` `search/*` consult-scope extension + `exa/*` removed from primary
- `a73748fb` prompt update
- `7136b7b0` integration tests + done-gate enforcement + CLI verify recipe
- `61484f44` trailer-pin storage + drop `await-consult!` + drop reactive rules

### CLI VERIFY

Recipe in `dev/cli-verify/phase-h-prime.md`. Providers authenticated
via `./bin/vis providers status` (anthropic-coding-plan, openai-codex,
github-copilot, zai). Operator runs manually.


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
