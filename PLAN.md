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

## Phase D — Reactive triggers (forward chaining) ❌

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

## Phase E — Proof composition ❌

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

## Phase F — Trailer FTS index ❌

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

## Phase G — Self-Discover reasoning structure ❌

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

## Phase H — Verbal reflection on archive entries ❌

**Goal.** Extend Phase A `:archived-proofs` entry with a natural-
language `:reflection` field. On rejection, engine triggers a tiny
LLM call (router with `:reasoning :minimal` budget) that produces a
one-paragraph summary: "what was tried, why it failed, what to try
next". Reflection is stored on the entry and surfaced in the
rendered `;; rejected-proofs` line.

Reflexion paper shows verbal summary > structured codes for
semantic-gradient signal.

**Paper.** Reflexion (Shinn NeurIPS 2023, arxiv 2303.11366), section
on episodic-memory text vs scalar feedback.

**Tests.**
- `reflection-generation-test` — mock LLM client, archive entry gets
  `:reflection` non-empty string
- `reflection-budget-test` — fails open (no reflection field) when
  LLM call times out or errors
- `reflection-rendering-test` — `;; rejected-proofs` line includes
  reflection prefix when present

**CLI verify.**
```bash
./bin/vis 'Stwórz spec z validatorem (= result 42).
Wyemituj proof którego forma zwraca 7. Następnie pokaż archived-proof
z polem :reflection.'
```

---

## Phase I — Fact semantic search (Voyager) — DEFERRED

**Goal.** Per-fact embedding + `(fact-search "query")` cosine top-K.

**Status.** DEFERRED in favour of FTS5 (Phase F). If FTS5 proves
insufficient for semantic recall, reopen this phase; otherwise drop.

---

## Phase J — Knowledge-graph triples — MERGED INTO B

**Status.** Subsumed by universal `:depends-on` (Phase B). Relations
are typed edges in the dep-graph; no separate triple store needed.

---

## Phase K — Probabilistic validators ❌

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
