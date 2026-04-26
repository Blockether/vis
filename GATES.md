# Implementation Gates

Per-phase acceptance criteria. Each gate has:
- **Code-level** — what compiles and exists.
- **Test gate** — automated test that must pass.
- **Behavior gate** — visible to a user / agent run.

A phase is **complete** when all three categories pass.

---

## Phase 0a — Provider-agnostic metrics

### Code
- `iteration` row metadata JSON includes:
  - `:expression-redundancy-fraction` (double 0–1)
  - `:var-history-recall-count` (int)
  - `:dedup-saves` (int — populated in Phase 2; default 0)

### Test
- `metrics_test.clj` — invoking `iteration-metrics` over a fake
  expression list returns the correct redundancy fraction.

### Behavior
- `vis run --json "..."` output's `:metrics` map contains the three
  counters per iteration.

---

## Phase 1 — Plan as first-class slot

### Code
- `spec.clj` exports `PLAN_ITEM_SPEC`, `PLAN_STATE_SPEC` (named refs).
- `make-iteration-spec` includes `:plan`, `:breadcrumb`,
  `:abandon-reason` fields in **both** variants. Top-level `:refs`
  vector includes `PLAN_STATE_SPEC`.
- New migration `V2__plan_slot.sql` adds:
  - `iteration.plan_state TEXT`
  - `iteration.breadcrumb TEXT`
  - `iteration.plan_diff TEXT`
  - `query_state.prior_outcome TEXT` (for cross-turn handover digest)
  - All NULLABLE.
- `store-iteration!` persists plan_state / breadcrumb / plan_diff.
- `db-list-query-iterations` returns plan_state / breadcrumb when present.
- New `effective-plan-for-query` helper in
  `loop.runtime.conversation.environment.query.core`.
- `build-iteration-context` rewrites trailing block to layered sections:
  `[iter N/M]` `<plan>` `<breadcrumbs>` `<recent>` `<recent_thought>`
  `<system_state>` `<var_index>` `[system_nudge]`.
- `CORE_SYSTEM_PROMPT` rewritten to STEP 0–4 framing.
- Cross-turn handover replaced by digest in `<system_state>` (drops
  `HANDOVER_KEEP_LAST` constant + iter-0 special branch).
- `sweep-orphaned-running-queries!` extended for cancellation outcome.

### Test
- `plan_slot_test.clj`:
  - `plan_state` round-trips through DB store + load.
  - Sticky-carry: when iter K omits `:plan`, the projection at iter K+1
    uses iter K-1's plan.
  - Plan-diff: changing one item's `:status` from `:pending`
    → `:in_progress` produces a diff with that item id.
  - Cross-field validation rejects: 21-item plan, two `:in_progress`
    items, non-monotonic `:id`.
  - Empty-plan greeting: emitting `:answer` with no `:plan` set ever
    is allowed (gate is plan-completion, not plan-existence).
- `system_prompt_test.clj`: rendered prompt contains "STEP 0 — PLAN"
  and `<style_appendix>` block.

### Behavior
- `vis run "list every defmacro in src and emit their arities"` —
  `<plan>` block visible in DB iteration row from iter 1 onward;
  `plan-edit-distance` (Phase 0b) ≤ 0 on ≥80% of iters after iter 0.

---

## Phase 0b — Plan-edit-distance metric

Ships with Phase 1.

### Code
- `iteration.metadata` includes `:plan-edit-distance` (int — count of
  added/removed/status-changed item ids vs. previous iter).
- `:plan-changed?` (bool — convenience).

### Test
- `plan_metrics_test.clj` — synthesizing two iter rows with one item
  status changed produces `:plan-edit-distance 1, :plan-changed? true`.

### Behavior
- Telemetry event `iteration.plan.diff` fired when distance > 0.

---

## Phase 2 — Attempts ledger + auto-dedup

### Code
- New helper `canonical-hash [code-str]` in
  `iteration.core` — round-trips through fast-edn reader; falls
  back to raw-string hash on failure.
- Per-query in-memory dedup atom (`{hash → {:iter-id :result :stdout}}`)
  attached to loop state.
- `run-iteration` checks dedup before SCI execution; on hit, returns a
  synthetic `:result` with `:cached-from "iN.K"` annotation.
- `<attempts>` block in `build-iteration-context` lists last 50 distinct
  successful calls with `iN.K` ids.
- `REPETITION_THRESHOLD` lowered to 1.

### Test
- `dedup_test.clj`:
  - `(canonical-hash "(grep \"x\")") == (canonical-hash "(grep  \"x\")")`
    (whitespace equivalence).
  - Tagged-literal fallback returns a deterministic hash without
    throwing.
  - Identical successful calls in iters 2 and 5 short-circuit the
    second call (verified by mock SCI runner counting invocations).
  - Errors and timeouts are NOT cached.

### Behavior
- Stress task that the model previously looped on now shows
  `:dedup-saves > 0` in iteration metadata.
- `expression-redundancy-fraction < 0.05` on
  `vis run "find every defmacro in src"`.

---

## Phase 3 — Value-bearing `<var_index>`

### Code
- `render-var-form` rewritten with type-aware rendering:
  - `:string` ≤200c → inline literal
  - `:map` ≤8 keys → `{:keys […]}`
  - `:vector|:set|:list` ≤5 elems → `[a b c …]`
  - `:fn` → arglists + first docstring line
  - persisted-only → archive marker
- SYSTEM vars NO LONGER sort first in `<var_index>` (they're inlined
  in `<system_state>` from Phase 1).
- New `<vars_archive>` block lists `:forget`'d / persisted-only var
  names + version counts.

### Test
- `var_index_render_test.clj`:
  - Each type renders as specified above (8 cases).
  - SYSTEM vars absent from `<var_index>`.
  - Forgotten var appears in `<vars_archive>`, not `<var_index>`.

### Behavior
- Trailing block visibly contains values, not just shapes, for cheap
  vars on a real run.

---

## Phase 4 — Tool surface widening

### Code
- New `(fs/patch-files [{:path :patch} …])` — atomic multi-file patch.
- `read-file` default: full content for files <8KB; head 4KB + tail
  1KB + size summary for larger; explicit offset/limit overrides.
- `list-files` default depth `:auto` — full tree if total entries
  <100, else depth=2.
- New `(fs/find-symbol 'name)` — returns
  `[{:path :line :arglists :doc}]`.
- Tool errors re-attach `:arglists` + `:examples` inline in the
  journal entry (`format-expression-results` error branch).
- Strict argument validation — wrong shape rejects with structured
  ex-info containing `:expected` + `:got` instead of silently coercing.

### Test
- `editing_extension_test.clj`:
  - `patch-files` rolls back ALL paths on any-patch failure.
  - `read-file` on small file returns full content (no preview tag).
  - `list-files` default depth respects entry-count threshold.
  - `find-symbol` finds `defn`, `def`, `defmacro` forms.
  - Wrong-shape `read-file` call returns structured error with
    `:expected` keys.

### Behavior
- Refactor task touching 3 files completes in 1 iter (was 3 iters).
- `(fs/find-symbol 'foo-fn)` returns the right entry on a real repo.

---

## Cross-cutting

### Doc gates (block PR merge)
- `AGENTS.md` `<prior_thinking>` rule rewritten.
- `docs/src/architecture/iteration-flow.md` updated with the new
  trailing-block schema.
- PLAN.md and CRITIQUE.md remain as historical docs (no edits
  required after implementation).

### Test runner gate
- `clojure -M:test` passes everything (new + existing tests).
- Targeted test subset for each phase exits 0 in <30 s.

### Manual smoke gate (each phase)
- `bin/vis run "..."` completes a representative task without
  throwing.
- TUI session for 5+ iterations renders all sections without
  visual artifacts.
