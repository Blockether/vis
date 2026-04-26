# Cross-validation of PLAN.md

> Read after PLAN.md. Verifies every load-bearing claim in PLAN.md
> against (a) the actual codebase, (b) svar 0.3.9's internals, and
> (c) Anthropic / OpenAI public documentation. Flags concrete bugs,
> scope undercounts, internal-consistency gaps, and overstatements.
> Items with ✅ are verified; ⚠ need refinement; ❌ are bugs that
> must be fixed before implementation.

---

## A. Plan-level bugs (block implementation if unfixed)

### A.1 ❌ Wrong type keyword: `:spec.type/integer`

PLAN.md §5.1 (`PLAN_ITEM_SPEC :id`) writes:

```clojure
::spec/type :spec.type/integer
```

svar 0.3.9's allowed primitive types are exactly:
`:spec.type/{string,int,float,bool,keyword,ref}` (verified from
`com/blockether/svar/internal/spec.clj:125`). There is no
`:spec.type/integer`. **Use `:spec.type/int`.**

### A.2 ❌ `PLAN_STATE_SPEC` missing its spec name

PLAN.md §5.1 declares:

```clojure
(def PLAN_STATE_SPEC
  (spec/spec
    {:refs [PLAN_ITEM_SPEC]}
    (spec/field {::spec/name :goal …}) …))
```

This top-level form has no name. The `:plan` field on
`make-iteration-spec` targets `:plan_state` (`::spec/target :plan_state`),
which requires the referenced spec to be **named**
(`(spec/spec :plan_state {:refs […]} …)`). Pattern verified against
`PASSAGE_SPEC` (`(spec/spec :passage …)`) at `spec.clj:362`. Without
the name, svar's `build-ref-registry` won't resolve the target and
spec validation throws `:svar.spec/unresolved-ref-target`.

**Fix:** `(spec/spec :plan_state {:refs [PLAN_ITEM_SPEC]} …)`.
Same fix needed for any other named ref the plan introduces.

### A.3 ❌ `make-iteration-spec` refs vector not extended

PLAN.md §5.1 specifies new fields but does not show the change to
`make-iteration-spec`'s final line:

```clojure
(apply spec/spec {:refs [CODE_BLOCK_SPEC NEXT_SPEC]} fields)
```

Must become:

```clojure
(apply spec/spec
  {:refs [CODE_BLOCK_SPEC NEXT_SPEC PLAN_STATE_SPEC]} fields)
```

Without this, svar's spec validator rejects iteration responses
with a `:plan` field because `:plan_state` is not in the registry.
PLAN_ITEM_SPEC does not need to be in the top-level refs because
`build-ref-registry` recursively collects from PLAN_STATE_SPEC's
`:refs`, verified at `internal/spec.clj:436-453`.

### A.4 ❌ `:abandon-reason` referenced but never added to the spec

PLAN.md §10.1 (Phase 6 PEV gate) references
`:final.abandon-reason` as the model's escape hatch when plan
items are open. The field is never added to the iteration spec
in §5.1. Either add it to base-fields:

```clojure
(spec/field {::spec/name :abandon-reason
             ::spec/type :spec.type/string
             ::spec/cardinality :spec.cardinality/one
             ::spec/required false
             ::spec/description
               "Required when emitting :answer with open <plan> items.
                Concrete reason this turn cannot complete the plan."})
```

…or reframe the gate to use `:confidence :low` as the explicit
abandon signal (already in the spec). Recommend adding the new
field — `:confidence :low` is too overloaded.

### A.5 ❌ `:spec.type/int` vector cardinality nuance for `:items`

Plan items use cardinality/many over a ref. svar's spec validator
forbids combining `::target` with non-ref types
(`internal/spec.clj:217`). PLAN's `:items` field correctly uses
`:spec.type/ref + :spec.type/many`, but cite this here so the
implementer doesn't try `:spec.type/int :cardinality/many` for a
flat list of ids — that is allowed, but the ref form is what
PLAN intends.

### A.6 ⚠ Existing docstring contradicts new behavior

`spec.clj:185-198` (the existing `make-iteration-spec` docstring)
ends with:

> The iteration loop ALWAYS sends only the previous iteration's
> `:thinking` under `<prior_thinking>`. There is no spec knob to
> request more — older reasonings live in `(var-history '*reasoning*)`
> …

PLAN.md Phase 1 changes this behavior. The docstring must be
rewritten in the same patch, otherwise an `AGENTS.md` rule will
collide with the new code (`AGENTS.md` already lifts the
"on-demand path is the only path" claim verbatim from this
docstring). **Update both files in the Phase 1 commit.**

---

## B. Scope undercounts

### B.1 ❌ Phase 5 (reasoning-channel compatibility) cost is wrong

PLAN.md estimates 4–5 eng-days. This is **vis-core only**.
Reality:

- svar 0.3.9 has **no** `:capability` system. Branching is on
  `:api-style` (`:openai|:anthropic`), set in
  `internal/llm.clj:441-448`. There is no extension point for
  "native tool-use mode" or "Responses API mode."
- Adding **native tool-use** for Anthropic with thinking enabled
  requires:
  - New request-body builder (tools array, schema-extraction from
    spec, NOT `:json_schema` mode).
  - New response parser (`tool_use` blocks → `:code` array equivalent;
    `thinking` blocks already extracted at
    `internal/llm.clj:248-253` ✅).
  - Multi-turn replay logic that passes the full assistant message
    (including thinking blocks) back unmodified, with
    `cache_control` on the last assistant block.
  - Streaming delta extraction for `tool_use` (different event types
    than `text_delta`).
- Adding **OpenAI Responses API** (`/v1/responses`) requires:
  - A new endpoint in `make-chat-url` (currently hardcoded for chat
    completions / messages).
  - `previous_response_id` chaining state across iterations.
  - Different response shape (`output_items` vs `choices[0].message`).
  - Reasoning-item passthrough.
- Both must remain feature-flagged so the existing spec-JSON path
  keeps working.

**Realistic estimate:** 7–10 eng-days **including svar PRs**, or
2–3 days for just enabling capability-flagged paths if the svar
work is upstreamed independently. Plan should call this out.

### B.2 ⚠ Phase 0 ordering

PLAN.md schedules Phase 0 (metrics) before Phase 1 (plan slot).
But the metric `plan-edit-distance` is meaningless before plan
exists as a structured field. Either:

- Sequence Phase 0 partially-after Phase 1 (define metric stubs
  pre-Phase-1; populate after); or
- Replace `plan-edit-distance` baseline with "presence/absence of
  thinking-chain plan markers" — heuristic, but nonzero.

The other two metrics (`expression-redundancy`,
`var-history-recall-rate`) work today and should ship first.

---

## C. Internal-consistency gaps

### C.1 ⚠ Cancellation does not interact cleanly with sticky plan

When a user cancels mid-turn, the current `iteration` row may have
a partial plan and no final answer. PLAN.md §5.5 (cross-turn
handover) ships "previous turn's final `<plan>`" but a cancelled
turn has no _final_ plan, only the last persisted plan_state.

`conversations/sweep-orphaned-running-queries!` already exists for
crashed queries; extend the same pattern for cancellation:

- On turn-end, if `:status` ∈ `{:cancelled, :error-budget-exhausted}`,
  store a synthetic `:plan_diff "[abandoned at iter N]"` and let
  handover render it with the marker.

### C.2 ⚠ `:abandon-reason` and `:final` interaction with the gate

PLAN.md §10.1 says reject `:final.answer` when plan has open
items unless `:abandon-reason` is set. But `:final` is not a top-
level spec field today — it does not exist in `ITERATION_SPEC_BASE`.
`:answer`, `:answer-type`, `:confidence` are the existing fields.

Either:
- Reframe gate as: reject `:answer` if plan has open items AND
  `:abandon-reason` absent. (Cleanest; matches existing fields.)
- Add a new wrapper `:final` ref spec. (Heavier; not justified.)

Recommend the first. Update Phase 6.1 to use `:answer`.

### C.3 ⚠ `request-more-iterations` vs the gate

When plan has open items and budget exhausted, the model has two
escape paths:
1. `(request-more-iterations N)` from `:code` (existing — extends
   budget).
2. Emit `:answer` + `:abandon-reason` (Phase 6.1 — accepts
   incomplete plan).

System prompt must disambiguate. Suggestion (add to STEP 0/4):

> If budget is short and ≥1 plan item is `:in_progress` with
> visible progress, prefer `(request-more-iterations N)`. If plan
> items are blocked or you have insufficient information, emit
> `:answer` with `:abandon-reason` describing what you'd need.

### C.4 ⚠ `<system_state>` size when previous-turn plan is shipped

§5.5 ships previous turn's final plan in `<system_state>`. A maxed-
out plan is up to 20 items × ~80 chars + goal + open + decided ≈
2-4 KB. Multi-turn conversations could carry several KB of stale
plan context. Bound this:

- Ship only `goal`, `items` summary (`done: N, abandoned: N`), and
  the final `:answer` from prior turn.
- Drop `open` / `decided` from prior turns (current turn doesn't
  need to inherit them).

PLAN's "the plan IS the handover" framing is right, but the full
plan is too much; a digest is enough.

### C.5 ⚠ Dedup map lifetime

PLAN.md §6.1 implies DB-side `(lookup-prior-success db query-id …)`.
Cleaner: in-memory `{(canonical-hash code) {:iter-id … :result …}}`
attached to the existing `call-counts-atom` in
`build-iteration-context`'s call sites. Per-query lifetime, no DB
schema change, no JDBC round-trip per check.

Specify the canonicalization function explicitly:

```clojure
(defn canonical-hash [code-str]
  ;; round-trip through Clojure reader to normalize whitespace +
  ;; equivalent forms; hash the pr-str of the read form.
  (try (-> code-str clojure.tools.reader.edn/read-string pr-str hash str)
    (catch Throwable _ (hash code-str))))
```

Without normalization, `"(grep \"x\")"` and `"(grep  \"x\")"` hash
differently and dedup misses. Falls back to raw string hash on
read failure (e.g. tagged literals not in the EDN reader).

### C.6 ⚠ Schema migration nullability

PLAN.md §5.2 lists `ALTER TABLE iteration ADD COLUMN plan_state TEXT`
etc. SQLite ALTER ADD COLUMN is nullable by default ✅, but
deserialization in
`packages/vis-persistance-sqlite/.../sqlite/core.clj` must handle
NULL → nil, NOT empty-map. Confirm in the migration commit.

### C.7 ⚠ Sub-RLM queries

`SUB_RLM_QUERY_SPEC` was removed (per spec.clj:262 comment), but
sub-RLM queries still exist (search for `(query!` calls). If a
nested query inherits a parent plan, behavior is undefined.
Recommend: nested queries get a fresh plan; the parent's plan
visible only via `<system_state>` carry. Document explicitly.

---

## D. Industry-claim corrections

### D.1 ✅ Verified — Anthropic prompt caching

- 4 explicit cache_control breakpoints max per request.
- 20-block lookback window for incremental cache reuse.
- 5-minute TTL by default since March 2026 (was 1 hour).
- Stable-at-top, variable-at-bottom layout.

Source: docs.anthropic.com/en/docs/build-with-claude/prompt-caching
+ console.anthropic.com/docs/.../tool-use-with-prompt-caching.

### D.2 ✅ Verified — TodoWrite shape

- Schema `{content: string, status: enum[pending,in_progress,completed], activeForm: string}` (PLAN uses `:done` instead of `:completed` and adds `:blocked`; both are reasonable extensions).
- "Use these tools VERY frequently … failure to plan is unacceptable" — verbatim from Claude Code's leaked system prompt.
- Max 20 items, exactly one in_progress.

Source: anthropics/claude-code GH issue #6968 + community
reverse-engineering at claudecn.com.

### D.3 ⚠ "+5% TauBench" claim is GPT-5-specific

PLAN.md §1 claims "+5% TauBench from preserved reasoning." Source
is OpenAI's Responses-API blog post which says **specifically**:
"GPT-5 integrated via Responses scores 5% better on TAUBench
compared to Chat Completions, purely by taking advantage of
preserved reasoning."

Don't generalize: smaller reasoning models (o3-mini, gpt-4o-mini-
reasoning) have not been shown to gain the same delta. Soften the
claim to "OpenAI reports +5% TauBench for GPT-5; expect smaller
gains on lighter models."

### D.4 ⚠ Anthropic "thinking blocks must be replayed" — applies to native tool-use

PLAN.md §1 phrases this generally. Refinement: the rule applies
**when using `tool_use` blocks** — i.e. native tool-calling mode
with thinking enabled. Spec-JSON callers (today's vis) don't trip
the 400 error because there are no `tool_use` blocks for the model
to expect a preceding `thinking` block on. The benefit (reasoning
continuity) is still missed, but the **error-class** differs. Phase
5.1 already handles this correctly.

### D.5 ✅ Verified — interleaved thinking on Claude 4

- Available via `interleaved-thinking-2025-05-14` beta header
  (deprecated for Sonnet 4.6+ which auto-enables it via adaptive
  thinking).
- Allows thinking between tool calls, not only at turn start.
- `budget_tokens` can exceed `max_tokens` in interleaved mode.
- Interleaved thinking is ONLY available with native tool_use,
  not with response-format / structured-output / JSON mode.

Source: docs.anthropic.com/en/docs/build-with-claude/extended-thinking.

This confirms PLAN.md §9.1's premise: getting interleaved thinking
requires switching off spec-JSON for that model class.

### D.6 ✅ Verified — svar already extracts thinking text

`com/blockether/svar/internal/llm.clj:248-253` filters Anthropic
`content[].type == "thinking"` blocks into `:reasoning`. Same for
OpenAI `:reasoning_content` at line 354. So the **content** of
single-pass reasoning lands in `iteration.thinking` today — the
critique's "Anthropic broken by construction" was overstated.
The losses are: (a) interleaved thinking, (b) reasoning continuity
that depends on assistant-role replay, not text. Both real;
neither catastrophic. PLAN's §0 self-correction captures (a) and
(b); could be sharper about why text-replay isn't equivalent to
native-block-replay (TL;DR: cache-hit, signature-validation,
provider-side reasoning state).

---

## E. Edge cases the plan does not address

### E.1 Concurrent turns within a conversation

`conversations/send!` is `locking`'d per conv-id (`conversation/core.clj:200`),
so serial. ✅ No issue. Note this in PLAN for completeness.

### E.2 `:forget` on plan-related vars

`:forget` evicts vars from the SCI sandbox. Plan lives in iteration
row metadata, not as a var. So `:forget '*plan*` does nothing to
the plan slot — but the model might try, and we should respond
informatively. Trivial: log a no-op message.

### E.3 Empty plan with non-empty `:answer`

If the model emits `:answer` with no `:plan` ever set (e.g. trivial
greeting), the gate at §10.1 should pass (no open items). But the
sticky-carry logic at §5.2 must handle "no prior plan" gracefully
(treat as empty plan, not error). Specify in test plan.

### E.4 Token cost vs cache hit on sticky plan

A 5-item plan ≈ 400-800 tokens, sticky after iter 0. Compared to
today's `<prior_thinking>` capped at 4000 chars (≈ 1000 tokens),
**changing every iter**: net cache win on Anthropic with
breakpoint after `<plan>` block. Do quantify in Phase 0 once
metrics land. Worth a one-page memo before Phase 1 commits.

### E.5 What does the model see when plan's `:items` is empty
but `:goal` is set?

Mid-decomposition state. Render as:

```
<plan>
  goal: <one line>
  items: (empty — decomposition pending)
</plan>
```

Don't elide the block; the explicit "decomposition pending" is a
nudge for the model to populate items.

---

## F. Verified-correct items (no action needed)

- ✅ svar's spec system supports `:spec.type/ref` with
  `:spec.cardinality/many` and named refs. Pattern `PASSAGE_SPEC`
  /`CHUNK_SELECTION_SPEC` matches PLAN's intent.
- ✅ Nested refs are recursively collected by `build-ref-registry`
  (verified at `internal/spec.clj:436+`).
- ✅ `:spec/values` enums work with both string-vector and
  `{value→description}` map (per `VERIFICATION_RESULT_SPEC :verdict`).
- ✅ The trailing user block adds 2 message blocks per iter
  (1 user + 1 assistant), well under Anthropic's 20-block lookback.
  PLAN's "ensure ≤15 blocks" was over-cautious; current loop is
  trivially under.
- ✅ Anthropic `thinking` blocks AND OpenAI `reasoning_content`
  are extracted by svar today and flow into
  `iteration.thinking` → `<prior_thinking>`. The reasoning text is
  not lost; only the assistant-role replay is.
- ✅ `expression_state` rows persist `:expr` text — Phase 2 dedup
  can index by hash of `:expr` directly, no schema change needed if
  using the in-memory map (§C.5).
- ✅ `conversations/send!` is per-conv serial — no concurrent-plan
  race condition.
- ✅ Schema columns in §5.2 are nullable via SQLite's default
  ALTER ADD COLUMN behavior.
- ✅ `(:tokens ask-result)` includes `:cached`, so the
  `cache-hit-rate` metric in §11.3 is implementable from existing
  svar return shape.

---

## G. Summary: changes required before Phase 1 commit

1. Fix `:spec.type/integer` → `:spec.type/int` (A.1).
2. Name `PLAN_STATE_SPEC` as `:plan_state` (A.2).
3. Extend `make-iteration-spec` `:refs` vector (A.3).
4. Add `:abandon-reason` field to spec (A.4).
5. Update `make-iteration-spec` docstring (A.6) AND the matching
   AGENTS.md paragraph (`<prior_thinking>` rule).
6. Re-estimate Phase 5: 7-10 eng-days including svar PRs (B.1).
7. Re-sequence Phase 0 metrics so `plan-edit-distance` lands
   after Phase 1 lifts the structured plan into existence (B.2).
8. Specify dedup canonicalization explicitly (C.5).
9. Reframe Phase 6.1 gate to use `:answer` (existing field) +
   `:abandon-reason` instead of imaginary `:final.answer` (C.2).
10. Add "request-more-iterations vs abandon" disambiguation to
    the new system prompt (C.3).
11. Bound previous-turn plan in `<system_state>` to a digest, not
    full body (C.4).
12. Soften "+5% TauBench" claim to GPT-5-specific (D.3).

None of these are showstoppers. All are surgical edits to PLAN.md
plus one or two extra spec fields. Phase 1 is implementable as
outlined once these corrections land.
