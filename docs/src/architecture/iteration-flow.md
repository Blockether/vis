# Iteration Flow

What happens when the user sends a message, end to end.

## Sequence

```
user message
  → conversation/core.clj :: send!         acquire per-conv lock; build history
  → query/core.clj       :: query!         validate; store query; enter loop
      loop:
        1. Build Context
        2. Ask LLM
        3. Execute Code
        4. Persist + Decide
             — has :code, no :final  → back to step 1
             — :final present        → return answer + metadata
             — error                 → feed error as user msg, back to step 1
```

**Step details:**

1. **Build Context** — `<plan>` (sticky structured TODO list), `<breadcrumbs>` (last K=20 one-liners), `<recent>` (last iteration's expressions with `iN.K` ids), `<recent_thought>` (last iteration's `:thinking`), `<system_state>` (`QUERY` / `ANSWER` / `REASONING` / `ITERATION` / `PRIOR_TURN` — ITERATION carries `{:current :budget :remaining}` and replaces the legacy standalone `[iter N/M]` header), `<var_index>` (user-defined vars only — SYSTEM vars now live in `<system_state>`), nudges (built-in + extension + loop-injected)
2. **Ask LLM** — svar structured JSON output: code blocks + optional
   `:final`. The call site is `(llm/ask! (:router environment) …)`,
   i.e. the env's snapshot router — NOT the global
   `query-core/router-atom`. See
   [Router Lifecycle](state.md#router-lifecycle) for why this matters
   when provider/model is switched mid-session.
3. **Execute Code** — lint, SCI eval with timeout, capture stdout/stderr/result per block
4. **Persist + Decide** — `store-iteration!`, attach extension metadata, route to next step

## System Prompt Assembly

`loop-core/assemble-system-prompt` is the **single source of truth** for
the system message content. Both iteration loop paths and the TUI
`[?]` inspector call it. It composes:

1. **Core instructions** (`CORE_SYSTEM_PROMPT`) — iteration steps
   (READ/COMPUTE/PERSIST/FINALIZE), Mustache docs, grounding rule,
   query primacy, perf hints, tool discipline, CLJ rules, output voice
2. **Date + environment block** — CWD, home, user, platform, shell
3. **Extension prompts** — each active extension’s canonical
   symbol-derived prompt block, prefixed with `[namespace: alias → ns]`,
   plus any optional extra `:ext/prompt` tail

The iteration spec schema (svar’s `spec->prompt`) is appended separately
by svar as a final user message — it is NOT part of the system message.

## Error Recovery

When an iteration throws:

1. If the error is **infrastructure** (router down, DB closed, JVM): re-throw — the turn aborts.
2. Otherwise: normalize the error and append it as the next user message; continue the loop.
3. If consecutive errors reach **5**, attempt a **strategy restart** (fresh prompt assembly, reset call counts). Up to **3** restarts.
4. After 3 failed restarts, the budget is considered exhausted and the turn ends with `:status :error`.

## Budget Extension

The default budget is **4 iterations** — deliberately tight so the LLM
must plan. When more work is genuinely needed, the LLM calls
`(request-more-iterations n)` from `:code` to extend on demand.
There is **no cap** on how high the budget can grow.

This is especially important when a budget `[system_nudge]` fires.
The intended behavior is: read the nudge, decide whether more work is
actually needed, and if yes call `(request-more-iterations n)`
immediately instead of limping into a bad finalize.

## Plan slot, breadcrumbs, recent thought

Reasoning continuity is delivered by **three structured slots**, not by
a lossy summarization chain:

- **`<plan>`** — sticky `:plan_state` map. The model emits it at iter 0
  (or whenever the approach changes); the loop carries the most-recent
  persisted plan forward verbatim until the model re-emits one.
  Schema: `:goal` / `:items [{:id :content :status :evidence}]` /
  `:open` / `:decided`. Max 20 items, exactly one `:in_progress`.
- **`<breadcrumbs>`** — cumulative one-liner per iteration, authored by
  the model in `:breadcrumb`. Bounded at last K=20 entries, oldest-first.
  Tactical "what I just did" rendered as `i3  [3] grep yielded 12 hits`.
- **`<recent_thought>`** — the most recent iteration's free-form
  `:thinking` text, capped at 4000 chars. For nuance the breadcrumb
  couldn't carry.

Older reasonings are no longer auto-shipped. When the model genuinely
needs them, the (opt-in) `vis-ext-self-debug` extension exposes
`(self/breadcrumbs N)` and `(self/turn-history N)` for programmatic
introspection without wasting iterations on `(var-history)` round-trips.

## SYSTEM vars: `QUERY`, `REASONING`, `ANSWER`

The sandbox-visible system vars carrying the user's current query, the
model's last reasoning text, and the prior-turn final answer are named
**`QUERY`, `REASONING`, `ANSWER`** — ALL CAPS, no earmuffs. They are
explicitly defined at environment construction (`(def QUERY nil)` etc.)
so the symbols always resolve, even before the first turn.

Uppercase, not earmuffs, because:

- Earmuffs are Clojure's idiom for *dynamic vars* (`*out*`, `*ns*`).
  These are plain SCI bindings, not dynamic; the earmuff signal misled
  readers into thinking they could `binding`-shadow them.
- Uppercase aligns with the Clojure idiom for *constants*
  (`MAX_VAL`, `URL_PATTERN`). The SYSTEM vars are read-only from the
  model's POV; the loop owns mutation.

The registry is fixed: `SYSTEM_VAR_NAMES = #{QUERY ANSWER REASONING}`.
See `loop.runtime.conversation.environment.core/system-var-sym?` for
the predicate. SYSTEM vars are *excluded* from `<var_index>` (their
current values appear inlined in `<system_state>` instead) and never
subject to auto-forget.

## Cross-turn handover digest

At iteration 0 of turn N, `<system_state>.PRIOR_TURN` carries a
**bounded digest** of the previous turn: `{:goal :counts :outcome
:abandon-reason}`. Only this digest — not the full plan body, not raw
reasoning, not the transcript. Multi-turn conversations cannot
accumulate stale plan context here. The next turn's plan is fresh.

## Var-index

`<var_index>` is the latest namespace snapshot of *user-defined* vars
only. When a symbol shows `:v N`, the full persisted version timeline
is available via `(var-history 'sym)`. SYSTEM vars do NOT appear in
`<var_index>` — they live in `<system_state>` with their current values
inlined.
