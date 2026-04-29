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

1. **Build Context** — `<plan>` (sticky structured TODO list), `<breadcrumbs>` (last K=20 one-liners), `<recent>` (last iteration's expressions with `iN.K` ids), `<recent_thought>` (last iteration's `:thinking`), `<system_state>` (`QUERY` / `ANSWER` / `REASONING` / `PRIOR_TURN`), `<var_index>` (user-defined vars only — SYSTEM vars now live in `<system_state>`), nudges (built-in + extension + loop-injected)
2. **Ask LLM** — svar structured JSON output: code blocks + optional
   `:final`. The call site is `(llm/ask! (:router environment) …)`,
   i.e. the env's snapshot router — NOT the global
   `query-core/router-atom`. See
   [Router Lifecycle](state.md#router-lifecycle) for why this matters
   when provider/model is switched mid-session.
3. **Execute Code** — lint, SCI eval with timeout, capture stdout/stderr/result per block
4. **Persist + Decide** — `store-iteration!`, attach extension metadata, route to next step

## System prompt assembly

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

The iteration spec schema lives with the iteration engine in
`com.blockether.vis.core`.
svar’s `spec->prompt` appends that schema separately as a final user
message — it is NOT part of the system message.

## Error recovery

When an iteration throws:

1. If the error is **infrastructure** (router down, DB closed, JVM): re-throw — the turn aborts.
2. Otherwise: normalize the error and append it as the next user message; continue the loop.
3. A `[system_nudge]` fires at `CONSECUTIVE_ERROR_NUDGE_AT` (=2) consecutive errors so the model gets one warning to re-emit `:plan` with a different strategy.
4. If consecutive errors reach `max-consecutive-errors` (default **3**, overridable via `:max-consecutive-errors`), the loop attempts a **strategy restart** — fresh prompt assembly + reset counters — up to `max-restarts` (default **3**, overridable via `:max-restarts`).
5. After the final restart still fails, the turn ends with `:status :error`.

## Finalize gates

Before accepting a `:final` answer, the loop applies two gates:

- **Open-plan gate (PEV):** if any plan item is still `:pending` or
  `:in-progress`, `:answer` is rejected unless `:abandon-reason` is set.
- **Confidence gate:** if `:confidence` is `:low`, `:answer` is rejected
  unless `:abandon-reason` explains what would raise confidence.

Rejected finals are turned into `[system_nudge]` messages for the next
iteration and retried with the same bounded gate retry budget.

## Final answer rendering

One mode: every `:answer` is rendered as a **Mustache template against
sandbox vars** (markdown content allowed). There is no `:answer-type`
field, no auto-detect, no separate "SCI expression" mode.

- Plain text / markdown without `{{…}}` tags renders verbatim, so
  prose answers pass through unchanged.
- `{{var}}`, `{{#list}}…{{/list}}`, `{{^val}}…{{/val}}`, `{{.}}`,
  `{{list.size}}` interpolate sandbox vars. Missing referenced vars
  surface as a re-prompted validation error.
- To inject a **computed** value, define it in `:code` and reference
  the var in `:answer`:

  ```clojure
  ;; :code
  (def summary (clojure.string/join "\n" rendered-rows))
  ;; :answer
  "{{summary}}"
  ```

## Loop termination

The loop runs **until the model emits `:answer`**, the user cancels,
or the consecutive-error budget is exhausted. The model decides when
it's done by finalizing; the user decides when to bail by
cancelling.

The consecutive-error budget (`max-consecutive-errors`, default 3,
with `max-restarts` strategy resets, default 3) is the only
automatic termination path apart from finalize / cancel.

## Plan, breadcrumbs, recent thought

Reasoning continuity is delivered by **three structured slots**, not by
a lossy summarization chain:

- **`<plan>`** — sticky `:plan_state` map. The model emits it at iter 0
  (or whenever the approach changes); the loop carries the most-recent
  persisted plan forward verbatim until the model re-emits one.
  Schema: `:goal` / `:items [{:id :content :status :evidence}]` /
  `:open` / `:decided`. Max 20 items, exactly one `:in-progress`.
- **`<breadcrumbs>`** — cumulative one-liner per iteration, authored by
  the model in `:breadcrumb`. Bounded at last K=20 entries, oldest-first.
  Tactical "what I just did" rendered as `i3  [3] grep yielded 12 hits`.
- **`<recent_thought>`** — the most recent iteration's free-form
  `:thinking` text, capped at 4000 chars. For nuance the breadcrumb
  couldn't carry.

For deeper introspection, the opt-in `vis-common-meta` extension exposes
`(meta/turn)`, `(meta/conversation)`, `(meta/conversations)`,
`(meta/var-history 'sym)`, `(meta/find-attempts pattern)`,
`(meta/failures)`, and `(meta/diagnose)`. See
[Meta extension](../extensions/common/vis-common-meta.md).

## SYSTEM vars

`QUERY`, `REASONING`, `ANSWER` are read-only sandbox bindings
carrying the current user request, the model's last reasoning text,
and the prior turn's final answer. UPPERCASE marks them as constants;
the registry `SYSTEM_VAR_NAMES = #{QUERY ANSWER REASONING}` is fixed.
See `loop.runtime.conversation.environment.core/system-var-sym?` for
the predicate. SYSTEM vars are excluded from `<var_index>` (their
current values appear inlined in `<system_state>` instead) and are
not subject to auto-forget.

## Cross-turn handover digest

At iteration 0 of turn N, `<system_state>.PRIOR_TURN` carries a
**bounded digest** of the previous turn: `{:goal :counts :outcome
:abandon-reason}`. Only this digest — not the full plan body, not raw
reasoning, not the transcript. Multi-turn conversations cannot
accumulate stale plan context here. The next turn's plan is fresh.

## Var index

`<var_index>` is the latest namespace snapshot of *user-defined* vars
only. SYSTEM vars do NOT appear here — they live in `<system_state>`.

Rendering is **type-aware**: cheap values get their actual content
inlined so the model never has to round-trip via `(var-history 'sym)`
just to read what's in a var. Expensive values fall back to a schema
preview. Stats live in a `;;` comment line, **never** as reader-macro
metadata onto the symbol.

```
;; v=3 scope=live n=2
(def s "hello")
;; v=1 scope=live n=12
(def m {:keys [:status :id :name :owner :created-at :updated-at :tags :owner-id]})
;; v=1 scope=live n=42
(def big-map {:n 42 :keys-sample [:a :b :c :d :e :f]})
;; v=2 scope=live n=4
(def callers [{:path "src/x.clj"} {:path "src/y.clj"} …])
;; v=1 scope=live
(defn foo-fn [x opts] "Update foo-fn arity. …")
```

Rules:

- `:string ≤200c` → inline literal.
- `:string >200c` → `{:string-size N :head "first 80c…"}`.
- `:map ≤8 keys` → `{:keys […]}`.
- `:map  >8 keys` → `{:n N :keys-sample […]}`.
- `:vector|:set|:list|:seq ≤5 elems` → inline `[…]`.
- `:vector|:set|:list|:seq >5 elems` → `{:n N :head […]}`.
- `:fn` → arglists + first docstring line.
- `:nil|:bool|:int|:float|:keyword|:symbol` → inline literal.

Forgotten / persisted-only vars (not currently live in the sandbox
but present in the DB var registry) are routed to a separate
`<vars_archive>` subblock with name + version count only —
`(var-history 'sym)` recovers the full history on demand.

```
<vars_archive>
  archived-var  ;; v=3 (call (var-history 'archived-var) to inspect)
</vars_archive>
```
