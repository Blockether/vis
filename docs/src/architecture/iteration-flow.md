# Iteration Flow

What happens when the user sends a message, end to end.

## Sequence

```
user message
  → conversation/core.clj :: send!         acquire per-conversation lock; build history
  → query/core.clj       :: query!         validate; store query; enter loop
      loop:
        1. Build Context
        2. Ask LLM
        3. Execute Code
        4. Persist + Decide
             — forms ran, no `(answer …)` call  → back to step 1
             — `(answer …)` was called           → return answer + metadata
             — error                              → feed error as user msg, back to step 1
```

**Step details:**

1. **Build Context** — `<plan>` (sticky structured TODO list), `<breadcrumbs>` (last K=20 one-liners), `<recent>` (last iteration's expressions with `iN.K` ids), `<recent_thought>` (last iteration's `:thinking`), `<system_state>` (`QUERY` / `ANSWER` / `REASONING` / `PRIOR_TURN`), `<var_index>` (user-defined vars only — SYSTEM vars now live in `<system_state>`), nudges (built-in + extension + loop-injected)
2. **Ask LLM** — plain-text completion + fenced code-block
   extraction. The call site is
   `(svar/ask-code! (:router environment) {:lang "clojure" …})`.
   svar sends `:messages` verbatim, parses the assistant response,
   filters Clojure-tagged + untagged fences, returns the concatenated
   source. NO JSON spec, NO schema validation, NO schema-reject
   retry layer; reader errors on the extracted source flow as
   ordinary iteration errors instead. The router is the env's
   snapshot router — NOT the global `query-core/router-atom`. See
   [Router Lifecycle](state.md#router-lifecycle) for why this matters
   when provider/model is switched mid-session.
3. **Execute Code** — lint, SCI eval with timeout, capture stdout/stderr/result per block
4. **Persist + Decide** — `db-store-iteration!`, attach extension metadata, route to next step

## System prompt assembly

`prompt/assemble-system-prompt` is the **single source of truth** for
the system message content. Both iteration loop paths and the TUI
`[?]` inspector call it. It composes:

1. **Core instructions** (`CORE_SYSTEM_PROMPT`) — the agent contract:
   reply with Clojure source inside ```clojure … ``` fences, use
   `;; comments` for thinking, call `(answer …)` to finish.
2. **Extension prompts** — each active extension’s canonical
   symbol-derived prompt block, prefixed with `[namespace: alias → ns]`,
   plus any optional extra `:ext/prompt` tail (cwd / OS / git facts
   come from the `vis-common-environment` extension, not from the
   runtime).

No iteration spec schema is appended anymore. svar’s `ask-code!`
appends one short `code-tail-pointer` reminder as the LAST text
block of the LAST user message (`Reply with clojure source inside
 ` ` ` clojure …  ` ` ` fences …`); that lives outside the system
message and is NOT cached.

## Error recovery

When an iteration throws:

1. If the error is **infrastructure** (router down, DB closed, JVM): re-throw — the turn aborts.
2. Otherwise: normalize the error and append it as the next user message; continue the loop.
3. A `[system_nudge]` fires at `CONSECUTIVE_ERROR_NUDGE_AT` (=2) consecutive errors so the model gets one warning to re-emit `:plan` with a different strategy.
4. If consecutive errors reach `max-consecutive-errors` (default **3**, overridable via `:max-consecutive-errors`), the loop attempts a **strategy restart** — fresh prompt assembly + reset counters — up to `max-restarts` (default **3**, overridable via `:max-restarts`).
5. After the final restart still fails, the turn ends with `:status :error`.

## Final answer

The model finishes a turn by calling `(answer "…")` from inside a
fenced code block. The runtime binds `answer` as a one-arg SCI fn
that `reset!`s an environment-scoped atom; after evaluating every
form in the iteration, the loop reads the atom and — if non-nil and
no expression in the same iteration errored — commits the captured
string as the turn's final answer.

```clojure
```clojure
(let [summary (clojure.string/join "\n" rendered-rows)]
  (answer (str "# Done\n\n" summary)))
```
```

Key properties:

- `(answer …)` is a real Clojure call, so `(str …)`, `(format …)`,
  `(pr-str …)`, etc. all interpolate. No Mustache layer, no template
  language. Whatever `(str <arg>)` produces becomes the answer.
- Calling `(answer …)` more than once in the same iteration: the LAST
  call wins. The atom is reset to `nil` at the start of every
  iteration.
- If any form in the same iteration errors, the captured answer is
  discarded and the loop retries.

## Loop termination

The loop runs **until the model calls `(answer …)`**, the user
cancels, or the consecutive-error budget is exhausted. The model
decides when it's done by finalizing; the user decides when to bail
by cancelling.

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
