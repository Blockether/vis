# Iteration Flow

What happens when the user sends a message, end to end.

## Sequence

```
user message
  → internal/loop.clj :: send!         acquire per-conversation lock; build history
  → internal/loop.clj :: run-turn!     validate; store turn; enter loop
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

1. **Build Context** — `<journal>` (last iteration's blocks with `iN.K` ids), `<var_index>` (user-defined vars only). Plus the SCI-bound SYSTEM vars (every name in `SYSTEM_VAR_NAMES` — `TURN_USER_REQUEST`, `TURN_CONVERSATION_TURN_ID`, `TURN_CONVERSATION_SOUL_ID`, `TURN_CONVERSATION_STATE_ID`, `TURN_SYSTEM_PROMPT`, `TURN_ACTIVE_EXTENSIONS`, `ITERATION_ID`, `ITERATION_PREVIOUS_REASONING`, `CONVERSATION_TITLE`, `CONVERSATION_PREVIOUS_ANSWER`) the model can read directly. Active extensions may append one `[system_nudge]` line each via `:ext/nudge-fn`.
2. **Ask LLM** — plain-text completion + fenced code-block
   extraction. The call site is
   `(svar/ask-code! (:router environment) {:lang "clojure" …})`.
   svar sends `:messages` verbatim, parses the assistant response,
   filters Clojure-tagged + untagged fences, returns the concatenated
   source. NO JSON spec, NO schema validation, NO schema-reject
   retry layer; reader errors on the extracted source flow as
   ordinary iteration errors instead. The router is the env's
   snapshot router — NOT the global loop router atom. See
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
   come from the `vis-foundation` extension, not from the
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
3. A `[system_nudge]` fires at `CONSECUTIVE_ERROR_NUDGE_AT` (=2) consecutive errors so the model gets one warning to change strategy.
4. If consecutive errors reach `max-consecutive-errors` (default **3**, overridable via `:max-consecutive-errors`), the loop attempts a **strategy restart** — fresh prompt assembly + reset counters — up to `max-restarts` (default **3**, overridable via `:max-restarts`).
5. After the final restart still fails, the turn ends with `:status :error`.

## Final answer

<a id="answer-protocol"></a>

The model finishes a turn by calling `(answer "…")` from inside a
fenced code block. The runtime binds `answer` as a one-arg SCI fn
that `reset!`s an environment-scoped atom; after evaluating every
form in the iteration, the loop reads the atom and — if non-nil and
no expression in the same iteration errored — commits the captured
string as the turn's final answer.

The canonical way to build the answer body is the `v/` surface
(see [Markdown builders under `v/`](../extensions/common/markdown.md)):

```clojure
(answer
  (v/join
    (v/h1 "Done")
    (v/p "Three files touched.")
    (v/ul (map #(v/file-link % nil) touched-paths))))
```

Key properties:

- `(answer …)` is a real Clojure call, so `(str …)`, `(format …)`,
  `(pr-str …)`, etc. all interpolate. No Mustache layer, no template
  language. Whatever `(str <arg>)` produces becomes the answer.
- Use `v/` helpers (`v/h1`, `v/table`, `v/file-link`, …) to
  assemble the body — they keep formatting consistent across
  channels and surface clickable links the TUI can follow.
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

## Reasoning continuity

There is no projection layer carrying "plan" or "breadcrumb" data
between iterations. Continuity is delivered by:

- **`<journal>`** — the previous iteration's code blocks + results,
  addressable by `iN.K` ids.
- **`<var_index>`** — user-defined `(def …)` bindings, type-aware
  rendering (see below).
- **SCI bindings** — the SYSTEM vars (every name in `SYSTEM_VAR_NAMES`
  — see [SYSTEM vars](#system-vars) below) the model can read directly
  from inside a fenced code block.

For deeper introspection, the opt-in `vis-foundation` extension
exposes `(v/inspect)` as the canonical state data map and `(v/report)`
as the Markdown renderer over the same data. See
[Meta extension](../extensions/common/vis-foundation.md).

## SYSTEM vars

The registry `SYSTEM_VAR_NAMES` is fixed. Names are split across three
prefix-tagged lifetime tiers:

- **`TURN_*`** — frozen at turn start, immutable for the whole turn:
  - `TURN_USER_REQUEST` — user's current message text.
  - `TURN_CONVERSATION_TURN_ID` — UUID of THIS in-flight turn (== conversation turn soul id).
  - `TURN_CONVERSATION_SOUL_ID` — UUID of the `conversation_soul` row.
  - `TURN_CONVERSATION_STATE_ID` — UUID of the latest `conversation_state` row at turn start.
  - `TURN_SYSTEM_PROMPT` — the full assembled system prompt driving THIS turn.
  - `TURN_ACTIVE_EXTENSIONS` — frozen vec of compact extension descriptors (`:alias`, `:namespace`, `:doc`, `:kind`, `:version`, `:author`, `:owner`, `:license`, `:registry-id`, `:source-paths`, `:source-mtime-max`, `:source-hash-sha256`, `:symbols`, `:docs`).
- **`ITERATION_*`** — rebound at every iteration boundary:
  - `ITERATION_ID` — UUID of the most recently persisted iteration (nil before iter 1).
  - `ITERATION_PREVIOUS_REASONING` — last iteration's `:thinking` text.
- **`CONVERSATION_*`** — conversation-state, mutates freely within the turn:
  - `CONVERSATION_TITLE` — current conversation title ("" until set).
  - `CONVERSATION_PREVIOUS_ANSWER` — previous turn's final answer string.

UPPERCASE marks them as constants. See
`com.blockether.vis.core/system-var-sym?` for the predicate. SYSTEM
vars are excluded from `<var_index>` and are not subject to
auto-forget — the model reads them as plain SCI symbols.

`TURN_SYSTEM_PROMPT` carries the full assembled system prompt as a
multi-KB string; because SYSTEM vars are excluded from `<var_index>`,
the binding costs zero per-iteration tokens. It only enters context
when the model evaluates the symbol explicitly.

## Var index

`<var_index>` is the latest namespace snapshot of *user-defined* vars
only.

Rendering is **type-aware**: cheap values get their actual content
inlined so the model never has to call a history helper just to read
what's in a var. Expensive values fall back to a schema
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

There is no `<vars_archive>` block. Vars that get auto-forgotten
(see `auto-forget-stale-vars!` in `internal/loop.clj`) drop out of
the sandbox and are no longer rendered. The DB still carries their
full history; the agent recovers prior versions on demand via
`(v/inspect)` (from `vis-foundation`).
