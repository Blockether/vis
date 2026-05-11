# Lifecycle Hooks (Nudges + Hard Guard Hooks)

`:ext/hooks` is the lifecycle callback surface for extensions.

Canonical naming used in this repo:

- **soft hooks** = pre-phase nudges (`:session/start`, `:turn/start`, `:turn.iteration/start`)
- **hard hooks** = answer-validation guard hooks (`:turn.answer/validate`)
- **post hooks** = side-effect hooks (`:turn.iteration/stop`, `:turn/stop`)

So “guard” is not a separate extension API; it is a lifecycle hook role.
Structural/engine invariants (answer-alone form shape,
answer-with-mutation preflight, etc.) remain core preflight gates in
`internal/loop.clj`.

## Where current engine nudges live

Every iteration the host assembles a per-iteration trailer:

```xml
<current_turn_context>...</current_turn_context>
<journal>...</journal>
<bindings>...</bindings>
<current_engine_start_nudges>
  <current_engine_start_nudge importance="high">...</current_engine_start_nudge>
  <current_engine_start_nudge importance="low">...</current_engine_start_nudge>
</current_engine_start_nudges>
```

Inside `<current_engine_start_nudges>` every entry comes from exactly one pre-phase
hook hit: an active extension's `:ext/hooks` entry whose `:fn` returned
a non-nil `{:hint ...}` map for that phase.

## Hook contract

```clojure
{:id    :my-ext/some-check                      ; namespaced keyword
 :doc   "What this hook reminds the model about."
 :phase :turn.iteration/start                   ; namespaced lifecycle phase
 :fn    (fn [ctx] nil-or-hit)}
```

Pre-phase `:fn` returns either:

- `nil` — hook passes silently for this phase.
- `{:hint "<string>" :importance :low|:normal|:high|:critical}` — host wraps the hint in `<current_engine_start_nudge importance="...">`.

`:importance` is optional; defaults to `:normal`.

Post-phase `:fn` return values are ignored. Use post phases for
telemetry, logging, and external side effects. The answer-validation phase is
hard guard territory: `nil` accepts; `{:reject true :message ... :hint ...}`
rejects the candidate answer and surfaces the message on the answer form.

Return shapes are validated. Reject-looking maps that do not match the
canonical shape (for example `{:reject false ...}`) are logged and ignored.

## Phases

Only namespaced keywords are valid. Old dash phases are removed.

| phase | runtime call site | invoked on | return handling |
|---|---|---|---|
| `:session/start` | `prompt/build-iteration-context` | first iteration of first turn | `{:hint ...}` becomes current engine nudge |
| `:turn/start` | `prompt/build-iteration-context` | first iteration of each turn | `{:hint ...}` becomes current engine nudge |
| `:turn.iteration/start` | `prompt/build-iteration-context` | every iteration before eval | `{:hint ...}` becomes current engine nudge |
| `:turn.iteration/stop` | `loop/emit-post-hooks!` | every iteration after eval | ignored |
| `:turn.answer/validate` | `loop/final-answer-gate-error` | when `(answer ...)` produced a candidate final answer | nil accepts; `{:reject true :message ... :hint ...}` rejects |
| `:turn/stop` | `loop/emit-post-hooks!` | after the turn closes | ignored |

Hooks do NOT block evaluation except `:turn.answer/validate`, which is
explicitly a hard gate for candidate final answers.

## The `ctx` pre-phase nudge hooks receive

```clojure
{:environment         <full env map>
 :phase               :turn.iteration/start
 :iteration           <1-based current iteration>
 :previous-blocks     <last iteration's blocks (or nil)>
 :model               <resolved model map: :name :provider :reasoning? ...>
 :context-limit       <effective context window for the model>
 :input-tokens        <estimated tokens of the assembled prompt-so-far>
 :title-refresh?      <turn-boundary refresh hint>
 :conversation-title  <current title, or nil/blank>
 :user-request        <raw current-turn user request>
 :current-objective   <deterministic objective map or nil>}
```

## The `ctx` answer-validation hooks receive

```clojure
{:environment         <full env map>
 :phase               :turn.answer/validate
 :iteration           <1-based current iteration>
 :blocks              <evaluated blocks from this iteration>
 :answer              <candidate final answer IR/value>
 :user-request        <raw current-turn user request>
 :current-objective   <deterministic objective map or nil>
 :previous-iterations <prior iterations in this turn for obligation checks>
 :previous-blocks     <flattened prior blocks fallback>}
```

## Soft nudge example

```clojure
{:ext/namespace 'com.acme.ext.docs
 :ext/doc       "Doc search"
 :ext/hooks
 [{:id    :docs/searches-failing
   :doc   "Warn when a long turn keeps producing search errors."
   :phase :turn.iteration/start
   :fn    (fn [{:keys [iteration previous-blocks]}]
            (when (and (> iteration 5)
                    (some :error previous-blocks))
              {:hint       "Doc searches are failing; check :error :hint and retry with a narrower query."
               :importance :high}))}]}
```

## Hard answer-validation example

```clojure
{:ext/namespace 'com.acme.ext.answer-policy
 :ext/doc       "Answer policy"
 :ext/hooks
 [{:id    :answer-policy/no-empty-answer
   :doc   "Reject empty final answers."
   :phase :turn.answer/validate
   :fn    (fn [{:keys [answer]}]
            (when (= [:ir] answer)
              {:reject true
               :message "Empty answer IR is not allowed."
               :hint "Return at least one paragraph or code block."}))}]}
```

## Error handling

If a hook `:fn` throws, the host catches the exception, logs at
`:warn`, and skips that hook for that phase. A misbehaving hook never
takes the loop down.

## Built-in foundation hooks

`vis-foundation` ships three pre-phase nudges and two hard
answer-validation hooks:

| id | phase | when it fires |
|---|---|---|
| `:foundation/conversation-title` | `:turn.iteration/start` | title blank / refresh-flagged / stale (periodic) |
| `:foundation/context-pressure` | `:turn.iteration/start` | input tokens > ~50% of model's context window |
| `:foundation/blind-answer` | `:turn.iteration/start` | iter 1 + investigation-style request + zero prior observation |
| `:foundation/unresolved-errors-before-answer` | `:turn.answer/validate` | open failure obligations remain (block/journal errors not closed by later proof). For answer-alone preflight failures, rerun the rejected sibling forms without `(answer ...)`, observe success, then answer alone. |
| `:foundation/action-request-needs-evidence` | `:turn.answer/validate` | action request (`fix/implement/run/...`) but no prior tool/code evidence in this turn and answer is not blocked/partial |
