# Lifecycle Hooks and System Nudges

`:ext/hooks` is the lifecycle callback surface for extensions. Pre-eval
phases can surface **model-facing** `<system_nudge>` entries in the
per-iteration prompt. The user never sees them; they nudge the model
toward correct behavior without ever blocking evaluation. For hard
rejections, use the preflight gates in `internal/loop.clj`.

## Where system nudges live

Every iteration the host assembles a per-iteration trailer:

```xml
<journal>...</journal>
<bindings>...</bindings>
<system_nudges>
  <system_nudge importance="high">...</system_nudge>
  <system_nudge importance="low">...</system_nudge>
</system_nudges>
```

Inside `<system_nudges>` every entry comes from exactly one pre-phase
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
- `{:hint "<string>" :importance :low|:normal|:high|:critical}` — host wraps the hint in `<system_nudge importance="...">`.

`:importance` is optional; defaults to `:normal`.

Post-phase `:fn` return values are ignored. Use post phases for
telemetry, logging, and external side effects. The answer-validation phase is
hard guard territory: `nil` accepts; `{:reject true :message ... :hint ...}`
rejects the candidate answer and surfaces the message on the answer form.

## Phases

Only namespaced keywords are valid. Old dash phases are removed.

| phase | invoked on | return handling |
|---|---|---|
| `:session/start` | first iteration of first turn | `{:hint ...}` becomes system nudge |
| `:turn/start` | first iteration of each turn | `{:hint ...}` becomes system nudge |
| `:turn.iteration/start` | every iteration before eval | `{:hint ...}` becomes system nudge |
| `:turn.iteration/stop` | every iteration after eval | ignored |
| `:turn.answer/validate` | when `(answer ...)` produced a candidate final answer | nil accepts; `{:reject true :message ... :hint ...}` rejects |
| `:turn/stop` | after the turn closes | ignored |

Hooks do NOT block evaluation. Pre-phase hooks append a
`<system_nudge>` to the iteration prompt; the model decides whether to
amend.

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
 :user-request        <raw current-turn user request>}
```

## The `ctx` answer-validation hooks receive

```clojure
{:environment  <full env map>
 :phase        :turn.answer/validate
 :iteration    <1-based current iteration>
 :blocks       <evaluated blocks from this iteration>
 :answer       <candidate final answer IR/value>}
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

`vis-foundation` ships three pre-phase nudge hooks and one hard
answer-validation hook:

| id | phase | when it fires |
|---|---|---|
| `:foundation/conversation-title` | `:turn.iteration/start` | title blank / refresh-flagged / stale (periodic) |
| `:foundation/context-pressure` | `:turn.iteration/start` | input tokens > ~50% of model's context window |
| `:foundation/blind-answer` | `:turn.iteration/start` | iter 1 + request contains investigation verbs + zero observation calls |
| `:foundation/unresolved-errors-before-answer` | `:turn.answer/validate` | candidate final answer while the latest previous iteration still contains block or journal errors |
