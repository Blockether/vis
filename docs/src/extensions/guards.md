# Guards

`:ext/guards` is the single mechanism for extensions to surface
**model-facing** `<system_nudge>` entries in the per-iteration prompt.
The user never sees them; they nudge the model toward correct
behavior without ever blocking evaluation. For hard rejections, use
the preflight gates in `internal/loop.clj`.

## Where the entries live

Every iteration the host assembles a per-iteration trailer:

```
<journal>...</journal>
<bindings>...</bindings>
<system_nudges>
  <system_nudge importance="high">...</system_nudge>
  <system_nudge importance="low">...</system_nudge>
</system_nudges>
```

Inside `<system_nudges>` every entry comes from exactly one guard hit
(active extension's `:ext/guards` whose `:check-fn` returned a non-nil
hint this iteration).

## Guard contract

```clojure
{:id       :my-ext/some-check                      ; namespaced keyword
 :doc      "What this guard reminds the model about."
 :scope    :iteration                              ; :iteration | :turn | :session
 :check-fn (fn [ctx] nil-or-hit)}
```

`:check-fn` returns either:

- `nil` — guard passes silently this iteration.
- `{:hint "<string>" :importance :low|:normal|:high|:critical}` — host wraps the hint in `<system_nudge importance="...">`.

`:importance` is optional; defaults to `:normal`.

## Scopes

`:scope` controls **when** the host invokes `:check-fn`:

| scope | invoked on |
|---|---|
| `:iteration` | every iteration |
| `:turn` | only the first iteration of each turn |
| `:session` | only the first iteration of the first turn of the session |

Guards do NOT block evaluation. They append a `<system_nudge>` to the
iteration's prompt; the model decides whether to amend.

## The `nudge-ctx` your `:check-fn` receives

```clojure
{:environment         <full env map>
 :iteration           <1-based current iteration>
 :previous-blocks     <last iteration's blocks (or nil)>
 :model               <resolved model map: :name :provider :reasoning? ...>
 :context-limit       <effective context window for the model>
 :input-tokens        <estimated tokens of the assembled prompt-so-far>
 :title-refresh?      <turn-boundary refresh hint>
 :conversation-title  <current title, or nil/blank>
 :user-request        <raw current-turn user request>}
```

## Example

```clojure
{:ext/namespace 'com.acme.ext.docs
 :ext/doc       "Doc search"
 :ext/guards
 [{:id       :docs/searches-failing
   :doc      "Warn when a long turn keeps producing search errors."
   :scope    :iteration
   :check-fn (fn [{:keys [iteration previous-blocks]}]
               (when (and (> iteration 5)
                       (some :error previous-blocks))
                 {:hint       "Doc searches are failing; check :error :hint and retry with a narrower query."
                  :importance :high}))}]}
```

## Error handling

If a `:check-fn` throws, the host catches the exception, logs at
`:warn` (telemetry id `:com.blockether.vis.internal.prompt/guard-threw`),
and skips that guard for the iteration. A misbehaving guard never
takes the loop down.

## Built-in guards

`vis-foundation` ships three guards:

| id | scope | when it fires |
|---|---|---|
| `:foundation/conversation-title` | `:iteration` | title blank / refresh-flagged / stale (periodic) |
| `:foundation/context-pressure` | `:iteration` | input tokens > ~50% of model's context window |
| `:foundation/blind-answer` | `:iteration` | iter 1 + request contains investigation verbs + zero observation calls |

## History

A freeform `:ext/nudge-fn` hook existed previously (one fn per
extension, returning 0+ nudges). It was retired in favor of
`:ext/guards` so each emitter is declarative, named, scope-aware,
and individually toggleable.
