# Hint Lifecycle Spec

## Goal

Replace XML-ish iteration hints with EDN data in `ctx`, and add a silent Clojure primitive for explicit hint satisfaction.

## Names

- Use **hint** everywhere in new code/docs.
- Retire **nudge** names when touched:
  - `nudges.clj` -> `hints.clj`
  - `title-nudge` -> `title-hint`
  - `context-pressure-nudge` -> `context-pressure-hint`
- Compatibility may keep old vars temporarily only if needed, but prompt/model-facing names are `hint`.

## Data model

`ctx` gains top-level key:

```clojure
:hints [{:id :vis.foundation/conversation-title
         :importance :high
         :text "The conversation title is currently empty. Set it via `(set-conversation-title! \"...\")`."
         :satisfy-with '(satisfy-hint! :vis.foundation/conversation-title)}]
```

### Hint fields

Required:

- `:id` keyword, stable per hint source.
- `:text` nonblank string.

Optional:

- `:importance` keyword, default `:normal`.
- `:satisfy-with` quoted form shown to model; default `(satisfy-hint! <id>)`.
- `:source` extension namespace keyword/symbol if useful for diagnostics.

## Rendering

Remove XML trailer block:

```xml
<iteration_hints>
...
</iteration_hints>
```

No `<iteration_hint>` tags. No XML escaping path.

Render hints only through printed EDN `ctx`:

```clojure
;; ctx =
{:conversation {...}
 :iteration {...}
 :hints [...]
 :defs {...}}
```

Optional one-line trailer before `ctx` is allowed:

```clojure
;; Read (:hints ctx). After satisfying a hint, call (satisfy-hint! <id>).
```

No XML.

## Hook contract

`:turn.iteration/start` hook returns hint map:

```clojure
{:id :vis.foundation/conversation-title
 :hint "..."          ;; legacy input accepted during migration
 :text "..."          ;; preferred
 :importance :high}
```

Collector normalizes to:

```clojure
{:id :vis.foundation/conversation-title
 :text "..."
 :importance :high
 :satisfy-with '(satisfy-hint! :vis.foundation/conversation-title)}
```

Invalid returns are logged and ignored. Hook exceptions are logged and ignored.

## Satisfaction primitive

Add sandbox primitive:

```clojure
(satisfy-hint! :vis.foundation/conversation-title)
;; => :vis/silent
```

### Behavior

- Requires keyword hint id.
- Adds id to per-turn/per-conversation `:satisfied-hints` state.
- Returns `:vis/silent`.
- Visible UI/progress treats call as silent bookkeeping.
- Next `ctx` omits satisfied hints:

```clojure
(remove #(contains? satisfied-hints (:id %)) hints)
```

### Scope

Satisfaction removes hint from current rendered `(:hints ctx)` only. It does **not** remove hook from extension registry.

Hook may re-emit later if condition becomes true again. For state-derived hints, source condition should normally suppress it before satisfied state is consulted.

## Auto-satisfaction

Prefer state-based suppression when possible.

Examples:

- Title hint disappears when conversation title becomes nonblank.
- Context-pressure hint disappears only when context pressure drops below threshold or turn/session state marks it satisfied.

`satisfy-hint!` is explicit acknowledgement, not primary truth when runtime state can prove completion.

## Filtering order

For each iteration:

1. Build base `ctx` without hints.
2. Run active `:turn.iteration/start` hooks.
3. Normalize hook hits into hint maps.
4. Apply state-derived suppression from hint source logic.
5. Filter `satisfied-hints` ids.
6. Add remaining hints to `ctx`.
7. Bind `ctx` in sandbox and render trailer.

## Persistence

Minimum viable:

- In-memory atom on environment for current turn/session:

```clojure
:satisfied-hints-atom (atom #{})
```

Better later:

- Persist per conversation/turn if needed.
- Key by conversation id + turn id to avoid stale global suppression.

Initial scope: current turn/session enough.

## Prompt contract

Extension/Foundation prompt should say:

```text
Read (:hints ctx) before acting. Hints are host requests for current iteration.
When you satisfy a hint, call (satisfy-hint! <hint-id>) in the same Clojure form.
`satisfy-hint!` is silent bookkeeping; do not mention it in final answer.
```

Do not ask model to write prose like `satisfy-hint`. Use primitive call only.

## Tests

Add/adjust tests:

1. `ctx` renders hints as EDN under `:hints`.
2. Trailer contains no `<iteration_hints>` or `<iteration_hint>`.
3. Hook hit with `:hint` normalizes to `:text`.
4. Hook hit id preserved.
5. `(satisfy-hint! id)` returns `:vis/silent`.
6. Satisfied id filters hint out of next `ctx`.
7. Title hint auto-suppresses when title nonblank.
8. Invalid `satisfy-hint!` arg throws clear error.

## Migration steps

1. Add `:hints` support to `ctx/build`.
2. Preserve `:id` in iteration hook collector.
3. Normalize `:hint` -> `:text`.
4. Route hints into `ctx`, not `render-iteration-hints`.
5. Delete XML hint renderer + tests.
6. Add `satisfy-hint!` primitive returning `:vis/silent`.
7. Add satisfied-hints state + filter.
8. Rename foundation nudge namespace/functions to hints.
9. Update prompt text from nudges/XML to hints/EDN.

## Non-goals

- No hard enforcement for hints.
- No answer rejection from unsatisfied hints.
- No XML hint syntax.
- No prose acknowledgement protocol.
