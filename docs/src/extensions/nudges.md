# Nudge System

Nudges are structured host hints injected into the per-iteration context as XML-ish blocks:

```xml
<system_nudges>
  <system_nudge importance="low|normal|high|critical">
    guidance text
  </system_nudge>
</system_nudges>
```

They come from two sources:

1. **Built-in nudges** — title and context-pressure hints from the prompt assembler.
2. **Extension nudges** — any active extension's `:ext/nudge-fn`.

There is no built-in repetition / duplication nudge. The model already sees the previous iteration's result in `<journal>`, which is signal enough to change strategy when it spots itself re-issuing an identical call.

## Built-in nudges

| Nudge | Importance | When it fires |
|---|---|---|
| Title nudge | `:low` | `CONVERSATION_TITLE` is blank, or title refresh cadence fires. |
| Context-pressure nudge | `:high` | Prompt usage crosses the context-pressure threshold. |

## Extension nudges

When an extension provides `:ext/nudge-fn`, it is called every iteration with a context map. The return value is spec-checked at runtime.

Valid returns:

```clojure
nil
"plain nudge text"
{:importance :low|:normal|:high|:critical
 :text "nudge text"}
```

`{:message "..."}` and `{:body "..."}` are accepted aliases for `:text`.

A legacy string prefixed with `[system_nudge]` is accepted; Vis strips the prefix and renders a tagged `<system_nudge importance="normal">` entry.

Invalid returns are logged at `:warn` and skipped. Throwing nudge-fns are caught, logged at `:warn`, and skipped.

`:ext/activation-fn` is checked first. If it returns falsy for the current environment, `:ext/nudge-fn` is not called at all.

### Context map

```clojure
{:environment     env    ;; full environment map
 :iteration       int    ;; 1-based current iteration position
 :previous-blocks [map]} ;; previous iteration's blocks, nil/empty on first iteration
```

Key names spell out the full word (`:previous-blocks`, not `:prev-blocks`) per the no-abbreviation rule in `AGENTS.md`.

### Rules

1. Return `nil`, a non-blank string, or a valid map with `:importance` + text.
2. Use importance intentionally:
   - `:low` — reminder / hygiene.
   - `:normal` — situational guidance.
   - `:high` — context/risk pressure; act soon.
   - `:critical` — likely wrong/destructive path; stop or ask.
3. Keep nudges short. One actionable instruction beats paragraphs.
4. Never throw. Throwing is isolated, but it burns signal.
5. Stay read-only. Nudge-fns are observers, not actors — every mutation belongs in a hook or symbol invocation.

### Example

```clojure
(sdk/extension
  {:ext/namespace 'my-tool
   :ext/doc       "My custom tool"
   :ext/kind      "tools"
   :ext/prompt    "Prefer batching by 10 items at a time."
   :ext/symbols   [my-tool-sym]
   :ext/nudge-fn  (fn [{:keys [iteration previous-blocks]}]
                    (when (and (> iteration 6)
                               (some :timeout? previous-blocks))
                      {:importance :high
                       :text "my-tool calls are timing out. Try smaller batch sizes."}))})
```

## Pipeline

Inside `build-iteration-context` (called every iteration):

```text
1. Compute built-in nudges.
2. For each active extension with :ext/nudge-fn:
   a. call :ext/nudge-fn with context;
   b. spec-check return value;
   c. normalize to {:importance ... :text ...};
   d. skip invalid/blank results with a warn log.
3. Render all nudges under <system_nudges>.
4. Append after <journal>/<var_index> in iteration context.
```
