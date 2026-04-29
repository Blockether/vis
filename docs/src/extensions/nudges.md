# Nudge System

Nudges are short `[system_nudge]` strings injected into the iteration
prompt to steer the LLM's behavior. They come from two sources:

1. **Built-in nudges** — `internal/loop.clj` (consecutive-error
   warning, strategy-restart hint)
2. **Extension nudges** — any extension's `:ext/nudge-fn`

There is no built-in repetition / duplication nudge. The model
already sees the previous iteration's result in `<journal>`, and the
dedup cache short-circuits literal re-issues with `:cached? true`,
which is signal enough to change strategy.

## Built-in nudges

| Nudge | When it fires |
|-------|--------------|
| Consecutive-error warning | `CONSECUTIVE_ERROR_NUDGE_AT` (=2) consecutive failed iterations |

## Extension nudges

When an extension provides `:ext/nudge-fn`, it is called **every
iteration** with a context map. Return a `[system_nudge] …` string to
inject, or `nil` to skip.

`:ext/activation-fn` is checked first — if it returns falsy for the
current environment, `:ext/nudge-fn` is not called at all.

### Context map

```clojure
{:environment            env    ;; the full environment map (see Environment Map)
 :iteration              int    ;; 0-indexed current iteration number
 :previous-blocks   [map]  ;; previous iteration's blocks:
                                ;;   [{:code str :result any :error str?
                                ;;     :stdout str :stderr str
                                ;;     :execution-time-ms int
                                ;;     :timeout? bool :repaired? bool} …]
                                ;;   nil on iteration 0 or after error recovery
 :user-var-count         int}   ;; user-defined vars in the sandbox
```

Key names spell out the full word (`:previous-blocks`, not
`:prev-blocks`) per the no-abbreviation rule in `AGENTS.md`.

### Rules

1. **Return `nil` or a non-blank string.** Anything else is silently dropped.
2. **Prefix with `[system_nudge]`** so the LLM recognises it as system guidance.
3. **Keep it short.** One line. Nudges >200 chars dilute signal.
4. **Never throw.** A throwing nudge-fn is caught, logged at `:warn`, and skipped.
5. **Do not mutate environment state.** Nudge-fns are observers, not actors.

### Example

```clojure
(sdk/extension
  {:ext/namespace 'my-tool
   :ext/doc       "My custom tool"
   :ext/group     "tools"
   :ext/prompt    "Prefer batching by 10 items at a time."
   :ext/symbols   [my-tool-sym]
   :ext/nudge-fn  (fn [{:keys [environment iteration previous-blocks]}]
                    (when (and (> iteration 5)
                              (some :timeout? previous-blocks))
                      "[system_nudge] my-tool calls are timing out. Try smaller batch sizes."))})
```

## Pipeline

Inside `build-iteration-context` (called every iteration):

```
1. Compute built-in nudges (consecutive-error warning).
2. Call collect-extension-nudges (internal/loop.clj):
   for each registered extension with `:ext/nudge-fn`:
     a. Check `:ext/activation-fn` against environment.
     b. If active, call `:ext/nudge-fn` with context.
     c. Collect non-nil string results.
3. Join all nudges with newline.
4. Append to iteration context (after `<var_index>`).
```
