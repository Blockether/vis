# Nudge System

Nudges are short `[system_nudge]` strings injected into the iteration
prompt to steer the LLM's behavior. They come from two sources:

1. **Built-in nudges** — `prompt.clj` (budget warning, var-index overflow,
   repetition detection)
2. **Extension nudges** — any extension's `:ext/nudge-fn`

## Built-in Nudges

| Nudge | When it fires |
|-------|--------------|
| Budget warning | ≤2 iterations remaining in the budget |
| Var-index overflow | >150 user-defined vars in the sandbox |
| Repetition warning | Same code/result pair seen ≥3 times |

## Extension Nudges

When an extension provides `:ext/nudge-fn`, it is called **every
iteration** with a context map. Return a `[system_nudge] …` string to
inject, or `nil` to skip.

`:ext/activation-fn` is checked first — if it returns falsy for the
current environment, `:ext/nudge-fn` is not called at all.

### Context Map

```clojure
{:environment            env       ;; the full environment map (see Environment Map)
 :iteration              int       ;; 0-indexed current iteration number
 :current-max-iterations int       ;; live budget cap (includes runtime extensions)
 :prev-expressions       [map]     ;; previous iteration's expressions:
                                   ;;   [{:code str :result any :error str?
                                   ;;     :stdout str :stderr str
                                   ;;     :execution-time-ms int
                                   ;;     :timeout? bool :repaired? bool} …]
                                   ;;   nil on iteration 0 or after error recovery
 :prev-iteration         int       ;; iteration index that produced prev-expressions
                                   ;;   -1 when prev-expressions is nil
 :user-var-count          int}      ;; user-defined vars in the sandbox
```

### Rules

1. **Return `nil` or a non-blank string.** Anything else is silently dropped.
2. **Prefix with `[system_nudge]`** so the LLM recognises it as system guidance.
3. **Keep it short.** One line. Nudges >200 chars dilute signal.
4. **Never throw.** A throwing nudge-fn is caught, logged at `:warn`, and skipped.
5. **Do not mutate environment state.** Nudge-fns are observers, not actors.

### Example

```clojure
(ext/extension
  {:ext/namespace 'my-tool
   :ext/doc       "My custom tool"
   :ext/group     "tools"
   :ext/prompt    "Use (my-tool ...) to do X."
   :ext/symbols   [my-tool-sym]
   :ext/nudge-fn  (fn [{:keys [environment iteration prev-expressions]}]
                    (when (and (> iteration 5)
                              (some :timeout? prev-expressions))
                      "[system_nudge] my-tool calls are timing out. Try smaller batch sizes."))})
```

## Pipeline

Inside `build-iteration-context` (called every iteration):

```
1. Compute built-in nudges (budget, var-overflow, repetition)
2. Call collect-extension-nudges (prompt.clj)
   → for each registered extension with :ext/nudge-fn:
     a. Check :ext/activation-fn against environment
     b. If active, call :ext/nudge-fn with context
     c. Collect non-nil string results
3. Join all nudges with newline
4. Append to iteration context (after <var_index>)
```
