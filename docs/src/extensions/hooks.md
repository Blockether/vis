# Hook Protocol

Every hook returns a **map** (runtime hooks) or a **string-or-nil**
(parse-error rescue). Missing keys in a runtime-hook return keep the
current value.

## Invocation Pipeline

```
  source string ──────┬─ parses? ─yes─→ :before-fn → :fn → :after-fn → result
                       │                                  │
                       │                                  └─ throws → :on-error-fn
                       │
                       └─ parses? ─no─→ :on-parse-error-fn (symbol-level)
                                            └─ falls back to :ext/on-parse-error-fn
```

`wrap-extension` wires the runtime hooks. The iteration loop wires
the parse-error hooks via `try-rescue-parse-error`.

`wrap-extension` wires this up automatically. Direct calls via
`invoke-symbol-wrapper` are rarely needed.

## `:before-fn` — `(fn [env f args] → map)`

Called before the implementation fn. Can transform inputs or short-circuit.

| Return key | Effect |
|------------|--------|
| `:env` | Override env for the call |
| `:fn` | Override the implementation fn |
| `:args` | Override the args vector |
| `:result` | **Short-circuit** — skip `:fn` entirely, return this value |

## `:after-fn` — `(fn [env f args result] → map)`

Called after the implementation fn returns. Can transform the result.

| Return key | Effect |
|------------|--------|
| `:result` | Override the result |
| `:env`, `:fn`, `:args` | Override (rarely needed) |

## `:on-error-fn` — `(fn [err env f args] → map)`

Called when `:fn` throws. The return map determines recovery:

| Return key | Effect |
|------------|--------|
| `:result` | Use this as the fallback result |
| `:error` | Throw this error instead |
| `:fn` / `:args` | **Retry** — re-invoke with (possibly different) fn and args |

If no `:on-error-fn` is defined, the original exception propagates.

## `:on-parse-error-fn` — `(fn [{:code :error :sym :environment}] → string|nil)`

Called when SCI/edamame rejects the LLM's source code **before** any
tool fn is dispatched. Symbol-level: fires only when the broken
source mentions this symbol (bare or `alias/sym`).

Return:

| Value | Effect |
|-------|--------|
| A new source string | The iteration loop retries the parse with it; if it parses cleanly the rewritten code runs and the resulting expression is tagged `:repaired? true` |
| `nil` | Pass — the next matching symbol's hook is consulted, then the extension-level `:ext/on-parse-error-fn`, then the original error is surfaced to the LLM |

The iteration loop walks every active extension and resolves in this
order:

1. **Symbol-level** `:ext.symbol/on-parse-error-fn` whose symbol name
   appears in the broken source.
2. **Extension-level** `:ext/on-parse-error-fn` as a catch-all for
   cross-cutting rewrites.

First non-nil rewrite different from `code` wins. Hooks that throw
are logged and skipped — a buggy hook can never break query
execution.

Example (from `vis-common-operations`):

```clojure
(ext/symbol 'grep-files grep-files
  {:doc      "Search files with RE2/J."
   :arglists '([pattern] [pattern path])
   :on-error-fn       rescue-grep-args        ;; runtime: bad regex
   :on-parse-error-fn rescue-parse-error})    ;; parse: bare `\|`
```

When the LLM emits `(fs/grep-files "foo\|bar")` (raw `\|` instead of
`\\|`), edamame fails. The loop notices `fs/grep-files` in the broken
form and calls `rescue-parse-error`, which doubles the backslash so
the re-parse succeeds and the tool fn runs with the LLM's intended
string.

## Observability

All hook invocations are logged via `taoensso.telemere/log!`:

| Level | Event |
|-------|-------|
| `:info` | Symbol invocation start/end with elapsed ms |
| `:debug` | Individual hook start/end (before, after, fn return) |
| `:warn` | `:fn` threw; `:on-error-fn` invoked |

Log data includes `:ext` (namespace), `:sym` (symbol name), `:phase`,
and `:ms` (elapsed milliseconds).
