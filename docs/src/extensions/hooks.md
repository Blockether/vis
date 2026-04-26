# Hook Protocol

Every hook returns a **map**. Missing keys keep the current value.

## Invocation Pipeline

```
before-fn  →  :fn  →  after-fn  →  result
   │          │
   │          └── throws → on-error-fn ─┬─ :result        → fallback result
   │                                  ├─ :error         → throw
   │                                  └─ :fn / :args    → retry the call
   └─ :result → short-circuit (skip :fn entirely)
```

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

## Observability

All hook invocations are logged via `taoensso.telemere/log!`:

| Level | Event |
|-------|-------|
| `:info` | Symbol invocation start/end with elapsed ms |
| `:debug` | Individual hook start/end (before, after, fn return) |
| `:warn` | `:fn` threw; `:on-error-fn` invoked |

Log data includes `:ext` (namespace), `:sym` (symbol name), `:phase`,
and `:ms` (elapsed milliseconds).
