# Symbol Decorators

Three of the four hooks on a symbol are **decorators** around the
target fn — same pattern as Ring middleware, Pedestal interceptors,
or AOP around-advice. They wrap `:fn`, can transform inputs, transform
outputs, short-circuit, and recover from errors.

The fourth hook (`:on-parse-error-fn`) is a different beast: it fires
**before** any dispatch, when SCI/edamame can't even parse the LLM's
source string. It's documented in its own section below.

## The decorator pipeline

```
  parses? ──yes──→ :before-fn ──→ :fn ──→ :after-fn ──→ result
                       │            │
                       │            └── throws ──→ :on-error-fn
                       │
                       └── {:result v} short-circuits past :fn
                       └── {:fn :args :env} overrides what :fn sees

  parses? ──no───→ :on-parse-error-fn  (see "Parse-Error Rescue" below)
```

`wrap-extension` installs the decorators automatically when the
extension is registered. Direct calls to `invoke-symbol-wrapper` are
rarely needed.

Every decorator returns a **map**. Missing keys keep the current value
— there is no positional return, no `nil`-means-default magic. The
return shape is the contract.

## Entry decorator

Slot key: `:before-fn`. Signature:

```clojure
(fn [environment f args] → map)
```

Runs before `:fn`. Can rewrite inputs or skip the call entirely.

| Return key | Effect |
|------------|--------|
| `:env` | Override the environment passed to `:fn` |
| `:fn` | Swap in a different implementation fn |
| `:args` | Override the args vector |
| `:result` | **Short-circuit** — skip `:fn` and return this value (the post-`:after-fn` step is bypassed) |

Use it for: argument normalization, permission checks that abort with
a synthetic result, swapping in a mock fn for testing.

## Exit decorator

Slot key: `:after-fn`. Signature:

```clojure
(fn [environment f args result] → map)
```

Runs after `:fn` returns successfully. Can transform the result.

| Return key | Effect |
|------------|--------|
| `:result` | Override the result the caller sees |
| `:env`, `:fn`, `:args` | Override (rarely useful here) |

Use it for: result truncation, telemetry, post-processing, attaching
metadata.

## Error decorator

Slot key: `:on-error-fn`. Signature:

```clojure
(fn [error environment f args] → map)
```

Runs when `:fn` throws. The return map decides what happens next.

| Return key | Effect |
|------------|--------|
| `:result` | Swallow the error, return this as the fallback |
| `:error` | Replace the original exception with this one and rethrow |
| `:fn` and/or `:args` | **Retry** — re-invoke with the (possibly different) fn and args |

If no `:on-error-fn` is defined, the original exception propagates to
the iteration loop unchanged.

Use it for: graceful degradation, "did you mean…?" retries with
corrected args, wrapping low-level exceptions in extension-specific
error types.

## Composition order

There is **one** decorator of each kind per symbol. Vis does not stack
multiple `:before-fn`s on the same symbol — if an extension needs to
compose behavior, the extension author composes the fns themselves
when building the symbol map. This is deliberate: stacked invisible
decorators are exactly the debuggability hole we don't want.

## Parse-error rescue

Not a decorator. Slot key: `:on-parse-error-fn`. Signature:

```clojure
(fn [{:keys [code error sym environment]}] → string|nil)
```

This fires **before** any dispatch, when SCI/edamame rejects the
LLM's source. There is no `:fn` to wrap yet — the source string isn't
even a valid form. Conceptually closer to a reader macro or a
preprocessor than to function decoration.

Symbol-level: only fires when the broken source mentions this symbol
(bare or `alias/sym`).

| Return value | Effect |
|--------------|--------|
| A new source string | The iteration loop retries the parse with it. If it parses cleanly the rewritten code runs and the resulting expression is tagged `:repaired? true`. |
| `nil` | Pass — the next matching symbol's hook is consulted, then the extension-level `:ext/on-parse-error-fn`, then the original error is surfaced to the LLM. |

Resolution order, walking every active extension:

1. **Symbol-level** `:ext.symbol/on-parse-error-fn` whose symbol name
   appears in the broken source.
2. **Extension-level** `:ext/on-parse-error-fn` as a catch-all for
   cross-cutting rewrites.

First non-`nil` rewrite different from `code` wins. Hooks that throw
are logged and skipped — a buggy rescue can never break turn
execution.

Sketch (hypothetical — no production extension currently ships parse-error
rescue; the `vis/rg` API was deliberately narrowed to a vector of literal
substrings precisely so this class of bug becomes unrepresentable on the
input side):

```clojure
(sdk/symbol 'frob frob-fn
  {:doc      "Hypothetical tool that takes a regex string."
   :arglists '([pattern] [pattern path])
   :on-error-fn       rescue-frob-args        ;; runtime decorator: bad arg
   :on-parse-error-fn rescue-frob-parse})     ;; parse rescue: rewrite source
```

If the LLM emitted `(frob "foo\|bar")` (an invalid Clojure escape),
edamame would fail. The loop would notice `frob` in the broken form and
call `rescue-frob-parse`, which could double the backslash so the
re-parse succeeds and the tool fn runs with the LLM's intended string.
In practice we prefer to design the API so the bad shape is unreachable
(`vis/rg` patterns vector + auto-quoted literals) rather than rescue it
at runtime — cheaper to maintain, easier to reason about.

## A note on naming

If you've used Ring middleware, Pedestal interceptors, Python's
`functools` decorators, or any AOP framework, you know this pattern.
`:before-fn` / `:after-fn` / `:on-error-fn` are exactly `:enter` /
`:leave` / `:error` from Pedestal, or the around / before / after /
after-throwing quadrant from AOP, or a Ring middleware split into
phases. The names are different; the contract is the same.

We kept the `-fn` suffix because Vis extension maps mix decorator
fns with non-fn metadata (`:doc`, `:arglists`, `:examples`) and the
suffix prevents collisions. It is **not** because these are something
fundamentally different from decorators. They aren't. They're
decorators.

## Observability

Every decorator invocation is logged via `taoensso.telemere/log!`:

| Level | Event |
|-------|-------|
| `:info` | Symbol invocation start/end with elapsed ms |
| `:debug` | Individual decorator start/end (before, after, fn return) |
| `:warn` | `:fn` threw; `:on-error-fn` invoked |

Log data includes `:ext` (namespace), `:sym` (symbol name), `:phase`,
and `:ms` (elapsed milliseconds).
