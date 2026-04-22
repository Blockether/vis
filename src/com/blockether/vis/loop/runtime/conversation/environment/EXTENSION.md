# Extension System

> **Namespace:** `com.blockether.vis.loop.runtime.conversation.environment.extension`

Extensions are the **only** way to add symbols, classes, and documentation
to the SCI sandbox. An extension is a namespace-like bundle that groups
related tools, constants, and prompt context into a single validated unit.

## Public API

### `extension` — build and validate an extension

```clojure
(extension spec) → validated extension map
```

The canonical constructor. Accepts a map and returns a validated
`::extension` conforming to the full spec. Fills in defaults for
optional keys:

| Key | Required | Default | Description |
|-----|----------|---------|-------------|
| `:ext/namespace` | ✓ | — | Unique symbol name, e.g. `'documents`, `'git` |
| `:ext/doc` | ✓ | — | Extension-level description |
| `:ext/group` | ✓ | — | Top-level prompt group, e.g. `"knowledge"` |
| `:ext/subgroup` | ✗ | same as `:ext/group` | Finer-grained grouping within the group |
| `:ext/activation-fn` | ✗ | `(constantly true)` | `(fn [env] → bool)` — when falsy, all symbols are unbound |
| `:ext/prompt` | ✓ | — | String or `(fn [env] → string)` — LLM-facing docs in system prompt |
| `:ext/symbols` | ✓ | — | Vector of symbol entries (from `symbol` / `value`) |
| `:ext/classes` | ✗ | `{}` | `{fq-symbol → Class}` — Java classes exposed in sandbox |
| `:ext/imports` | ✗ | `{}` | `{short-symbol → fq-symbol}` — short-name imports |

### `symbol` — build a function symbol entry

```clojure
(symbol sym-name f opts) → validated fn symbol entry
```

Creates a function binding for the SCI sandbox.

| Opt | Required | Default | Description |
|-----|----------|---------|-------------|
| `:doc` | ✓ | — | One-liner shown in the sandbox var's docstring |
| `:arglists` | ✓ | — | Argument signatures, e.g. `'([query] [query opts])` |
| `:examples` | ✗ | derived from `:arglists` | Usage examples injected into system prompt |
| `:before-fn` | ✗ | — | `(fn [env f args] → map)` — pre-call hook |
| `:after-fn` | ✗ | — | `(fn [env f args result] → map)` — post-call hook |
| `:on-error-fn` | ✗ | — | `(fn [err env f args] → map)` — error handler |

### `value` — build a value symbol entry

```clojure
(value sym-name val opts) → validated value symbol entry
```

Creates a plain constant/data binding. No hooks, no arglists.

| Opt | Required | Description |
|-----|----------|-------------|
| `:doc` | ✓ | One-liner description |

### `wrap-extension` — wrap all symbols for runtime use

```clojure
(wrap-extension ext env) → {sym → fn-or-value}
```

Wraps every function symbol through `invoke-symbol-wrapper`
(before → fn → after, with on-error recovery). Value symbols
are returned as `{sym → value}`. Each wrapped fn closes over
the extension and symbol entry.

### `invoke-symbol-wrapper` — full invocation pipeline

```clojure
(invoke-symbol-wrapper ext sym-entry args env) → result
```

Runs the complete hook chain for a single function call:

```
before-fn → fn → after-fn
              ↓ (on error)
          on-error-fn
```

Not typically called directly — `wrap-extension` wires this up
automatically.

### `validate!` — normalize and assert extension conformance

```clojure
(validate! ext) → normalized ext (or throws)
```

Normalizes `:ext/prompt` (string → fn) then checks that the extension
map conforms to `::extension`. Called internally by `extension`; safe
to call standalone on hand-built maps — string prompts are converted
automatically.

> **Note:** The `::extension` spec requires `:ext/prompt` to be `fn?`.
> Both `extension` and `validate!` normalize strings to `(constantly s)`
> before validation, so callers can pass either form.

## Hook Protocol

Every hook returns a **map**. Missing keys keep the current value.

### `:before-fn` — `(fn [env f args] → map)`

| Return key | Effect |
|------------|--------|
| `:env` | Override env for the call |
| `:fn` | Override the implementation fn |
| `:args` | Override the args vector |
| `:result` | **Short-circuit** — skip `:fn` entirely, return this value |

### `:after-fn` — `(fn [env f args result] → map)`

| Return key | Effect |
|------------|--------|
| `:result` | Override the result |
| `:env`, `:fn`, `:args` | Override (rarely needed) |

### `:on-error-fn` — `(fn [err env f args] → map)`

Called when `:fn` throws. The return map determines recovery:

| Return key | Effect |
|------------|--------|
| `:result` | Use this as the fallback result |
| `:error` | Throw this error instead |
| `:fn` / `:args` | **Retry** — re-invoke with (possibly different) fn and args |

If no `:on-error-fn` is defined, the original exception propagates.

## Example

```clojure
(require '[com.blockether.vis.loop.runtime.conversation.environment.extension
           :as ext])

;; Function symbol with hooks
(def search-sym
  (ext/symbol 'search-documents search-fn
    {:doc        "Full-text search across ingested documents."
     :arglists  '([query] [query opts])
     :examples  ["(search-documents \"neural\")"
                 "(search-documents \"attention\" {:limit 5})"]
     :before-fn (fn [env f args]
                  {:args (update args 0 str/lower-case)})
     :after-fn  (fn [env f args result]
                  {:result (take 10 result)})}))

;; Value symbol
(def max-results-sym
  (ext/value 'max-search-results 50
    {:doc "Maximum number of search results returned."}))

;; Build the extension
(def docs-ext
  (ext/extension
    {:ext/namespace     'documents
     :ext/doc           "Document search and retrieval"
     :ext/group         "knowledge"
     :ext/subgroup      "documents"
     :ext/prompt        "Full-text search across ingested documents.
                         Use (search-documents query) to find relevant pages."
     :ext/activation-fn (fn [env] (seq (list-docs (:db-info env))))
     :ext/symbols       [search-sym max-results-sym]
     :ext/classes       {'java.time.LocalDate java.time.LocalDate}
     :ext/imports       {'LocalDate 'java.time.LocalDate}}))

;; At runtime — wrap for SCI binding
(def wrapped (ext/wrap-extension docs-ext env))
;; => {'search-documents  (fn [& args] ...)
;;     'max-search-results 50}
```

## Observability

All hook invocations are logged via `taoensso.trove/log!` with structured
data:

| Level | Event |
|-------|-------|
| `:info` | Symbol invocation start/end with elapsed ms |
| `:debug` | Individual hook start/end (before, after, fn return) |
| `:warn` | `:fn` threw; `:on-error-fn` invoked |

Log data includes `:ext` (namespace), `:sym` (symbol name), `:phase`,
and `:ms` (elapsed milliseconds).
