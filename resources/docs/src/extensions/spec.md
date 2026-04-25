# Extension Spec

## `extension` — build and validate

```clojure
(ext/extension spec) → validated extension map
```

| Key | Required | Default | Description |
|-----|----------|---------|-------------|
| `:ext/namespace` | ✓ | — | Unique symbol name, e.g. `'documents`, `'git` |
| `:ext/doc` | ✓ | — | Extension-level description |
| `:ext/group` | ✓ | — | Top-level prompt group, e.g. `"knowledge"` |
| `:ext/subgroup` | ✗ | same as `:ext/group` | Finer-grained grouping within the group |
| `:ext/activation-fn` | ✗ | `(constantly true)` | `(fn [env] → bool)` — when falsy, all symbols are unbound and nudge-fn is skipped |
| `:ext/prompt` | ✓ | — | String or `(fn [env] → string)` — LLM-facing docs in system prompt |
| `:ext/nudge-fn` | ✗ | — | `(fn [ctx] → string\|nil)` — per-iteration nudge composer (see [Nudge System](nudges.md)) |
| `:ext/symbols` | ✓ | — | Vector of symbol entries (from `symbol` / `value`) |
| `:ext/classes` | ✗ | `{}` | `{fq-symbol → Class}` — Java classes exposed in sandbox |
| `:ext/imports` | ✗ | `{}` | `{short-symbol → fq-symbol}` — short-name imports |

## `symbol` — function binding

```clojure
(ext/symbol sym-name f opts) → validated fn symbol entry
```

| Opt | Required | Default | Description |
|-----|----------|---------|-------------|
| `:doc` | ✓ | — | One-liner shown in the sandbox var's docstring |
| `:arglists` | ✓ | — | Argument signatures, e.g. `'([query] [query opts])` |
| `:examples` | ✗ | derived from `:arglists` | Usage examples injected into system prompt |
| `:before-fn` | ✗ | — | `(fn [env f args] → map)` — pre-call hook |
| `:after-fn` | ✗ | — | `(fn [env f args result] → map)` — post-call hook |
| `:on-error-fn` | ✗ | — | `(fn [err env f args] → map)` — error handler |

## `value` — constant binding

```clojure
(ext/value sym-name val opts) → validated value symbol entry
```

| Opt | Required | Description |
|-----|----------|-------------|
| `:doc` | ✓ | One-liner description |

## `wrap-extension` — bind into SCI

```clojure
(ext/wrap-extension ext env) → {sym → fn-or-value}
```

Wraps every function symbol through `invoke-symbol-wrapper`
(before → fn → after, with on-error recovery). Value symbols
are returned as `{sym → value}`. Each wrapped fn closes over
the extension, symbol entry, and environment.

## `validate!` — standalone validation

```clojure
(ext/validate! ext) → normalized ext (or throws)
```

Normalizes `:ext/prompt` (string → fn) then checks the spec.
Called internally by `extension`; safe to call standalone.

> **Note:** `:ext/prompt` accepts `string` or `fn?`. Both `extension`
> and `validate!` normalize strings to `(constantly s)` before
> validation.

## Full Example

```clojure
(require '[c.b.vis.loop.runtime.conversation.environment.extension :as ext])

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

(def max-results-sym
  (ext/value 'max-search-results 50
    {:doc "Maximum number of search results returned."}))

(def docs-ext
  (ext/extension
    {:ext/namespace     'documents
     :ext/doc           "Document search and retrieval"
     :ext/group         "knowledge"
     :ext/subgroup      "documents"
     :ext/prompt        "Full-text search across ingested documents.
                         Use (search-documents query) to find relevant pages."
     :ext/activation-fn (fn [env] (seq (list-docs (:db-info env))))
     :ext/nudge-fn      (fn [{:keys [environment iteration prev-expressions]}]
                          (when (and (> iteration 5)
                                    (some :error prev-expressions))
                            "[system_nudge] Document searches are failing."))
     :ext/symbols       [search-sym max-results-sym]
     :ext/classes       {'java.time.LocalDate java.time.LocalDate}
     :ext/imports       {'LocalDate 'java.time.LocalDate}}))
```
