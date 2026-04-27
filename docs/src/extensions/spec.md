# Extension Spec

## Auto-Discovery

Extensions are auto-discovered from the unified `META-INF/vis.edn` on
the classpath. See [Overview — Auto-Discovery](overview.md#auto-discovery-from-classpath-recommended)
for the full convention.

## `extension` — build and validate

```clojure
(require '[com.blockether.vis.extension :as ext])

(ext/extension spec) → validated extension map
```

The extension contract lives entirely on `com.blockether.vis.extension`
(in the standalone `com.blockether/vis-extension` package). The older
re-exports of `extension` / `symbol` / `value` / `register-global!` /
`render-prompt` from `com.blockether.vis.core` were **removed on
purpose** so authoring code only depends on the slim contract
(telemere + clojure.spec) instead of dragging in the whole runtime.

The runtime composition helpers that DO need a live environment
(`active-extensions`, `assemble-system-prompt`, `register-extension!`)
stay on `com.blockether.vis.core` because they only make sense against
a running env.

| Key | Required | Default | Description |
|-----|----------|---------|-------------|
| `:ext/namespace` | ✓ | — | Fully qualified symbol, e.g. `'com.blockether.vis.ext.common-operations.core`, `'com.acme.ext.git` |
| `:ext/doc` | ✓ | — | Extension-level description |
| `:ext/group` | ✓ | — | Top-level prompt group, e.g. `"knowledge"` |
| `:ext/subgroup` | ✗ | same as `:ext/group` | Finer-grained grouping within the group |
| `:ext/activation-fn` | ✗ | `(constantly true)` | `(fn [env] → bool)` — when falsy, all symbols are unbound and nudge-fn is skipped |
| `:ext/prompt` | ✗ | — | Optional extra string or `(fn [env] → string)` appended after the auto-rendered symbol prompt |
| `:ext/nudge-fn` | ✗ | — | `(fn [ctx] → string\|nil)` — per-iteration nudge composer (see [Nudge System](nudges.md)) |
| `:ext/on-parse-error-fn` | ✗ | — | `(fn [{:code :error :environment}] → string\|nil)` — catch-all source rewriter for SCI/edamame parse errors. Fires only when no symbol-level `:on-parse-error-fn` produced a rewrite. See [Hook Protocol](hooks.md). |
| `:ext/requires` | ✗ | `[]` | Vector of extension namespace symbols that must be registered first, e.g. `['com.blockether.vis.ext.common-operations.core]` |
| `:ext/version` | ✗ | — | Semver version string, e.g. `"1.0.0"`, `"0.3.1-SNAPSHOT"` |
| `:ext/author` | ✗ | — | Author name or org, e.g. `"Blockether"` |
| `:ext/license` | ✗ | — | SPDX license identifier, e.g. `"MIT"`, `"Apache-2.0"`, `"EPL-2.0"` |
| `:ext/symbols` | ✓ | — | Vector of symbol entries (from `symbol` / `value`) |
| `:ext/classes` | ✗ | `{}` | `{fq-symbol → Class}` — Java classes exposed in sandbox |
| `:ext/imports` | ✗ | `{}` | `{short-symbol → fq-symbol}` — short-name imports |
| `:ext/ns-alias` | conditional | — | `{:ns 'vis.ext.fs :alias 'fs}` — **required when `:ext/symbols` is non-empty** (enforced by `ns-alias-required-when-symbols?`). Creates a dedicated SCI namespace with that alias. Symbols are bound **only** into this namespace, never into `sandbox` directly. The alias is auto-required in the sandbox. The LLM must use `(fs/read-file ...)` — bare `(read-file ...)` does not resolve. |
| `:ext/cli` | ✗ | — | Vector of CLI entries `[{:cmd "name" :doc "…" :args […] :fn (fn [parsed-args] …)}]`. Each entry mounts as `vis ext <cmd>` via the `vis-commandline` adapter in `channels.cli`. Use this for occasional one-shot commands tied to your extension; for richer command trees, register a top-level command directly through `com.blockether.vis.commandline/register-global!`. |

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
| `:on-parse-error-fn` | ✗ | — | `(fn [{:code :error :sym :environment}] → string\|nil)` — source rewriter that fires when SCI/edamame rejects the LLM's source AND this symbol's name appears in the broken code. See [Hook Protocol](hooks.md). |

## `value` — constant binding

```clojure
(ext/value sym-name val opts) → validated value symbol entry
```

| Opt | Required | Description |
|-----|----------|-------------|
| `:doc` | ✓ | One-liner description |

## `render-prompt` — canonical prompt preview / renderer

```clojure
(ext/render-prompt opts) → prompt-string
```

Renders the canonical LLM-facing prompt text directly from extension
symbol metadata: function `:doc` + `:arglists`, plus value `:doc`
lines.

This is the same canonical block the loop prepends automatically for
active extensions. Call it when you want to preview how an extension
will look in the system prompt.

| Opt | Required | Description |
|-----|----------|-------------|
| `:ext/doc` or `:heading` | ✓ | Prompt heading |
| `:ext/ns-alias` | ✗ | If present, emits `use alias/ prefix` in the heading and renders calls as `(alias/fn ...)` |
| `:ext/symbols` | ✓ | Vector of `ext/symbol` / `ext/value` entries to render |
| `:usage-note` | ✗ | Extra heading note, e.g. `"positional args only"` |
| `:notes` | ✗ | String or seq of extra lines appended verbatim |

Example:

```clojure
(ext/render-prompt
  {:ext/doc "Filesystem tools"
   :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
   :ext/symbols [read-file-sym patch-file-sym]
   :usage-note "positional args only"})
```

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
(ns com.blockether.vis.ext.documents
  (:require [clojure.string :as str]
            [com.blockether.vis.extension :as ext]))

(defn- search-fn [query] ...)
(defn- search-with-opts [query opts] ...)

(def search-sym
  (ext/symbol 'search search-fn
    {:doc        "Full-text search across ingested documents."
     :arglists  '([query] [query opts])
     :examples  ["(docs/search \"neural\")"
                 "(docs/search \"attention\" {:limit 5})"]
     :before-fn (fn [env f args]
                  {:args (update args 0 str/lower-case)})
     :after-fn  (fn [env f args result]
                  {:result (take 10 result)})}))

(def max-results-sym
  (ext/value 'max-results 50
    {:doc "Maximum number of search results returned."}))

(def docs-ext
  (ext/extension
    {:ext/namespace     'com.blockether.vis.ext.documents
     :ext/doc           "Document search and retrieval"
     :ext/version       "1.0.0"
     :ext/author        "Blockether"
     :ext/license       "Apache-2.0"
     :ext/group         "knowledge"
     :ext/subgroup      "documents"
     :ext/ns-alias      {:ns 'vis.ext.docs :alias 'docs}
     :ext/requires      ['com.blockether.vis.ext.common-operations.core]
     :ext/prompt        "Prefer narrow searches before broad scans."
     :ext/activation-fn (fn [env] (seq (list-docs (:db-info env))))
     :ext/nudge-fn      (fn [{:keys [environment iteration prev-expressions]}]
                          (when (and (> iteration 5)
                                    (some :error prev-expressions))
                            "[system_nudge] Document searches are failing."))
     :ext/symbols       [search-sym max-results-sym]
     :ext/classes       {'java.time.LocalDate java.time.LocalDate}
     :ext/imports       {'LocalDate 'java.time.LocalDate}}))

;; Self-register at load time
(ext/register-global! docs-ext)
```

The LLM sees in the system prompt:

```
[namespace: docs → vis.ext.docs]
Document search and retrieval (use docs/ prefix)
- (docs/search query) or (docs/search query opts) — Full-text search across ingested documents.
- docs/max-results — Maximum number of search results returned.
Prefer narrow searches before broad scans.
```

And calls `(docs/search "neural")` from `:code` blocks.
Bare `(search "neural")` does **not** resolve.
