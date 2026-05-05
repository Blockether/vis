# Extension spec

## Auto-discovery

Extensions are auto-discovered from the unified `META-INF/vis-extension/vis.edn` on
the classpath. See [Overview — Classpath auto-discovery](overview.md#classpath-auto-discovery)
for the full convention.

## Building an extension

The constructor and registrar both live on the public facade
`com.blockether.vis.core` (shipped by the `com.blockether/vis` jar):

```clojure
(require '[com.blockether.vis.core :as sdk])

(sdk/extension spec)              ;; → validated extension map
(sdk/register-extension! ext-map) ;; global registration + slot dispatch
```

Every authoring helper an extension needs is on that namespace:
`extension`, `symbol`, `value`, `register-extension!`,
`install-extension!` (per-env companion), `render-prompt`,
`extension-provenance`, `extension-source-markers-of`,
`load-extension!`, `discover-extensions!`. Composition helpers used
by the runtime itself (`active-extensions`, `assemble-system-prompt`)
live there too, so an embedder needs only one require.

Only `:ext/namespace` and `:ext/doc` are unconditionally required.
Everything else is optional (with sensible defaults applied by the
`extension` constructor) so an extension that ships, say, only
`:ext/channels` doesn't need to declare any SCI-symbol bookkeeping.

Two conditional rules apply on top of the spec:

- `:ext/kind` is required when `:ext/symbols` is non-empty (enforced
  by `kind-required-when-symbols?`). For pure non-symbol extensions
  (channels-only / providers-only / persistence-only) the `extension`
  builder auto-derives `:ext/kind` — see the table row below.
- `:ext/ns-alias` is required when `:ext/symbols` is non-empty
  (enforced by `ns-alias-required-when-symbols?`).

| Key                      | Required        | Default              | Description |
|--------------------------|-----------------|----------------------|-------------|
| `:ext/namespace`         | ✓              | —                    | Fully qualified symbol, e.g. `'com.blockether.vis.ext.foundation.editing.core`, `'com.acme.ext.git`. Also the dedup key in the global registry. |
| `:ext/doc`               | ✓              | —                    | Extension-level description. |
| `:ext/kind`              | conditional     | auto                 | Top-level *kind* of surface this extension contributes — used for prompt rendering AND as the section heading in `vis extensions list`. **Required when `:ext/symbols` is non-empty.** Auto-derived for the categorical cases when not set: extensions contributing `:ext/providers` get `"providers"`, `:ext/channels` get `"channels"`, `:ext/persistance` get `"persistance"`. Explicit `:ext/kind` always wins. |
| `:ext/activation-fn`     | ✗              | `(constantly true)`  | `(fn [env] → bool)` — when falsy, all symbols are unbound and `:ext/nudge-fn` is skipped. |
| `:ext/prompt`            | ✗              | —                    | Optional extra string or `(fn [env] → string)` appended as an extension prompt block. Strings are normalized to `(constantly s)`. |
| `:ext/environment-info-fn` | ✗            | —                    | `(fn [env] → string\|seq\|map\|nil)` — live environment-info contribution rendered inside the system prompt's `<environment-info>` block. Use for cwd/repo/runtime facts; any active extension can add a sibling section. |
| `:ext/nudge-fn`          | ✗              | —                    | `(fn [ctx] → string\|{:importance :low\|:normal\|:high\|:critical :text string}\|nil)` — per-iteration nudge composer. Return value is spec-checked at runtime and rendered as `<system_nudge importance="...">` (see [Nudge System](nudges.md)). |
| `:ext/on-parse-error-fn` | ✗              | —                    | `(fn [{:code :error :environment}] → string\|nil)` — catch-all source rewriter for SCI/edamame parse errors. Fires only when no symbol-level `:on-parse-error-fn` produced a rewrite. See [Symbol Decorators](hooks.md). |
| `:ext/requires`          | ✗              | `[]`                  | Vector of extension namespace symbols that must be registered first, e.g. `['com.blockether.vis.ext.foundation.editing.core]`. |
| `:ext/version`           | ✗              | —                    | Semver version string, e.g. `"1.0.0"`, `"0.3.1-SNAPSHOT"`. |
| `:ext/author`            | ✗              | —                    | Author / creator of the extension itself — the entity that wrote the code. e.g. `"Blockether"`. |
| `:ext/owner`             | ✗              | —                    | Owner of the *package* / distribution that ships this extension. Distinct from `:ext/author`: a Blockether-authored extension may be vendored by another distribution. Every extension bundled in this repo declares `:ext/owner "vis"`. Surfaces as the `Owner` column in `vis extensions list`. |
| `:ext/license`           | ✗              | —                    | SPDX license identifier, e.g. `"MIT"`, `"Apache-2.0"`, `"EPL-2.0"`. |
| `:ext/symbols`           | ✗              | `[]`                  | Vector of symbol entries (from `symbol` / `value`). When non-empty, `:ext/kind` and `:ext/ns-alias` become required. |
| `:ext/classes`           | ✗              | `{}`                  | `{fq-symbol → Class}` — Java classes exposed in the SCI sandbox (`(java.time.LocalDate/now)` style). |
| `:ext/imports`           | ✗              | `{}`                  | `{short-symbol → fq-symbol}` — short-name imports for sandbox interop (`(LocalDate/now)` style). |
| `:ext/ns-alias`          | conditional     | —                    | `{:ns 'vis.ext.tools :alias 'vis}` — **required when `:ext/symbols` is non-empty**. Creates a dedicated SCI namespace with that alias. Symbols are bound **only** into this namespace, never into `sandbox` directly. The alias is auto-required in the sandbox. The LLM must use `(v/cat …)` — bare `(cat …)` does not resolve. |
| `:ext/cli`               | ✗              | `[]`                  | Vector of [`com.blockether.vis.core`](../architecture/packages.md#auto-discovery) command maps (`{:cmd/name … :cmd/doc … :cmd/run-fn … :cmd/args? :cmd/usage? :cmd/subcommands? :cmd/parent?}`). **Always auto-mounted under `vis extensions <cmd>`** — the dispatcher defaults `:cmd/parent` to `["extensions"]` for omitted values, and rejects entries whose `:cmd/parent` starts with anything other than `"extensions"` (`:type :ext/cli-bad-parent`). Top-level commands like `vis run` live outside the extension tree; they call `register-cmd!` directly. See the [CLI command slot](#cli-command-slot) section below for the three accepted forms. |
| `:ext/channels`          | ✗              | `[]`                  | Vector of channel descriptors (`{:channel/id :channel/cmd :channel/doc :channel/main-fn :channel/usage? :channel/owns-tty?}`). Each entry is forwarded to `register-channel!`; it appears under `vis channels <cmd>`. See [Channels](../architecture/channels.md). |
| `:ext/providers`         | ✗              | `[]`                  | Vector of LLM provider descriptors (`{:provider/id :provider/label :provider/auth-fn :provider/get-token-fn …}`). Each entry is forwarded to `register-provider!`. |
| `:ext/persistance`       | ✗              | `[]`                  | Vector of persistence-backend descriptors (`{:persistance/id <kw> :persistance/ns <fq-symbol>}`). Each entry is forwarded to `com.blockether.vis.core/register-backend!`. |

## CLI command slot

`:ext/cli` is the slot for commands an extension contributes to
`vis extensions <cmd>`. Three forms accepted:

### Flat entry

No `:cmd/parent` — the dispatcher inserts `["extensions"]` for you:

```clojure
:ext/cli [{:cmd/name   "blame"
           :cmd/doc    "Run git blame on a path."
           :cmd/run-fn #'cli-blame}]
```

→ `vis extensions blame`.

### Embedded subcommand vector

The entry carries its whole subcommand tree inline:

```clojure
:ext/cli [{:cmd/name "docker"
           :cmd/doc  "Docker operations."
           :cmd/subcommands [{:cmd/name "ps"   :cmd/doc "List." :cmd/run-fn #'docker-ps}
                             {:cmd/name "logs" :cmd/doc "Tail." :cmd/run-fn #'docker-logs}]}]
```

→ `vis extensions docker`, `vis extensions docker ps`,
`vis extensions docker logs`.

### Deeper nesting

Mount entries at any depth under `vis extensions …` by specifying a
`:cmd/parent` whose first element is `"extensions"`:

```clojure
:ext/cli [{:cmd/name "git"
           :cmd/doc  "Git operations."
           :cmd/subcommands #(sdk/registered-under ["extensions" "git"])}
          {:cmd/name   "status"
           :cmd/parent ["extensions" "git"]
           :cmd/doc    "Show git status."
           :cmd/run-fn #'git-status}
          {:cmd/name   "blame"
           :cmd/parent ["extensions" "git"]
           :cmd/doc    "Show git blame."
           :cmd/run-fn #'git-blame}]
```

→ `vis extensions git`, `vis extensions git status`,
`vis extensions git blame`.

### Rejected placements

```clojure
:ext/cli [{:cmd/name "rogue" :cmd/parent ["channels"] :cmd/run-fn ...}]
;; Throws ex-info with :type :ext/cli-bad-parent at register-extension! time.
```

`:ext/cli` is the registration slot for extension-owned commands. For top-level commands or other
placements (the binary's own built-ins, custom command trees), call
`com.blockether.vis.core/register-cmd!` directly. See
`src/com/blockether/vis/internal/main.clj` for an example: `vis run`,
`vis providers`, `vis doctor`, `vis conversations` are registered with
`registry/register-cmd!`; only the `vis extensions list` subcommand
goes through `:ext/cli`.

## Function binding

The `symbol` constructor produces a function entry for `:ext/symbols`:

```clojure
(sdk/symbol sym-name f opts) → validated fn symbol entry
```

| Opt | Required | Default | Description |
|-----|----------|---------|-------------|
| `:doc` | ✓ | — | One-liner shown in the sandbox var's docstring |
| `:arglists` | ✓ | — | Argument signatures, e.g. `'([query] [query opts])` |
| `:examples` | ✗ | derived from `:arglists` | Usage examples injected into system prompt |
| `:before-fn` | ✗ | — | `(fn [env f args] → map)` — entry decorator (transform args / short-circuit). See [Symbol Decorators](hooks.md). |
| `:after-fn` | ✗ | — | `(fn [env f args result] → map)` — exit decorator (transform result). See [Symbol Decorators](hooks.md). |
| `:on-error-fn` | ✗ | — | `(fn [err env f args] → map)` — error decorator (recover, retry, or rethrow). See [Symbol Decorators](hooks.md). |
| `:on-parse-error-fn` | ✗ | — | `(fn [{:code :error :sym :environment}] → string\|nil)` — parse rescue (not a decorator) that fires when SCI/edamame rejects the LLM's source AND this symbol's name appears in the broken code. See [Symbol Decorators](hooks.md). |

## Constant binding

The `value` constructor produces a non-fn entry for `:ext/symbols`:

```clojure
(sdk/value sym-name val opts) → validated value symbol entry
```

| Opt | Required | Description |
|-----|----------|-------------|
| `:doc` | ✓ | One-liner description |

## Prompt preview helper

`render-prompt` returns the canonical LLM-facing prompt block for a
set of symbols, exactly as the loop would render it:

```clojure
(sdk/render-prompt opts) → prompt-string
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
| `:ext/symbols` | ✓ | Vector of `sdk/symbol` / `sdk/value` entries to render |
| `:usage-note` | ✗ | Extra heading note, e.g. `"positional args only"` |
| `:notes` | ✗ | String or seq of extra lines appended verbatim |

Example:

```clojure
(sdk/render-prompt
  {:ext/doc "Filesystem tools"
   :ext/ns-alias {:ns 'vis.ext.tools :alias 'vis}
   :ext/symbols [read-file-sym patch-sym]
   :usage-note "positional args only"})
```

## SCI binding helper

`wrap-extension` is the lower-level helper the runtime uses to attach
an extension's symbols to a live env:

```clojure
(ext/wrap-extension ext env) → {sym → fn-or-value}
```

Wraps every function symbol through `invoke-symbol-wrapper`
(before → fn → after, with on-error recovery). When the public return
value is a tool-result envelope, the wrapper also stamps canonical
extension provenance onto `[:provenance :tool]`, `[:provenance :extension]`,
and `[:provenance :source]`. Value symbols are returned as `{sym → value}`.
Each wrapped fn closes over the extension, symbol entry, and environment.

## Provenance helpers

`extension-provenance` returns the canonical extension-level provenance
map the runtime reuses everywhere:

```clojure
(sdk/extension-provenance ext)
;; => {:namespace 'com.acme.ext.git
;;     :alias 'git
;;     :doc "Git integration"
;;     :kind "vcs"
;;     :version "1.2.3"
;;     :author "Acme"
;;     :owner "acme-suite"
;;     :license "Apache-2.0"
;;     :registry-id 'git
;;     :source-paths ["/abs/.../core.clj" ...]
;;     :source-mtime-max 1714403520000
;;     :source-hash-sha256 "abc..."}
```

The same map feeds:

- `TURN_ACTIVE_EXTENSIONS`
- `(v/extensions)`
- tool-result enrichment in `invoke-symbol-wrapper`

`extension-source-markers-of` reads the cached source-marker sidecar
for a registered extension:

```clojure
(sdk/extension-source-markers-of 'com.acme.ext.git)
;; => {:source-paths [...]
;;     :source-mtime-max ...
;;     :source-hash-sha256 ...}
```

Source markers are resolved from the classpath source/jar entries and
are deterministic across reloads for unchanged content.

## Standalone validation

`validate!` is the spec check the `extension` constructor uses
internally. Safe to call directly when you want to validate a map
you built by hand:

```clojure
(ext/validate! ext) → normalized ext (or throws)
```

Normalizes `:ext/prompt` (string → fn) then checks the spec.
Called internally by `extension`; safe to call standalone.

> **Note:** `:ext/prompt` accepts `string` or `fn?`. Both `extension`
> and `validate!` normalize strings to `(constantly s)` before
> validation. `:ext/environment-info-fn` is always a function; return
> `nil` or blank text when the extension has no facts to add.

## Full example

```clojure
(ns com.blockether.vis.ext.documents
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as sdk]))

(defn- search-fn [query] ...)
(defn- search-with-opts [query opts] ...)

(def search-sym
  (sdk/symbol 'search search-fn
    {:doc        "Full-text search across ingested documents."
     :arglists  '([query] [query opts])
     :examples  ["(docs/search \"neural\")"
                 "(docs/search \"attention\" {:limit 5})"]
     :before-fn (fn [env f args]
                  {:args (update args 0 str/lower-case)})
     :after-fn  (fn [env f args result]
                  {:result (take 10 result)})}))

(def max-results-sym
  (sdk/value 'max-results 50
    {:doc "Maximum number of search results returned."}))

(def docs-ext
  (sdk/extension
    {:ext/namespace     'com.blockether.vis.ext.documents
     :ext/doc           "Document search and retrieval"
     :ext/version       "1.0.0"
     :ext/author        "Blockether"
     :ext/owner         "vis"
     :ext/license       "Apache-2.0"
     :ext/kind          "knowledge"
     :ext/ns-alias      {:ns 'vis.ext.docs :alias 'docs}
     :ext/requires      ['com.blockether.vis.ext.foundation.editing.core]
     :ext/prompt        "Prefer narrow searches before broad scans."
     :ext/environment-info-fn
     (fn [env]
       (when-let [root (:docs/root env)]
         (str "docs.root: " root)))
     :ext/activation-fn (fn [env] (seq (list-docs (:db-info env))))
     :ext/nudge-fn      (fn [{:keys [environment iteration previous-blocks]}]
                          (when (and (> iteration 5)
                                    (some :error previous-blocks))
                            {:importance :high
                             :text "Document searches are failing."}))
     :ext/symbols       [search-sym max-results-sym]
     :ext/classes       {'java.time.LocalDate java.time.LocalDate}
     :ext/imports       {'LocalDate 'java.time.LocalDate}}))

;; Self-register at load time
(sdk/register-extension! docs-ext)
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
