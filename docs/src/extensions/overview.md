# Extension System

> **Library:** `com.blockether/vis-extension` (standalone Clojure deps
> alias: `{:local/root "vis-extension"}` or, when published, the Maven
> coordinate). Single namespace: `com.blockether.vis.extension`.
>
> **Why a separate library:** the extension contract is intentionally
> decoupled from the vis runtime. An extension only needs telemere +
> clojure.spec, NOT the SCI sandbox, the SQLite stack, or any of the
> channels. Older versions re-exported the contract through
> `com.blockether.vis.core` — that re-export is **gone**. Authoring
> code must require `com.blockether.vis.extension` directly.
>
> **What stays in vis-core:** the runtime composition helpers that
> need a live environment (`active-extensions`, `assemble-system-prompt`,
> `register-extension!` for ad-hoc per-env registration). Those live
> on `com.blockether.vis.core` because they need the runtime they
> compose against.

Extensions are the **only** way to add symbols, classes, and documentation
to the SCI sandbox. An extension is a namespace-like bundle that groups
related tools, constants, prompt context, and per-iteration nudges into
a single validated unit.

## What an Extension Can Do

1. **Bind functions** into an aliased namespace - the LLM calls `(alias/fn ...)` from `:code`
2. **Bind constants** - data the LLM references via the alias prefix
3. **Inject prompt context** - LLM-facing docs in the system prompt
4. **Emit per-iteration nudges** - situational hints (budget, errors, etc.)
5. **Expose Java classes** - enable `(LocalDate/now)` style interop
6. **Guard activation** - conditionally enable/disable based on env state

## Registration

Two ways to register extensions:

### Global Registry (recommended)

Call `register-global!` at namespace load time. When any environment
is created, all global extensions are automatically installed in
dependency order.

```clojure
(ns my.company.ext.git
  (:require [....extension :as ext]))

(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.acme.ext.git
     :ext/requires  ['com.blockether.vis.ext.common-operations.core]
     :ext/doc       "Git integration"
     ...}))
```

Drop the jar on the classpath → namespace loads → extension
self-registers → every new environment gets it.

### Auto-Discovery from Classpath (recommended)

Extensions can be discovered **automatically** without any manual
`require`. Place a single `META-INF/vis.edn` file in your extension's
`resources/` directory listing every namespace whose load should
trigger registration:

```edn
[com.acme.ext.git
 com.acme.ext.search]
```

When `create-environment` runs (or the CLI dispatcher boots), it
calls `com.blockether.vis.extension/discover-extensions!` which:

1. Scans the classpath for **all** `META-INF/vis.edn` files
   (via `ClassLoader.getResources`)
2. Reads each file as a vector of namespace symbols
3. `require`s each namespace (triggering its `register-global!` call)
4. De-duplicates so a namespace listed in two jars is required only once
5. Logs every success at `:info` and every failure at `:error`

The loader is **type-agnostic**: the same `META-INF/vis.edn` resource
holds the namespaces for ext symbols, channels, CLI commands,
providers, and persistence backends. Whichever `register-global!`
(or `register-backend!`) the loaded namespace happens to call decides
which subsystem registry it ends up in.

This means: add the extension jar/local-root to your deps.edn aliases,
ensure it has a `META-INF/vis.edn` in its resources, and it will be
loaded automatically. No imports, no requires, no wiring.

**Directory layout for an extension:**

```
extensions/my-ext/
├── deps.edn                  ;; {:paths ["src" "resources"] ...}
├── resources/
│   └── META-INF/vis.edn      ;; [com.acme.ext.my-tool]
└── src/com/acme/ext/my_tool.clj  ;; calls register-global! at load time
```

**deps.edn alias:**

```clojure
:run {:extra-deps {com.acme.ext/my-tool {:local/root "extensions/my-ext"}}}
```

### Dynamic Loading

An extension can load other extensions at runtime:

```clojure
(ext/load-extension! 'my.company.ext.git)
;; => requires the ns, triggers register-global!, returns the ext
```

This is how meta-extensions (extension packs) work - one extension
`require`s others dynamically.

### Per-Environment (ad-hoc)

```clojure
(register-extension! environment my-ext)
```

For extensions that shouldn't be global.

## Lifecycle

From classpath jar to live tool call:

0. **discover-extensions!** — scan `META-INF/vis.edn` on the classpath
1. **ext/extension** — build and validate the extension spec
2. **register-global!** — add to the process-level registry
3. **Topo-sort** — order by `:ext/requires` dependencies (throws `missing-dependencies` if a required extension is absent)
4. **Install** — bind symbols into the aliased SCI namespace; auto-require the alias in `sandbox`
5. **Prompt** — auto-render canonical symbol docs, prepend `[namespace: alias → ns]`, then append the optional `:ext/prompt` tail
6. **Activation (per query)** — `:ext/activation-fn` check; when falsy, symbols stay unbound and `nudge-fn` is skipped for the whole turn
7. **Nudge (per iteration)** — active extensions' `:ext/nudge-fn` is invoked
8. **Hooks (per call)** — `:before-fn` → `:fn` → `:after-fn`, with `:on-error-fn` catching `:fn` errors

## Namespace Aliases (required when `:ext/symbols` is non-empty)

An extension that ships any `:ext/symbols` **must** declare
`:ext/ns-alias` — a map with `:ns` (the full SCI namespace symbol)
and `:alias` (the short alias the LLM uses). Pure prompt-only
extensions (no `:ext/symbols`) may omit it. When present, extension
symbols are bound **only** into this dedicated namespace, **never**
into the `sandbox` namespace directly, and the LLM must always use
the alias prefix.

```clojure
(ext/extension
  {:ext/namespace 'com.blockether.vis.ext.common-operations.core
   :ext/ns-alias  {:ns 'vis.ext.fs :alias 'fs}
   ...})
```

At `register-extension!` time:
1. A SCI namespace `vis.ext.fs` is created with all wrapped symbols
2. The alias `fs` is registered in the SCI context
3. `(require '[vis.ext.fs :as fs])` is auto-evaluated in the sandbox
4. The LLM calls `(fs/read-file ...)`, `(fs/list-files ...)`, etc.
5. Bare `(read-file ...)` does **not** resolve - the alias is mandatory

The system prompt auto-prepends a namespace header to each extension's
prompt block:

```
[namespace: fs → vis.ext.fs]
Filesystem tools (use fs/ prefix):
- (fs/read-file path) ...
```

Extension-declared `:ext/classes` and `:ext/imports` are also injected
into the SCI context, so `(LocalDate/now)` works if an extension
exposes `java.time.LocalDate`.

## Prompt Injection

Every active extension contributes a prompt block to the **system
prompt** at the start of each query. This is how the LLM knows which
tools are available in the sandbox.

The canonical tool section is rendered automatically inside the loop
from the extension's `:ext/doc`, `:ext/ns-alias`, and `:ext/symbols`
metadata. `:ext/prompt` is only the optional extra tail appended after
that canonical block.

Use `ext/render-prompt` when you want to preview how the canonical
block will look.

`loop-core/assemble-system-prompt` is the **single function** that
builds the complete system message. It:

1. Builds the core system prompt (`CORE_SYSTEM_PROMPT` + date +
   environment block + optional caller instructions)
2. Collects extension prompts: for each extension where
   `(:ext/activation-fn ext) environment` is truthy, it renders the
   canonical symbol-derived block and then evaluates `(:ext/prompt ext)`
   when present
3. Joins all active prompt blocks with `\n\n` and appends to the core prompt

Both iteration loop paths (`loop/core.clj` and `query/core.clj`) and
the TUI `[?]` inspector (`conversation/core.clj :: effective-system-prompt`)
call this same function — zero duplication, zero drift.

If an extension's `activation-fn` or `prompt` fn throws, the error is
logged at `:error` level and that extension's prompt is skipped —
the query still runs.

## Quick Example

```clojure
(ns com.acme.ext.search
  ;; Require the slim contract directly. The full vis runtime is NOT
  ;; needed (and not on the classpath) when this jar is consumed by
  ;; another extension or a sandbox host.
  (:require [com.blockether.vis.extension :as ext]))

(defn- search-fn [query] ...)

(def find-symbol
  (ext/symbol 'find search-fn
    {:doc      "Full-text search."
     :arglists '([query])
     :examples ["(search/find \"neural\")"]}))

(def search-ext
  (ext/extension
    {:ext/namespace     'com.acme.ext.search
     :ext/doc           "Document search"
     :ext/group         "knowledge"
     :ext/ns-alias      {:ns 'vis.ext.search :alias 'search}
     :ext/prompt        "Prefer search before manual file scans."
     :ext/symbols       [find-symbol]}))

;; Self-register at load time
(ext/register-global! search-ext)
```

The LLM sees in the system prompt:

```
[namespace: search → vis.ext.search]
Document search (use search/ prefix)
- (search/find query) — Full-text search.
Prefer search before manual file scans.
```

And calls `(search/find "neural")` from `:code` blocks. Bare
`(find "neural")` does not resolve.

## Sections

- [Extension Spec](spec.md) - all keys, defaults, validation
- [Symbol Decorators](hooks.md) - `:before-fn`, `:after-fn`, `:on-error-fn` (decorators around the target fn) + `:on-parse-error-fn` (parse rescue)
- [Environment Map](environment.md) - every key in the environment
- [Nudge System](nudges.md) - built-in + extension nudges
