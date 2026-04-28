# Extension system

> **Library:** `com.blockether/vis-core`. Extension authoring lives in
> the namespace `com.blockether.vis.extension`.
>
> **Why a dedicated namespace:** the extension contract is not re-exported
> through `com.blockether.vis.core`. Authoring code must require
> `com.blockether.vis.extension` directly.
>
> **What stays on the public facade:** the runtime composition helpers
> that need a live environment (`active-extensions`,
> `assemble-system-prompt`, `register-extension!` for ad-hoc per-env
> registration). Those live on `com.blockether.vis.core` because they
> need the runtime they compose against.

Extensions are the **only** way to add symbols, classes, and documentation
to the SCI sandbox. An extension is a namespace-like bundle that groups
related tools, constants, prompt context, and per-iteration nudges into
a single validated unit.

## What an extension can do

An extension is a single `(ext/extension {…})` data map declaring
zero or more of FIVE surfaces. The same `(ext/register-global! …)`
call handles all of them; the registrar dispatches each populated
slot to its matching sub-registry as a side effect.

| Slot | Purpose | Where the user sees it |
|------|---------|------------------------|
| `:ext/symbols`     | Functions / constants bound into the SCI sandbox under an alias. | LLM `:code` calls `(alias/fn …)`. |
| `:ext/cli`         | CLI subcommands the extension contributes. **Always auto-mount under `vis extensions <cmd>`**; deeper nests like `vis extensions git status` are allowed via `:cmd/parent ["extensions" "git"]`. | `vis extensions <name>`. |
| `:ext/channels`    | User-facing front-ends (TUI, Telegram bot, web hook) registered as `vis channels <id>`. | `vis channels <id>`. |
| `:ext/providers`   | LLM auth providers (OAuth + token exchange). | `vis auth <id>`. |
| `:ext/persistance` | Concrete backend implementations of the persistence facade. | Picked up automatically by `persistance.core/create-rlm-conn`. |

Alongside those surfaces, every extension may also:

- **Inject prompt context** — LLM-facing docs in the system prompt (`:ext/prompt`, derived from `:ext/symbols` metadata).
- **Emit per-iteration nudges** — situational hints (budget, errors, …) via `:ext/nudge-fn`.
- **Expose Java classes** — enable `(LocalDate/now)` style interop via `:ext/classes` / `:ext/imports`.
- **Guard activation** — conditionally enable/disable based on env state via `:ext/activation-fn`.
- **Declare dependencies** on other extensions via `:ext/requires`.

No extension is required to populate every slot — a TUI jar fills only `:ext/channels`, a backend jar fills only `:ext/persistance`, a tools jar fills only `:ext/symbols`, etc.

## Registration

Two ways to register extensions:

### Global registry (recommended)

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

### Classpath auto-discovery

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
holds the namespaces for every surface (SCI sandbox symbols,
channels, CLI commands, providers, persistance entries). The author
declares everything in one `(ext/extension {…})` map and calls
`(ext/register-global! …)` once; the registrar dispatches each
populated `:ext/<slot>` (`:ext/symbols`, `:ext/cli`, `:ext/channels`,
`:ext/providers`, `:ext/persistance`) to its matching sub-registry as
a side effect. The lower-level `register-global!` calls
(`channel/register-global!`, `cmd/register-global!`,
`provider/register-global!`, `persistance.core/register-backend!`)
remain available for embedded / programmatic use, and are still what
`ext/register-global!` ultimately invokes.

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
:dev {:extra-deps {com.acme.ext/my-tool {:local/root "extensions/my-ext"}}}
```

### Dynamic loading

An extension can load other extensions at runtime:

```clojure
(ext/load-extension! 'my.company.ext.git)
;; => requires the ns, triggers register-global!, returns the ext
```

This is how meta-extensions (extension packs) work - one extension
`require`s others dynamically.

### Per-environment (ad-hoc)

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

## Namespace aliases

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
   :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
   ...})
```

At `register-extension!` time:
1. A SCI namespace `vis.ext.tools` is created with all wrapped symbols
2. The alias `vis` is registered in the SCI context
3. `(require '[vis.ext.tools :as vis])` is auto-evaluated in the sandbox
4. The LLM calls `(vis/cat ...)`, `(vis/ls ...)`, `(vis/rg ...)`, etc.
5. Bare `(read-file ...)` does **not** resolve - the alias is mandatory

The system prompt auto-prepends a namespace header to each extension's
prompt block:

```
[namespace: vis → vis.ext.tools]
Filesystem tools (use vis/ prefix):
- (vis/cat path) ...
- (vis/ls ".") ...
- (vis/rg pattern ".") ...
```

Extension-declared `:ext/classes` and `:ext/imports` are also injected
into the SCI context, so `(LocalDate/now)` works if an extension
exposes `java.time.LocalDate`.

## Prompt injection

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

## Quick example

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

(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.acme.ext.search
     :ext/doc       "Document search"
     :ext/group     "knowledge"
     :ext/ns-alias  {:ns 'vis.ext.search :alias 'search}
     :ext/prompt    "Prefer search before manual file scans."
     :ext/symbols   [find-symbol]}))
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

## One example per surface

Real in-tree examples — every package below ships exactly one
`META-INF/vis.edn` and exactly one `(ext/register-global! …)` call.

### Sandbox tools

`extensions/vis-common-operations/.../core.clj` — read/list/grep/patch:

```clojure
(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.common-operations.core
     :ext/doc       "Common Vis operations: cat, ls, rg, patch."
     :ext/version   "0.4.0"
     :ext/group     "filesystem"
     :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
     :ext/symbols   all-symbols
     :ext/prompt    combined-prompt}))
```

### CLI commands

`packages/vis-core/.../channels/cli.clj` ships the `vis extensions list`
subcommand:

```clojure
(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.blockether.vis.channels.cli
     :ext/doc       "vis-core's contribution to the `vis extensions` subtree."
     :ext/cli       [{:cmd/name   "list"
                      :cmd/doc    "List every registered extension."
                      :cmd/usage  "vis extensions list"
                      :cmd/run-fn cli-extensions!}]}))
```

`:ext/cli` ALWAYS mounts under `["extensions"]`. The dispatcher
defaults `:cmd/parent` for entries that omit it. Three forms work:

1. **Flat** (most common): just `:cmd/name` + `:cmd/run-fn` → `vis extensions <name>`.
2. **Embedded subcommands**: the entry carries its own `:cmd/subcommands` vector → the whole tree mounts at `vis extensions <name>` and below.
3. **Deeper nest via parent path**: each entry sets `:cmd/parent ["extensions" "git"]` etc. → `vis extensions git <name>`.

Any `:cmd/parent` that doesn't start with `"extensions"` is rejected
at registration time with `:type :ext/cli-bad-parent`. Top-level
binary commands (`vis run`, `vis auth`, …) are NOT extension
commands; they use `cmd/register-global!` directly inside vis-core.

See [Extension Spec — CLI command slot](spec.md#cli-command-slot) for
full examples of each form.

### Channel front-end

`packages/vis-tui/.../tui/screen.clj`:

```clojure
(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.blockether.vis.channels.tui.screen
     :ext/doc       "Lanterna-based terminal UI channel."
     :ext/version   "0.3.0"
     :ext/channels  [{:channel/id        :tui
                      :channel/cmd       "tui"
                      :channel/doc       "Interactive terminal UI."
                      :channel/usage     "vis tui [--conversation-id ID | --resume]"
                      :channel/owns-tty? true
                      :channel/main-fn   #'channel-main}]}))
```

### LLM provider

`packages/vis-provider-github-copilot/.../github_copilot.clj`:

```clojure
(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.blockether.vis.providers.github-copilot
     :ext/doc       "GitHub Copilot OAuth + token-exchange provider."
     :ext/version   "0.3.0"
     :ext/providers [{:provider/id           :github-copilot
                      :provider/label        "GitHub Copilot"
                      :provider/status-fn    #'status
                      :provider/logout-fn    #'logout!
                      :provider/detect-fn    #'detect-oauth-token
                      :provider/auth-fn      #'interactive-auth!
                      :provider/get-token-fn #'get-copilot-token!}]}))
```

### Persistence backend

`packages/vis-persistance-sqlite/.../sqlite/core.clj`:

```clojure
(ext/register-global!
  (ext/extension
    {:ext/namespace   'com.blockether.vis.persistance.sqlite.core
     :ext/doc         "SQLite + Flyway persistence backend."
     :ext/version     "0.3.0"
     :ext/persistance [{:persistance/id :sqlite
                        :persistance/ns 'com.blockether.vis.persistance.sqlite.core}]}))
```

### Multiple surfaces at once

Nothing prevents an extension from filling many slots at once —
an extension that ships SCI tools, a CLI command, AND a channel:

```clojure
(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.acme.ext.git
     :ext/doc       "Git integration: SCI tools + CLI + a web-hook channel."
     :ext/group     "vcs"
     :ext/ns-alias  {:ns 'vis.ext.git :alias 'git}
     :ext/symbols   [git-status-sym git-blame-sym]
     :ext/cli       [{:cmd/name   "blame"
                      :cmd/doc    "Run git blame on a path."
                      :cmd/run-fn #'cli-blame}]
     :ext/channels  [{:channel/id      :git-webhook
                      :channel/cmd     "git-webhook"
                      :channel/doc     "Receive GitHub webhooks."
                      :channel/main-fn #'webhook-main}]}))
```

The registrar walks each slot in turn. The user gets:

- `(git/status)` / `(git/blame ...)` in the SCI sandbox
- `vis extensions blame` on the CLI
- `vis channels git-webhook` for the webhook front-end

## Where next

- [Extension Spec](spec.md) - all keys, defaults, validation
- [Symbol Decorators](hooks.md) - `:before-fn`, `:after-fn`, `:on-error-fn` (decorators around the target fn) + `:on-parse-error-fn` (parse rescue)
- [Environment Map](environment.md) - every key in the environment
- [Nudge System](nudges.md) - built-in + extension nudges
