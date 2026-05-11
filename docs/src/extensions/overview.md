# Extension system

> **Library:** `com.blockether/vis`. Authoring code requires the
> public facade `com.blockether.vis.core` directly. Every helper an
> extension needs (`extension`, `symbol`, `value`,
> `register-extension!`, `install-extension!`, `render-prompt`,
> `discover-extensions!`, `active-extensions`, `assemble-system-prompt`)
> is re-exported from there.

Extensions are the **only** way to add symbols, classes, documentation,
and extension-owned renderers to Vis. An extension is a namespace-like bundle
that groups related tools, constants, prompt context, renderers, and
per-iteration nudges into a single validated unit.

## What an extension can do

An extension is a single `(sdk/extension {...})` data map declaring
zero or more surfaces. A single `(sdk/register-extension! ...)` call handles
all of them; the registrar dispatches each populated slot to its matching
sub-registry as a side effect.

| Slot | Purpose | Where the user sees it |
|------|---------|------------------------|
| `:ext/symbols`     | Functions / constants bound into the SCI sandbox under an alias. | LLM `:code` calls `(alias/fn ...)`. |
| `:ext/cli`         | CLI subcommands the extension contributes. **Always auto-mount under `vis extensions <cmd>`**; deeper nests like `vis extensions git status` are allowed via `:cmd/parent ["extensions" "git"]`. | `vis extensions <name>`. |
| `:ext/channels`    | User-facing front-ends (TUI, Telegram bot, web hook) registered as `vis channels <id>`. | `vis channels <id>`. |
| `:ext/providers`   | LLM auth providers (OAuth + token exchange). | `vis providers auth <id>`. |
| `:ext/persistance` | Concrete backend implementations of the persistence facade. | Picked up automatically by `db-create-connection!`. |
| `:ext/fenced-renderers` | Renderers for Markdown fenced code blocks such as `mermaid`. | TUI Markdown projection, with fallback to normal code blocks. |

Alongside those surfaces, every extension may also:

- **Inject prompt context** - static LLM-facing docs via `:ext/prompt` and live environment facts via `:ext/environment-info-fn`.
- **Emit lifecycle nudges** - situational hints (budget, errors, ...) via pre-phase `:ext/hooks`.
- **Expose Java classes** - enable `(LocalDate/now)` style interop via `:ext/classes` / `:ext/imports`.
- **Guard activation** - conditionally enable/disable based on env state via `:ext/activation-fn`.
- **Declare dependencies** on other extensions via `:ext/requires`.

No extension is required to populate every slot - a TUI jar fills only `:ext/channels`, a backend jar fills only `:ext/persistance`, a tools jar fills only `:ext/symbols`, etc.

## Registration

Two ways to register extensions:

### Global registry (recommended)

Call `(sdk/register-extension! ...)` at namespace load time. When any
environment is created, all globally registered extensions are
automatically installed in dependency order.

```clojure
(ns my.company.ext.git
  (:require [com.blockether.vis.core :as sdk]))

(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.acme.ext.git
     :ext/requires  ['com.blockether.vis.ext.foundation.editing.core]
     :ext/doc       "Git integration"
     ...}))
```

Drop the jar on the classpath -> namespace loads -> extension
self-registers -> every new environment gets it.

### Classpath auto-discovery

Extensions are discovered **automatically** without any manual
`require`. Place a single `META-INF/vis-extension/vis.edn` file in
your extension's `resources/` directory:

```edn
{my-tool
 {:nses [com.acme.ext.my-tool]
  :docs {"README.md"
         {:created-at  #inst "2026-04-29"
          :description "One-paragraph LLM-facing summary."
          :content     "# my-tool\n..."}}}}
```

When `create-environment` runs (or the CLI dispatcher boots), it
calls `com.blockether.vis.core/discover-extensions!` which:

1. Scans the classpath for **all** `META-INF/vis-extension/vis.edn` resources
   (via `ClassLoader.getResources`).
2. Parses each as the EDN map shape above (`{<id> {:nses [...] :docs {...}}}`).
3. `require`s every namespace listed under `:nses` exactly once across
   all URLs, triggering each ns's `(sdk/register-extension! ...)` call as
   a side effect.
4. Validates declared docs (rejecting entries without `:description`
   or `:content`), and inverts authored `:links` into `:reflinks` on
   target descriptors.
5. Logs every success at `:info` and every failure at `:error`.

The loader is **type-agnostic**: the same
`META-INF/vis-extension/vis.edn` resource holds the namespaces for
every surface (SCI sandbox symbols, channels, CLI commands,
providers, persistance entries). The author declares everything in
one `(sdk/extension {...})` map and calls `(sdk/register-extension! ...)`
once; the registrar dispatches each populated `:ext/<slot>` to its
matching sub-registry as a side effect. The lower-level
`register-cmd!` / `register-channel!` / `register-provider!` /
`register-backend!` calls (also re-exported from
`com.blockether.vis.core`) remain available for embedded /
programmatic use and are what `register-extension!` ultimately
invokes.

This means: add the extension jar/local-root to your `deps.edn`,
ensure it has a `META-INF/vis-extension/vis.edn` in its resources,
and it will be loaded automatically. No imports, no requires, no
wiring.

For user-installed source extensions, the Unix `bin/vis` launcher also scans
`<project>/.vis/vis-extensions/*/deps.edn` and
`~/.vis/vis-extensions/*/deps.edn` before JVM startup, adding each extension
project as a `:local/root` dependency. Use:

```bash
vis extensions scaffold my-tools
```

to create `.vis/vis-extensions/my-tools` with a minimal `deps.edn`, manifest,
and namespace. Move or scaffold under `~/.vis/vis-extensions` when the
extension should load for every project.

**Directory layout for an extension:**

```
extensions/<category>/<name>/
├── deps.edn                              ;; {:paths ["src" "resources"] ...}
├── resources/
│   └── META-INF/vis-extension/vis.edn     ;; {<id> {:nses [...] :docs {...}}}
└── src/com/blockether/vis/ext/<id>/core.clj
                                          ;; calls (sdk/register-extension! ...) at load
```

**deps.edn alias:**

```clojure
:dev {:extra-deps {com.acme.ext/my-tool {:local/root "extensions/my-ext"}}}
```

### Dynamic loading

An extension can load other extensions at runtime:

```clojure
(sdk/load-extension! 'my.company.ext.git)
;; => requires the ns, triggers register-extension!, returns the ext
```

This is how foundation-extensions (extension packs) work - one extension
`require`s others dynamically.

### Per-environment (ad-hoc)

```clojure
(sdk/install-extension! environment my-ext)
```

For extensions that shouldn't be global. `install-extension!` is the
per-env companion to the global `register-extension!`.

## Lifecycle

From classpath jar to live tool call:

0. **discover-extensions!** - scan `META-INF/vis-extension/vis.edn` on the classpath
1. **sdk/extension** - build and validate the extension spec
2. **sdk/register-extension!** - add to the process-level registry; slot dispatcher fans every populated `:ext/<slot>` out to its sub-registry
3. **Topo-sort** - order by `:ext/requires` dependencies (throws `missing-dependencies` if a required extension is absent)
4. **Install** - bind symbols into the aliased SCI namespace; auto-require the alias in `sandbox`
5. **Prompt** - collect active `:ext/environment-info-fn` sections, then append each active extension's optional `:ext/prompt` block under `[namespace: alias -> ns]`
6. **Activation (per turn)** - `:ext/activation-fn` check; when falsy, symbols stay unbound and prompt/nudge hooks are skipped for the whole turn
7. **Lifecycle hooks** - active extensions' `:ext/hooks` are invoked by namespaced phase:
   - pre phases (`:session/start`, `:turn/start`, `:turn.iteration/start`) in `prompt/build-iteration-context`
   - hard answer policy phase (`:turn.answer/validate`) in `loop/final-answer-gate-error`
   - post phases (`:turn.iteration/stop`, `:turn/stop`) in `loop/emit-post-hooks!`
8. **Symbol decorators (per call)** - `:before-fn` -> `:fn` -> `:after-fn`, with `:on-error-fn` catching `:fn` errors

## Namespace aliases

An extension that ships any `:ext/symbols` **must** declare
`:ext/ns-alias` - a map with `:ns` (the full SCI namespace symbol)
and `:alias` (the short alias the LLM uses). Pure prompt-only
extensions (no `:ext/symbols`) may omit it. When present, extension
symbols are bound **only** into this dedicated namespace, **never**
into the `sandbox` namespace directly, and the LLM must always use
the alias prefix.

```clojure
(ext/extension
  {:ext/namespace 'com.blockether.vis.ext.foundation.editing.core
   :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
   ...})
```

At `register-extension!` time:
1. A SCI namespace `vis.ext.tools` is created with all wrapped symbols
2. The alias `vis` is registered in the SCI context
3. `(require '[vis.ext.tools :as vis])` is auto-evaluated in the sandbox
4. The LLM calls `(v/cat ...)`, `(v/ls ...)`, `(v/rg ...)`, etc.
5. Bare `(read-file ...)` does **not** resolve - the alias is mandatory

The system prompt auto-prepends a namespace header to each extension's
prompt block:

```
[namespace: vis -> vis.ext.tools]
Filesystem tools (use v/ prefix):
- (v/cat path) ...
- (v/ls ".") ...
- (v/rg {:any ["foo" "bar"] :paths ["."]}) ...
```

Extension-declared `:ext/classes` and `:ext/imports` are also injected
into the SCI context, so `(LocalDate/now)` works if an extension
exposes `java.time.LocalDate`.

## Prompt injection

Every active extension can contribute two system-prompt surfaces at
turn start:

- `:ext/environment-info-fn` - live facts rendered inside the shared
  `<environment-info>` block. Use this for cwd, repository state,
  runtime flags, external service status, or other changing context.
- `:ext/prompt` - static or semi-static tool guidance rendered as an
  extension prompt block. The runtime does **not** auto-render every
  `:ext/symbols` entry; extensions that want a tool list call
  `ext/render-prompt` inside their own `:ext/prompt`.

`loop-core/assemble-system-prompt` is the **single function** that
builds the complete system message. It:

1. Builds the core system prompt (`build-system-prompt` + optional
   caller instructions)
2. Collects environment-info sections from active extensions and wraps
   them in `<environment-info>`
3. Collects active extension prompt blocks by evaluating `(:ext/prompt ext)`
   when present
4. Joins all active prompt blocks with `\n\n` and appends to the core prompt

All iteration loop paths call this same function - zero duplication, zero drift.

If an extension's `activation-fn` or `prompt` fn throws, the error is
logged at `:error` level and that extension's prompt is skipped -
the turn still runs.

## Quick example

```clojure
(ns com.acme.ext.search
  (:require [com.blockether.vis.core :as sdk]))

(defn- search-fn [query] ...)

(def find-symbol
  (sdk/symbol 'find search-fn
    {:doc      "Full-text search."
     :arglists '([query])
     :examples ["(search/find \"neural\")"]}))

(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.acme.ext.search
     :ext/doc       "Document search"
     :ext/kind      "knowledge"
     :ext/ns-alias  {:ns 'vis.ext.search :alias 'search}
     :ext/prompt    "Prefer search before manual file scans."
     :ext/symbols   [find-symbol]}))
```

The LLM sees in the system prompt:

```
[namespace: search -> vis.ext.search]
Document search (use search/ prefix)
- (search/find query) - Full-text search.
Prefer search before manual file scans.
```

And calls `(search/find "neural")` from `:code` blocks. Bare
`(find "neural")` does not resolve.

## One example per surface

Real in-tree examples - every package below ships exactly one
`META-INF/vis-extension/vis.edn` and exactly one
`(sdk/register-extension! ...)` call.

### Sandbox tools

`extensions/common/vis-foundation/.../core.clj` - read / list / search (`v/rg`) / patch:

```clojure
(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.foundation.editing.core
     :ext/doc       "Common Vis operations: cat, ls, rg, patch."
     :ext/version   "0.4.0"
     :ext/kind      "filesystem"
     :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
     :ext/symbols   all-symbols
     :ext/prompt    combined-prompt}))
```

### CLI commands

`src/com/blockether/vis/internal/main.clj` ships the `vis extensions list`
subcommand as a first-party extension contribution:

```clojure
(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.core
     :ext/doc       "vis runtime's contribution to the `vis extensions` subtree."
     :ext/cli       [{:cmd/name   "list"
                      :cmd/doc    "List every registered extension."
                      :cmd/usage  "vis extensions list"
                      :cmd/run-fn cli-extensions!}]}))
```

`:ext/cli` ALWAYS mounts under `["extensions"]`. The dispatcher
defaults `:cmd/parent` for entries that omit it. Three forms work:

1. **Flat** (most common): just `:cmd/name` + `:cmd/run-fn` -> `vis extensions <name>`.
2. **Embedded subcommands**: the entry carries its own `:cmd/subcommands` vector -> the whole tree mounts at `vis extensions <name>` and below.
3. **Deeper nest via parent path**: each entry sets `:cmd/parent ["extensions" "git"]` etc. -> `vis extensions git <name>`.

Any `:cmd/parent` that doesn't start with `"extensions"` is rejected
at registration time with `:type :ext/cli-bad-parent`. Top-level
binary commands (`vis run`, `vis providers`, ...) are NOT extension
commands; they call `registry/register-cmd!` directly inside the
host runtime (see `src/com/blockether/vis/internal/main.clj`).

See [Extension Spec - CLI command slot](spec.md#cli-command-slot) for
full examples of each form.

### Channel front-end

`extensions/channels/vis-channel-tui/.../channel_tui/screen.clj`:

```clojure
(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-tui.screen
     :ext/doc       "Lanterna-based terminal UI channel."
     :ext/version   "0.3.0"
     :ext/channels  [{:channel/id        :tui
                      :channel/cmd       "tui"
                      :channel/doc       "Interactive terminal UI."
                      :channel/usage     "vis channels tui [--conversation-id ID | --resume]"
                      :channel/owns-tty? true
                      :channel/main-fn   #'channel-main}]}))
```

### LLM provider

`extensions/providers/vis-provider-github-copilot/.../provider_github_copilot.clj`:

```clojure
(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.provider-github-copilot
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

`extensions/persistance/vis-persistance-sqlite/.../persistance_sqlite/core.clj`:

```clojure
(sdk/register-extension!
  (sdk/extension
    {:ext/namespace   'com.blockether.vis.ext.persistance-sqlite.core
     :ext/doc         "SQLite + Flyway persistence backend."
     :ext/version     "0.3.0"
     :ext/persistance [{:persistance/id :sqlite
                        :persistance/ns 'com.blockether.vis.ext.persistance-sqlite.core}]}))
```

### Multiple surfaces at once

Nothing prevents an extension from filling many slots at once -
an extension that ships SCI tools, a CLI command, AND a channel:

```clojure
(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.acme.ext.git
     :ext/doc       "Git integration: SCI tools + CLI + a web-hook channel."
     :ext/kind      "vcs"
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
- [Symbol Decorators (Symbol-Level Hooks)](hooks.md) - `:before-fn`, `:after-fn`, `:on-error-fn` (decorators around the target fn) + `:on-parse-error-fn` (parse rescue)
- [Environment Map](environment.md) - every key in the environment
- [Lifecycle Hooks (Nudges + Hard Guard Hooks)](guards.md) - built-in + extension lifecycle policy
