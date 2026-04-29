# Getting Started

Vis is a single binary (`vis`) with a tree of sub-commands. Every
front-end — TUI, CLI agent, Telegram bot — is reached through the
same dispatcher.

This page covers the **user-facing** invocations. For the architecture
behind them, jump to [Architecture Overview](architecture/overview.md);
for the package layout, [Packages](architecture/packages.md).

## Install and run from source

```bash
git clone https://github.com/Blockether/vis.git
cd vis

# One-time: drop the mdbook-mermaid JS into docs/ (only needed for docs build)
mdbook-mermaid install docs

# Run any sub-command via the :vis alias
clojure -M:vis            # prints the full help tree
clojure -M:vis help       # same
```

`bin/vis` is a checked-in wrapper that just `exec`s `clojure -M:vis`
from the repo root. Add `bin/` to your `PATH` and every example below
works with `vis` instead of `clojure -M:vis`.

## First-time auth

Configure at least one LLM provider before the first turn. Vis
ships with GitHub Copilot OAuth out of the box:

```bash
vis auth github-copilot           # interactive device-flow auth
vis auth github-copilot --status  # show whether tokens are present + valid
vis auth github-copilot --logout  # forget tokens
```

Provider config + credentials live under `~/.vis/`:

```
~/.vis/
├── config.edn           single active provider, model, etc.
├── vis.mdb/vis.db       SQLite database (every conversation, every iteration)
└── vis.log              stderr log (when a TTY-owning channel is active)
```

Run `vis doctor` any time to sanity-check the environment (config
present, DB writable, providers reachable).

## The four ways to talk to the agent

### One-shot CLI agent

Best for scripts, CI, and quick one-liners. `vis run` creates a fresh
conversation in the `:cli` channel namespace, runs a single turn,
prints the answer, exits.

```bash
vis run "What is 2+2?"
vis run --model gpt-4o "Explain the auth flow"
vis run --json "Summarize foo.clj"            # machine-readable output
vis run --edn  "Summarize foo.clj"            # EDN output
vis run --trace "Refactor utils.clj"          # include the iteration trace in output
vis run --debug "..."                         # verbose logging on stderr
vis run --name reviewer "Review src/auth.clj" # name the agent (shows in conversations list)
vis run --db /tmp/scratch.db "..."            # use a non-default DB path
```

Past CLI runs are browsable later:

```bash
vis conversations cli
```

### Interactive TUI chat

`vis channels tui` opens the Lanterna terminal UI. Multi-turn, with
a sidebar of past conversations, the `[?]` system-prompt inspector,
and copy/paste dialogs.

```bash
vis channels tui                                  # fresh conversation
vis channels tui --resume                         # resume the most recent :tui conversation
vis channels tui --conversation-id <UUID>         # resume a specific one (full or short ID)
```

Key bindings live in the dialogs (`Ctrl+K` opens the action menu).
**Never bind `Ctrl+Y`** — the kernel intercepts it and suspends
the whole process; this is documented in `AGENTS.md`.

### Telegram bot

`vis channels telegram` runs a long-poll loop that wires Telegram
chats into the conversation runtime. Each chat-id maps 1:1 to a
conversation in the `:telegram` channel namespace.

```bash
export TELEGRAM_BOT_TOKEN=...
vis channels telegram
```

Past Telegram conversations:

```bash
vis conversations telegram
```

### Programmatic embedding

For embedding Vis in your own Clojure program, require
`com.blockether.vis.core`:

```clojure
(require '[com.blockether.vis.core :as vis]
         '[com.blockether.vis.core :as cfg]
         '[com.blockether.svar.internal.llm :as llm])

(def env (vis/create-environment (cfg/make-router) {:db :memory}))

(vis/query! env [(llm/user "What is 2+2?")])
;; => {:answer "4" :iterations 1 :duration-ms 312 :tokens {...} :cost {...}}

(vis/dispose-environment! env)
```

Full API reference: [Public API](architecture/packages.md#package-map).

## Browsing past conversations

Every conversation — from any channel — lives in a single SQLite DB
at `~/.vis/vis.mdb/vis.db`. Inspect it through the CLI:

```bash
vis conversations             # default: :tui
vis conversations tui         # explicit
vis conversations telegram    # Telegram chats
vis conversations cli         # CLI one-shots
```

The output table includes the conversation ID; pass it to
`vis channels tui --conversation-id <ID>` to resume in the TUI.

## Extensions

Extensions add tools to the SCI sandbox. Drop an extension jar on the
classpath — it self-registers via the unified `META-INF/vis-extension/vis.edn`
at startup.

```bash
vis extensions list                  # list everything that registered
vis extensions <cmd> [args…]         # run an extension's exported CLI command
```

The bundled `extensions/common/vis-common-editing` package adds `vis/cat`,
`vis/ls`, `vis/rg`, `vis/edit`, and `vis/sh` (filesystem + shell tools
namespaced under the `vis/` alias). It is already wired into the root
`deps.edn`; add the same `:local/root` entry to a downstream consumer's
`deps.edn` to enable it:

```clojure
;; deps.edn
{:deps {com.blockether/vis-common-editing
        {:local/root "extensions/common/vis-common-editing"}}}
```

To author your own extension, see [Extension System](extensions/overview.md).

## All top-level sub-commands

| Command                    | Purpose                                                                  |
| -------------------------- | ------------------------------------------------------------------------ |
| `vis run "prompt"`         | One-shot agent turn (CLI agent).                                         |
| `vis channels tui […]`     | Lanterna TUI chat.                                                       |
| `vis channels telegram`    | Telegram long-poll bot.                                                  |
| `vis auth <provider>`      | Provider OAuth flow.                                                     |
| `vis conversations [ch]`   | List conversations, optionally filtered by channel.                      |
| `vis doctor`               | Environment diagnostics.                                                 |
| `vis extensions list`      | List registered extensions.                                              |
| `vis extensions <cmd> […]` | Run an extension-provided CLI command.                                   |
| `vis channels <name> […]`  | Run any registered channel by `:channel/cmd` name.                       |
| `vis help`                 | Print the help tree (same as no args).                                   |

The dispatcher is a pure command tree. There is no magical fallback
to `vis run` for free-form prompts — unrecognized commands print
the help tree and exit non-zero. Use `vis run "…"` explicitly.

## Where to next

- [Introduction](README.md) — why code-eval over tool-calls
- [Architecture Overview](architecture/overview.md) — how the layers fit together
- [Packages](architecture/packages.md) — every `vis-*` jar, dep direction, auto-discovery
- [Extension System](extensions/overview.md) — add tools, prompts, nudges
- [Channels](architecture/channels.md) — build your own front-end
