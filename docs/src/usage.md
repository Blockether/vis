# Getting Started

Vis is a single binary (`vis`) with a tree of sub-commands. Every
front-end — TUI, CLI agent, Telegram bot — is reached through the
same dispatcher.

This page covers the **user-facing** invocations. For the architecture
behind them, jump to [Architecture Overview](architecture/overview.md);
for the package layout, [Packages](architecture/packages.md).

## Install / run from source

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
\u251c\u2500\u2500 config.edn           single active provider, model, etc.
\u251c\u2500\u2500 vis.mdb/vis.db       SQLite database (every conversation, every iteration)
\u2514\u2500\u2500 vis.log              stderr log (when a TTY-owning channel is active)
```

Run `vis doctor` any time to sanity-check the environment (config
present, DB writable, providers reachable).

## The four ways to talk to the agent

### 1. CLI agent (one-shot) \u2014 `vis run`

Best for scripts, CI, and quick one-liners. Creates a fresh
conversation in the `:cli` channel namespace, runs a single query,
prints the answer, exits.

```bash
vis run "What is 2+2?"
vis run --model gpt-4o "Explain the auth flow"
vis run --max-iterations 10 "Refactor utils.clj"
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

### 2. TUI chat \u2014 `vis channels tui`

Interactive Lanterna terminal UI. Multi-turn, with a sidebar of past
conversations, the `[?]` system-prompt inspector, and copy/paste
dialogs.

```bash
vis channels tui                                  # fresh conversation
vis channels tui --resume                         # resume the most recent :vis conversation
vis channels tui --conversation-id <UUID>         # resume a specific one (full or short ID)
```

Key bindings live in the dialogs (`Ctrl+K` opens the action menu).
**Never bind `Ctrl+Y`** \u2014 the kernel intercepts it and suspends
the whole process; this is documented in `AGENTS.md`.

### 3. Telegram bot \u2014 `vis channels telegram`

Long-poll loop that wires Telegram chats into the conversation
runtime. Each chat-id maps 1:1 to a conversation in the `:telegram`
channel namespace.

```bash
export TELEGRAM_BOT_TOKEN=...
vis channels telegram
```

Past Telegram conversations:

```bash
vis conversations telegram
```

### 4. Programmatic \u2014 `com.blockether.vis.core`

For embedding Vis in your own Clojure program:

```clojure
(require '[com.blockether.vis.core :as vis]
         '[com.blockether.vis.config :as cfg]
         '[com.blockether.svar.internal.llm :as llm])

(def env (vis/create-environment (cfg/make-router) {:db :temp}))

(vis/query! env [(llm/user \"What is 2+2?\")])
;; => {:answer \"4\" :iterations 1 :duration-ms 312 :tokens {...} :cost {...}}

(vis/dispose-environment! env)
```

Full API reference: [Public API on `com.blockether.vis.core`](architecture/packages.md#package-map).

## Browsing past conversations

Every conversation \u2014 from any channel \u2014 lives in a single
SQLite DB at `~/.vis/vis.mdb/vis.db`. Inspect it through the CLI:

```bash
vis conversations             # default: :vis (TUI)
vis conversations vis         # explicit
vis conversations telegram    # Telegram chats
vis conversations cli         # CLI one-shots
```

The output table includes the conversation ID; pass it to
`vis channels tui --conversation-id <ID>` to resume in the TUI.

## Extensions

Extensions add tools to the SCI sandbox. Drop an extension jar on the
classpath \u2014 it self-registers via the unified `META-INF/vis.edn`
at startup.

```bash
vis extensions                       # list everything that registered
vis ext <cmd> [args\u2026]                # run an extension's exported CLI command
```

The bundled `extensions/vis-common-operations` package adds `read`,
`list`, `grep`, and `patch` (filesystem tools, namespaced under the
`fs/` alias). Add it to your run alias:

```clojure
;; deps.edn
:dev {:extra-deps {com.blockether/vis-common-operations {:local/root \"extensions/vis-common-operations\"}}}
```

To author your own extension, see [Extension System](extensions/overview.md).

## All top-level sub-commands

| Command                    | Purpose                                                           |
| -------------------------- | ----------------------------------------------------------------- |
| `vis run \"prompt\"`        | One-shot agent query (CLI agent).                                  |
| `vis channels tui [\u2026]`    | Lanterna TUI chat.                                                |
| `vis channels telegram`    | Telegram long-poll bot.                                           |
| `vis auth <provider>`      | Provider OAuth flow.                                              |
| `vis conversations [ch]`   | List conversations, optionally filtered by channel.               |
| `vis doctor`               | Environment diagnostics.                                          |
| `vis extensions`           | List registered extensions.                                       |
| `vis ext <cmd> [\u2026]`       | Run an extension-provided CLI command.                            |
| `vis channels <name> [\u2026]` | Run any registered channel by `:channel/cmd` name.                |
| `vis help`                 | Print the help tree (same as no args).                            |

`vis "free-form prompt"` (no leading sub-command) falls back to
`vis run "free-form prompt"` for ergonomic one-liners.

## Where to next

- [Introduction](README.md) \u2014 why code-eval over tool-calls
- [Architecture Overview](architecture/overview.md) \u2014 how the layers fit together
- [Packages](architecture/packages.md) \u2014 every `vis-*` jar, dep direction, auto-discovery
- [Extension System](extensions/overview.md) \u2014 add tools, prompts, nudges
- [Channels](architecture/channels.md) \u2014 build your own front-end
