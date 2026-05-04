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

## Development nREPL path

For Clojure-style live development, use the `:dev` alias. It starts an
nREPL, writes `.nrepl-port` for discovery, and keeps the JVM alive while
you drive code through `clj-nrepl-eval`.

```bash
bin/dev                         # same as clojure -M:dev; nREPL + wait
bin/dev tui                     # open Terminal.app with nREPL + TUI in one JVM
bin/dev terminal-tui            # internal target run inside Terminal.app
bin/dev cli providers list      # nREPL + run a CLI entry point, then wait
NREPL_PORT=7890 bin/dev         # override default port 7888
```

From another terminal or an LLM shell tool:

```bash
clj-nrepl-eval --discover-ports
clj-nrepl-eval -p 7888 "(+ 1 2 3"   # delimiters are auto-repaired
clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload) (dev/cli! \"providers\" \"list\")"
clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload) (dev/tui! \"--resume\")"
```

`dev/tui!` opens a new macOS Terminal.app window running
`bin/dev terminal-tui`. That Terminal process starts its own nREPL and
then runs `vis channels tui` in the same JVM, so the TUI process remains
REPL-controllable. Terminal.app cannot attach an already-running JVM as
its controlling terminal; the JVM that owns the TUI must be launched by
Terminal.app.

If a Clojure edit leaves delimiters unbalanced, do not hand-repair the
paren pile; run the repair tool on the file:

```bash
clj-paren-repair src/com/blockether/vis/core.clj
```

## First-time auth

Configure at least one LLM provider before the first turn. Vis
ships with OAuth providers out of the box:

```bash
vis providers auth github-copilot    # GitHub device-flow auth
vis providers auth openai-codex      # ChatGPT/Codex browser OAuth
vis providers status openai-codex    # show auth state + limits
vis providers logout openai-codex    # forget tokens
```

You can also start the same OpenAI Codex browser OAuth flow from the
TUI provider picker; the CLI command remains the fallback when the
localhost callback cannot complete inside the terminal session.

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

Best for scripts, CI, and quick one-liners. `vis run` is ephemeral by
default: it uses an in-memory store, runs a single turn, prints the
answer, exits, and leaves no `:cli` conversation on disk. Pass
`--persist` when you want the run saved.

```bash
vis run "What is 2+2?"
vis run --model gpt-4o "Explain the auth flow"
vis run --json "Summarize foo.clj"            # machine-readable output
vis run --edn  "Summarize foo.clj"            # EDN output
vis run --trace "Refactor utils.clj"          # include the iteration trace in output
vis run --debug "..."                         # verbose logging on stderr
vis run --name reviewer "Review src/auth.clj" # name the agent
vis run --persist "Keep this conversation"    # save under the :cli channel
```

Persisted CLI runs are browsable later:

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
Typing `@` opens the file picker for inline file references; inside the
picker, `Alt+I` toggles ignored files, `Alt+S` cycles sort mode, and
`Alt+O` opens the selected file through the OS opener.
On the main chat screen, `Ctrl+R` cycles reasoning effort, `Ctrl+L`
cycles OpenAI Codex verbosity, and `Ctrl+T` cycles the primary provider's
configured models; changes apply to the next request and are persisted
with the rest of the TUI settings. These shortcuts are shown in the footer
next to the model/reasoning/verbosity values they change.
The provider picker can also launch the OpenAI Codex browser OAuth
flow directly from the TUI.
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
         '[com.blockether.svar.internal.llm :as llm])

(def env (vis/create-environment (vis/get-router) {:db :memory}))

(vis/turn! env [(llm/user "What is 2+2?")])
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

The bundled `extensions/common/vis-foundation` package adds `v/cat`,
`v/ls`, `v/rg`, `v/bash`, and thin babashka.fs wrappers like
`v/read-all-lines`, `v/write-lines`, `v/update-file`, and
`v/glob` (filesystem tools namespaced under the `v/` alias). It is already
wired into the root `deps.edn`; add the same `:local/root` entry to a
 downstream consumer's `deps.edn` to enable it:

```clojure
;; deps.edn
{:deps {com.blockether/vis-foundation
        {:local/root "extensions/common/vis-foundation"}}}
```

To author your own extension, see [Extension System](extensions/overview.md).

## All top-level sub-commands

| Command                    | Purpose                                                                  |
| -------------------------- | ------------------------------------------------------------------------ |
| `vis run "prompt"`         | One-shot agent turn (CLI agent).                                         |
| `vis channels tui […]`     | Lanterna TUI chat.                                                       |
| `vis channels telegram`    | Telegram long-poll bot.                                                  |
| `vis providers …`          | Provider auth / status / limits / logout commands.                       |
| `vis conversations [ch]`   | List conversations, optionally filtered by channel.                      |
| `vis doctor`               | Environment diagnostics.                                                 |
| `vis report <conv-id>`     | Forensic Markdown report for a persisted conversation.                   |
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
