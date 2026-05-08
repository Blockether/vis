# Getting Started

Vis is a single binary (`vis`) with a tree of sub-commands. Every
front-end — TUI, CLI agent, Telegram bot — is reached through the
same dispatcher.

This page covers the **user-facing** invocations. For the architecture
behind them, jump to [Architecture Overview](architecture/overview.md);
for the package layout, [Packages](architecture/packages.md).

## Install and run from source

The bootstrap scripts check for Java 21+, the official Clojure CLI
(`clojure -Sdescribe`), and git before cloning anything. If Clojure is
missing, install it from the [official Clojure install guide](https://clojure.org/guides/install_clojure)
and rerun the script.

`~/.vis` is the Vis app home, not necessarily your development checkout:

```text
~/.vis/
├── sourcecode/           default installed source checkout
├── config.edn            local provider/model config
├── vis.mdb/vis.db        SQLite conversations + iteration state
└── vis.log               stderr log for TTY-owning channels
```

You can keep source somewhere else. Runtime state still lives under
`~/.vis/` unless you explicitly configure otherwise.

### Method 1: default install

Use this when you just want a working global `vis` command.

macOS, Linux, and WSL:

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source | bash
~/.local/bin/vis help
```

Native Windows PowerShell:

```powershell
irm https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source.ps1 | iex
~\.local\bin\vis.cmd help
```

Both installers clone or update Vis under `~/.vis/sourcecode`; Unix-like
systems symlink `~/.local/bin/vis`, and native Windows writes
`~/.local/bin/vis.cmd`.

After installation, run `vis update` to fast-forward the source checkout used
by your installed `vis` command. It runs `git fetch --tags origin` and
`git pull --ff-only`; local uncommitted work or diverged branches are left for
you to resolve manually.

### Method 2: custom install directory

Use this when you want the source checkout under `~/code`, an external
volume, or any other normal repo location.

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source \
  | bash -s -- --dest "$HOME/code/vis"

~/.local/bin/vis help
```

Equivalent environment form:

```bash
VIS_SOURCE_DIR="$HOME/code/vis" \
  bash <(curl -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source)
```

Native Windows PowerShell:

```powershell
$env:VIS_SOURCE_DIR = "$HOME\code\vis"
irm https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source.ps1 | iex
~\.local\bin\vis.cmd help
```

### Method 3: install from your fork URL

Use this when you already have a GitHub fork and want the installer to
clone that fork instead of `Blockether/vis`.

HTTPS:

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source \
  | bash -s -- --repo https://github.com/YOUR_GITHUB_USER/vis.git --dest "$HOME/code/vis"
```

SSH:

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source \
  | bash -s -- --repo git@github.com:YOUR_GITHUB_USER/vis.git --dest "$HOME/code/vis"
```

Then add the upstream remote once:

```bash
cd "$HOME/code/vis"
git remote add upstream https://github.com/Blockether/vis.git
git fetch upstream
```

If you rerun the installer on a fork checkout, pass `--repo` again. The
installer intentionally sets `origin` to the repo URL you give it.

### Method 4: fork and clone with GitHub CLI through the installer

Use this for contribution work. The installer asks `gh` to create or
reuse your GitHub fork, clones that fork locally, sets `origin` to your
fork, sets `upstream` to `Blockether/vis`, and links the global `vis`
launcher to that checkout.

```bash
gh auth login

curl -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source \
  | bash -s -- --fork --dest "$HOME/code/vis"

cd "$HOME/code/vis"
git remote -v
vis help
vis extensions doctor
```

From an existing checkout, run the same mode directly:

```bash
bin/install-source --fork --dest "$HOME/code/vis"
```

Native Windows PowerShell:

```powershell
gh auth login
$install = irm https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source.ps1
& ([scriptblock]::Create($install)) -Fork -Dest "$HOME\code\vis"
~\.local\bin\vis.cmd doctor
```

Optional fork controls:

```bash
bin/install-source --fork \
  --fork-source Blockether/vis \
  --fork-owner YOUR_GITHUB_USER_OR_ORG \
  --fork-name vis \
  --dest "$HOME/code/vis"
```

Manual `gh` equivalent, if you do not want the installer to do it:

```bash
gh auth login
mkdir -p "$HOME/code"
cd "$HOME/code"
gh repo fork Blockether/vis --clone --remote
cd vis
mkdir -p "$HOME/.local/bin"
ln -sfn "$PWD/bin/vis" "$HOME/.local/bin/vis"
```

Push contribution branches to your fork:

```bash
git checkout -b my-change
# edit, test, commit
git push -u origin my-change
gh pr create --web
```

To update your fork from upstream:

```bash
git fetch upstream
git checkout main
git merge --ff-only upstream/main
git push origin main
```

Do not commit `~/.vis/config.edn`, `~/.vis/vis.mdb`, or `~/.vis/vis.log`.
Those are local runtime files, not repo files.

### Method 5: manual clone without installer

Use this when you want maximum control and do not need the bootstrap
script.

```bash
git clone https://github.com/Blockether/vis.git ~/.vis/sourcecode
cd ~/.vis/sourcecode

# One-time: drop the mdbook-mermaid JS into docs/ (only needed for docs build)
mdbook-mermaid install docs

# Run any sub-command via the :vis alias
clojure -M:vis            # prints the full help tree
clojure -M:vis help       # same

# Optional global launcher
mkdir -p "$HOME/.local/bin"
ln -sfn "$PWD/bin/vis" "$HOME/.local/bin/vis"
```

`bin/vis` is a checked-in wrapper that just `exec`s `clojure -M:vis`
from the repo root. The installer links it as `~/.local/bin/vis`; for
manual installs, create that symlink yourself. Make sure `~/.local/bin`
is on `PATH`, and every example below works with `vis` instead of
`clojure -M:vis`.

### Corporate / proxy install

Zscaler and similar corporate proxies can intercept HTTPS and re-sign
traffic with their own CA. That can make `curl` or `git` fail before Vis
is cloned.

Quick dirty path, only if you accept disabling verification:

```bash
curl -k -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source | bash
git config --global http.sslVerify false
```

Proper path:

```bash
export https_proxy="http://proxy.corp.local:8080"
export CURL_CA_BUNDLE=/path/to/corp-ca-bundle.pem
git config --global http.sslCAInfo /path/to/corp-ca-bundle.pem

curl -fsSL https://raw.githubusercontent.com/Blockether/vis/refs/heads/main/bin/install-source | bash
```

Ask IT for the proxy CA PEM file. Prefer the proper path when possible.

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

Provider config + credentials live under `~/.vis/` by default. A project may
also define `.vis/config.edn`; Vis merges global config first and then overlays
the project-local file, so project keys win when you run `vis` from that
project.

```
~/.vis/
├── config.edn           single active provider, model, etc.
├── vis.mdb/vis.db       SQLite database (every conversation, every iteration)
└── vis.log              stderr log (when a TTY-owning channel is active)
```

Run `vis extensions doctor` any time to sanity-check the environment (config
present, DB writable, providers reachable).

To remove the shell tool from the model sandbox for a project, add this to the
project-local `.vis/config.edn` (or to `~/.vis/config.edn` globally):

```clojure
{:tools {:bash {:enabled? false}}}
```

When disabled, `v/bash` is not registered and the prompt omits shell
examples. `v/bash` is the single shell tool and runs with strict mode (`set -euo pipefail`).

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
Typing `@` opens the file picker for inline file references. The visible
chat keeps `@path`, while the agent prompt gets an `[Attached File: path]`
read-now directive requiring `(v/cat path)`, a `def` binding, and
`v/preview` before answering about that file. Inside the picker, `Alt+I`
toggles ignored files, `Alt+S` cycles sort mode, and `Alt+O` opens the
selected file through the OS opener.
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

Telegram supports the same model/reasoning/verbosity controls as the TUI
through `/models`, `/reasoning`, and `/verbosity`. Voice messages are
transcribed by the unified `vis-voice` extension using the Parakeet ASR model.

To restrict the bot to approved chats, approve the chat id from a trusted
shell. Once the allow-list is non-empty, unlisted chats are rejected with the
same command printed back to them:

```bash
vis channels telegram approve --chat-id 123456789
vis channels telegram approve --chat-id 123456789 --restart
```

Restart an already-running bot as a fresh Java process after config changes:

```bash
vis channels telegram restart
# or from Telegram:
# /restart
```

The restart path starts a new `java -cp ... clojure.main -m com.blockether.vis.core channels telegram`
process by default. Set `VIS_TELEGRAM_RESTART_CMD` to override the exact shell
command. Bot pid/log files live under `~/.vis/telegram.pid` and
`~/.vis/telegram.log`.

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
at startup. Source extensions are also auto-loaded from these directories when
the Unix `bin/vis` launcher starts:

- `<project>/.vis/vis-extensions/<name>/deps.edn`
- `~/.vis/vis-extensions/<name>/deps.edn`

```bash
vis extensions list                  # list everything that registered
vis extensions scaffold my-tools     # create .vis/vis-extensions/my-tools
vis extensions <cmd> [args…]         # run an extension's exported CLI command
```

The bundled `extensions/common/vis-foundation` package adds `v/cat`,
`v/patch`, `v/ls`, `v/rg`, `v/bash`, and thin babashka.fs wrappers like
`v/glob` (filesystem tools namespaced under the `v/` alias). `v/rg` uses one
spec-map grammar: `(v/rg {:all ["defn" "name"] :paths ["src"]})` narrows to
lines containing every literal; `(v/rg {:any ["foo" "bar"] :paths ["."]})`
broadens to either literal. There is no public `v/grep`; bind `v/rg` once and
use `v/preview` to display selected hits. It is already wired into the root
`deps.edn`; add the same `:local/root` entry to a downstream consumer's
`deps.edn` to enable it:

```clojure
;; deps.edn
{:deps {com.blockether/vis-foundation
        {:local/root "extensions/common/vis-foundation"}}}
```

`vis extensions scaffold <name>` writes a minimal deps.edn, extension manifest,
and Clojure namespace. If you scaffold under the project-local `.vis` directory,
that project loads it automatically on the next `vis` start; scaffold under
`~/.vis/vis-extensions` for global user extensions.

To author your own extension, see [Extension System](extensions/overview.md).

## All top-level sub-commands

| Command                    | Purpose                                                                  |
| -------------------------- | ------------------------------------------------------------------------ |
| `vis run "prompt"`         | One-shot agent turn (CLI agent).                                         |
| `vis channels tui […]`     | Lanterna TUI chat.                                                       |
| `vis channels telegram`    | Telegram long-poll bot.                                                  |
| `vis providers …`          | Provider auth / status / limits / logout commands.                       |
| `vis conversations [ch]`   | List conversations, optionally filtered by channel.                      |
| `vis update`               | Fast-forward the installed source checkout.                              |
| `vis extensions doctor`    | Environment diagnostics.                                                 |
| `vis extensions reproduction <conv-id>` | Complete flag-free Markdown reproduction artifact for a persisted conversation. |
| `vis extensions list`      | List registered extensions.                                              |
| `vis extensions scaffold <name>` | Create a user extension project scaffold.                         |
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
