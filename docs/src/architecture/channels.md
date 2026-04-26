# Channels

A **channel** is a vis adapter — a user-facing front-end that turns
external input (terminal keys, HTTP requests, Telegram messages) into
calls to the runtime API in `com.blockether.vis.loop.runtime.conversation.core`.

Channels are **plug-ins**, not features hard-coded in `vis-core`. They
follow the same lifecycle as extensions:

| Concern              | Extensions (`com.blockether.vis.extension`) | Channels (`com.blockether.vis.channel`) |
| -------------------- | ------------------------------------------- | --------------------------------------- |
| What they add        | tools/values to the SCI sandbox             | sub-commands under `vis channel`        |
| Registration         | `register-global!` at ns load               | `register-global!` at ns load           |
| Auto-discovery       | `META-INF/vis/extensions.edn`               | `META-INF/vis/channels.edn`             |
| Spec / validation    | `clojure.spec` + `extension`                | `clojure.spec` + `channel`              |
| Consumed by          | `register-extensions!` per env              | `cli/-main` dispatcher                  |

`vis-core` ships zero channel implementations. The CLI dispatcher
calls `channel/discover-channels!` once at boot and exposes every
registered channel under the `vis channel` sub-command tree. Each
channel's `:channel/cmd` becomes the leaf name (`vis channel tui`,
`vis channel telegram`, …). The dispatcher never references a
concrete channel namespace — `vis-core` stays usable when an
optional channel jar is absent.

## Channel descriptor

```clojure
{:channel/id        :tui                                      ;; required, keyword identity
 :channel/cmd       "tui"                                     ;; required, sub-command name under `vis channel`
 :channel/doc       "Interactive terminal UI."                ;; required, one-line summary
 :channel/usage     "vis tui [--conversation-id ID|--resume]" ;; optional, shown in help
 :channel/owns-tty? true                                      ;; optional, see below
 :channel/main-fn   #'tui.screen/channel-main}                ;; required, IFn
```

`:channel/main-fn` receives the residual CLI tokens after the
`vis channel <cmd>` prefix as a single vector.

`:channel/owns-tty?` tells the launcher this channel takes over the
terminal (TUI, REPL-style channels). When set, the dispatcher
redirects stderr to `~/.vis/vis.log` **before** further class loading
so JVM warnings don't bleed into the alt-screen.

There is no `:channel/default?` flag and no implicit "default channel"
fallback. When the user runs `vis "do the thing"` (first token isn't
a known command), the dispatcher routes to `cli-run!` — the same code
path as `vis run "do the thing"` — not to a registered channel.

## Top-level command tree

The dispatcher mounts every channel under `vis channel`. The full
top-level command set built in `cli.clj :: root-command`:

| Command                  | Purpose                                                                  |
| ------------------------ | ------------------------------------------------------------------------ |
| `vis run "prompt"`       | One-shot agent query (see `channels.cli.agent`).                         |
| `vis auth <provider>`    | Provider auth (e.g. GitHub Copilot OAuth).                               |
| `vis conversations […]`  | List conversations, optionally filtered by channel (`vis|telegram|cli`). |
| `vis doctor`             | Environment diagnostics.                                                 |
| `vis extensions`         | List registered extensions.                                              |
| `vis ext <cmd> […]`      | Run an extension-provided CLI command (`:ext/cli` entries).              |
| `vis channel <name> […]` | Run a registered channel (e.g. `vis channel tui`, `vis channel telegram`).|

`vis "free-form prompt"` (no leading sub-command) falls back to
`cli-run!`. `vis` with no args prints the rendered help tree.

## Packages

The repo is split per channel so adopters can drop the ones they
don't need. `vis-core` stays usable without any of them:

| Package                 | Channel id   | Notes                                                                  |
| ----------------------- | ------------ | ---------------------------------------------------------------------- |
| `vis-core`              | `:cli`       | CLI dispatcher + one-shot `vis run` (`channels.cli.agent`).             |
| `vis-extension`         | —           | Plug-in spec: `extension` (SCI tools) + `channel` (CLI front-ends).    |
| `vis-commandline`       | —           | Reusable command tree / arg-parsing primitives used by `cli.clj`.      |
| `vis-persistance`       | —           | Persistence facade + spec. JDBC stays out of `vis-core`.               |
| `vis-persistance-sqlite`| —           | SQLite + Flyway backend. Auto-registers via `META-INF/vis/persistance-backends.edn`. |
| `vis-logging`           | —           | Telemere → SQLite log handler (opt-in).                                |
| `vis-provider`          | —           | Vendor auth (currently GitHub Copilot OAuth).                          |
| `vis-tui`               | `:tui`       | Lanterna TUI. `:channel/owns-tty? true`.                              |
| `vis-telegram`          | `:telegram`  | Long-poll bot.                                                         |

> **Note on the `:cli` channel id.** The CLI agent calls
> `(conversations/create! :cli …)` so its conversations show up under
> the `:cli` channel namespace. There is no separate "CLI channel"
> registered through `channel/register-global!` — the dispatcher itself
> *is* the CLI surface.

Adding a new channel from a third-party jar is identical to writing
an extension:

1. Create your `*-main` function: `(fn [args-vec] …)`.
2. Call `(channel/register-global! {…})` at namespace load.
3. Ship `META-INF/vis/channels.edn` listing your namespace.

The next `clojure -M:run` (or `vis` binary launch) picks up the new
channel automatically and exposes it as `vis channel <your-cmd>` —
no edits to `cli.clj`.

## Why this matters

Before this split `cli.clj` carried a hard-coded `cond` with
`(= cmd "telegram")` / `(= cmd "chat")` branches and direct
`:require` of `tui.screen` and `telegram.bot`. That meant:

- you couldn't ship `vis-core` without dragging Lanterna and the
  Telegram HTTP client onto every consumer's classpath;
- a new channel required a PR against `vis-core`;
- packaging a stripped-down distribution (e.g. only one channel) meant
  editing the dispatcher.

The registry pattern fixes all three. The dispatcher is now data-
driven; adding a channel is a load-time side effect of pulling its
jar onto the classpath.
