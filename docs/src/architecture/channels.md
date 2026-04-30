# Channels

A **channel** is a vis adapter — a user-facing front-end that turns
external input (terminal keys, HTTP requests, Telegram messages) into
calls to the runtime API in `com.blockether.vis.core`.

Channels are one **slot** on an extension. The word "extension" is
the umbrella term for everything extensible in vis: SCI sandbox
symbols, CLI commands, channels, LLM providers, persistance entries.
All five surfaces share one classpath-discovery resource
(`META-INF/vis-extension/vis.edn`), one loader
(`com.blockether.vis.core/discover-extensions!`), and one
author-facing entry point
(`com.blockether.vis.core/register-extension!`). Each surface is a
slot key (`:ext/symbols`, `:ext/cli`, `:ext/channels`,
`:ext/providers`, `:ext/persistance`); the registrar dispatches each
populated slot to its matching internal sub-registry. See
[Packages — Auto-discovery](packages.md#auto-discovery)
for the full slot → sub-registry table.

The host runtime ships zero channel implementations. The CLI
dispatcher calls `discover-extensions!` once at boot, which scans
every `META-INF/vis-extension/vis.edn` on the classpath and
`require`s the namespaces inside. Every `(sdk/register-extension! …)`
call with a populated `:ext/channels` slot lands the channel
descriptor in the channel registry as a side effect; the
`vis channels` sub-command tree exposes each one. Each channel's
`:channel/cmd` becomes the leaf name (`vis channels tui`,
`vis channels telegram`, …). The dispatcher never references a
concrete channel namespace — the host runtime stays usable when an
optional channel jar is absent.

## Channel descriptor

A channel is a plain map. The canonical way to register one is to
list it inside an extension's `:ext/channels` slot:

```clojure
(ns com.blockether.vis.ext.channel-tui.screen
  (:require [com.blockether.vis.core :as sdk]))

(defn channel-main [args] …)

(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-tui.screen
     :ext/doc       "Lanterna-based terminal UI channel."
     :ext/channels
     [{:channel/id        :tui                                      ;; required, keyword identity
       :channel/cmd       "tui"                                     ;; required, sub-command name under `vis channels`
       :channel/doc       "Interactive terminal UI."                ;; required, one-line summary
       :channel/usage     "vis channels tui [--conversation-id ID|--resume]" ;; optional, shown in help
       :channel/owns-tty? true                                      ;; optional, see below
       :channel/main-fn   #'channel-main}]}))                       ;; required, IFn
```

The lower-level `(sdk/register-channel! …)` call still works for
embedded / programmatic registration, and is what
`register-extension!` ultimately invokes for each `:ext/channels`
entry. New code should prefer the slot form because it lets a single
jar register everything an extension contributes in one validated
call.

`:channel/main-fn` receives the residual CLI tokens after the
`vis channels <cmd>` prefix as a single vector.

`:channel/owns-tty?` tells the launcher this channel takes over the
terminal (TUI, REPL-style channels). When set, the dispatcher
redirects stderr to `~/.vis/vis.log` **before** further class loading
so JVM warnings stay in the log and the alt-screen stays clean.

There is no `:channel/default?` flag and no implicit "default channel"
fallback. The dispatcher is a pure command tree: when the user runs
`vis "do the thing"` (first token isn't a known command) the
dispatcher prints the help tree and exits non-zero. Free-form prompts
must go through `vis run "…"` explicitly.

## Top-level command tree

The dispatcher mounts every channel under `vis channels`. The full
top-level command set built in `cli.clj :: root-command`:

| Command                  | Purpose                                                                  |
| ------------------------ | ------------------------------------------------------------------------ |
| `vis run "prompt"`         | One-shot agent query (defined in `internal/main.clj`).                            |
| `vis auth <provider>`      | Provider auth (e.g. GitHub Copilot OAuth, Z.ai static-API-key).                   |
| `vis conversations […]`    | List conversations, optionally filtered by channel (`tui` / `telegram` / `cli`).  |
| `vis doctor`               | Environment diagnostics.                                                          |
| `vis extensions list`      | List registered extensions.                                                       |
| `vis extensions <cmd> […]` | Run an extension-provided CLI command (`:ext/cli` entries).                       |
| `vis channels <name> […]`  | Run a registered channel (e.g. `vis channels tui`, `vis channels telegram`).      |

`vis` with no args prints the rendered help tree. Unknown commands
print the same tree and exit non-zero.

## Channel packages

The full per-package matrix lives on the [Packages](packages.md) page;
this section only highlights which packages register a channel.

| Package         | `:channel/id` | Notes                                                  |
| --------------- | ------------- | ------------------------------------------------------ |
| `vis-channel-tui`       | `:tui`        | Lanterna TUI. `:channel/owns-tty? true`.              |
| `vis-channel-telegram`  | `:telegram`   | Long-poll bot.                                         |

> **The `:cli` channel id is not a registered channel.** The CLI agent
> calls `(sdk/create! :cli …)` so its conversations show up under the
> `:cli` namespace, but there is no `register-channel!` call for it —
> the `vis` dispatcher itself is the CLI surface. The TUI, by contrast,
> both registers `:tui` as its dispatch channel AND writes its
> conversations under `:tui`: one keyword end-to-end.

## Adding a channel from a third-party jar

1. Create your `*-main` function: `(fn [args-vec] …)`.
2. At namespace load time, call `(sdk/register-extension! (sdk/extension {… :ext/channels [{…}]}))`. (Direct `(sdk/register-channel! …)` is still supported for embedded use; the slot form is the canonical author-facing API.)
3. Ship `META-INF/vis-extension/vis.edn` (the unified extension manifest) listing your namespace under `:nses`.

The next `clojure -M:vis` (or `bin/vis` launch) picks up the new
channel automatically and exposes it as `vis channels <your-cmd>` —
no edits to `cli.clj`.

## Why this matters

Before this split the dispatcher carried a hard-coded `cond` with
`(= cmd "telegram")` / `(= cmd "chat")` branches and direct
`:require` of `tui.screen` and `telegram.bot`. That meant:

- you couldn't ship the host runtime without dragging Lanterna and
  the Telegram HTTP client onto every consumer's classpath;
- a new channel required a PR against the host runtime;
- packaging a stripped-down distribution (e.g. only one channel)
  meant editing the dispatcher.

The registry pattern fixes all three. The dispatcher is now
data-driven; adding a channel is a load-time side effect of pulling
its jar onto the classpath.
