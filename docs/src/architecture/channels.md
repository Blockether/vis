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
(`com.blockether.vis.core/register-global!`). Each surface is a
slot key (`:ext/symbols`, `:ext/cli`, `:ext/channels`,
`:ext/providers`, `:ext/persistance`); the registrar dispatches each
populated slot to its matching internal sub-registry. See
[Packages — Auto-discovery](packages.md#auto-discovery)
for the full slot → sub-registry table.

`vis-runtime` ships zero channel implementations. The CLI dispatcher
calls `discover-extensions!` once at boot, which scans every
`META-INF/vis-extension/vis.edn` on the classpath and `require`s the namespaces
inside. Every `(ext/register-global! …)` call with a populated
`:ext/channels` slot lands the channel descriptor in the channel
registry as a side effect; the `vis channels` sub-command tree
exposes each one. Each channel's `:channel/cmd` becomes the leaf name
(`vis channels tui`, `vis channels telegram`, …). The dispatcher
never references a concrete channel namespace — `vis-runtime` stays
usable when an optional channel jar is absent.

## Channel descriptor

A channel is a plain map. The canonical way to register one is to
list it inside an extension's `:ext/channels` slot:

```clojure
(ns com.blockether.vis.ext.channel-tui.screen
  (:require [com.blockether.vis.core :as ext]))

(defn channel-main [args] …)

(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.channel-tui.screen
     :ext/doc       "Lanterna-based terminal UI channel."
     :ext/channels
     [{:channel/id        :tui                                      ;; required, keyword identity
       :channel/cmd       "tui"                                     ;; required, sub-command name under `vis channels`
       :channel/doc       "Interactive terminal UI."                ;; required, one-line summary
       :channel/usage     "vis tui [--conversation-id ID|--resume]" ;; optional, shown in help
       :channel/owns-tty? true                                      ;; optional, see below
       :channel/main-fn   #'channel-main}]}))                       ;; required, IFn
```

The lower-level `(channel/register-global! …)` call (from
`com.blockether.vis.core`) still works for embedded / programmatic
registration, and is what `ext/register-global!` ultimately calls
under the hood. New code should prefer the slot form because it lets
a single jar register everything an extension contributes in one
validated call.

`:channel/main-fn` receives the residual CLI tokens after the
`vis channels <cmd>` prefix as a single vector.

`:channel/owns-tty?` tells the launcher this channel takes over the
terminal (TUI, REPL-style channels). When set, the dispatcher
redirects stderr to `~/.vis/vis.log` **before** further class loading
so JVM warnings don't bleed into the alt-screen.

There is no `:channel/default?` flag and no implicit "default channel"
fallback. When the user runs `vis "do the thing"` (first token isn't
a known command), the dispatcher routes to `cli-run!` — the same code
path as `vis run "do the thing"` — not to a registered channel.

## Top-level command tree

The dispatcher mounts every channel under `vis channels`. The full
top-level command set built in `cli.clj :: root-command`:

| Command                  | Purpose                                                                  |
| ------------------------ | ------------------------------------------------------------------------ |
| `vis run "prompt"`       | One-shot agent query (see `channels.cli.agent`).                         |
| `vis auth <provider>`    | Provider auth (e.g. GitHub Copilot OAuth).                               |
| `vis conversations […]`  | List conversations, optionally filtered by channel (`vis|telegram|cli`). |
| `vis doctor`             | Environment diagnostics.                                                 |
| `vis extensions`         | List registered extensions.                                              |
| `vis ext <cmd> […]`      | Run an extension-provided CLI command (`:ext/cli` entries).              |
| `vis channels <name> […]` | Run a registered channel (e.g. `vis channels tui`, `vis channels telegram`).|

`vis "free-form prompt"` (no leading sub-command) falls back to
`cli-run!`. `vis` with no args prints the rendered help tree.

## Channel packages

The full per-package matrix lives on the [Packages](packages.md) page;
this section only highlights which packages register a channel.

| Package         | `:channel/id` | Notes                                                  |
| --------------- | ------------- | ------------------------------------------------------ |
| `vis-channel-tui`       | `:tui`        | Lanterna TUI. `:channel/owns-tty? true`.              |
| `vis-channel-telegram`  | `:telegram`   | Long-poll bot.                                         |

> **The `:cli` channel id is not a registered channel.** The CLI agent
> calls `(conversations/create! :cli …)` so its conversations show up
> under the `:cli` namespace, but there is no `channel/register-global!`
> for it — the `vis` dispatcher itself is the CLI surface. The TUI, by
> contrast, both registers `:tui` as its dispatch channel AND writes its
> conversations under `:tui`: one keyword end-to-end.

## Adding a channel from a third-party jar

1. Create your `*-main` function: `(fn [args-vec] …)`.
2. At namespace load time, call `(ext/register-global! (ext/extension {… :ext/channels [{…}]}))`. (Direct `(channel/register-global! …)` is still supported for embedded use; the slot form is the canonical author-facing API.)
3. Ship `META-INF/vis-extension/vis.edn` (the unified extension manifest) listing your namespace.

The next `clojure -M:vis` (or `bin/vis` launch) picks up the new
channel automatically and exposes it as `vis channels <your-cmd>` —
no edits to `cli.clj`.

## Why this matters

Before this split `cli.clj` carried a hard-coded `cond` with
`(= cmd "telegram")` / `(= cmd "chat")` branches and direct
`:require` of `tui.screen` and `telegram.bot`. That meant:

- you couldn't ship `vis-runtime` without dragging Lanterna and the
  Telegram HTTP client onto every consumer's classpath;
- a new channel required a PR against `vis-runtime`;
- packaging a stripped-down distribution (e.g. only one channel) meant
  editing the dispatcher.

The registry pattern fixes all three. The dispatcher is now data-
driven; adding a channel is a load-time side effect of pulling its
jar onto the classpath.
