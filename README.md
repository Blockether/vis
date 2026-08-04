<p align="center">
  <img src="logo.png" alt="vis logo" width="240"/>
</p>

<div align="center">
  <h2>
    <a href="https://clojars.org/com.blockether/vis"><img src="https://img.shields.io/clojars/v/com.blockether/vis?color=%23007ec6&label=clojars" alt="Clojars version"></a>
    <a href="https://github.com/Blockether/vis/blob/main/LICENSE">
      <img src="https://img.shields.io/badge/license-Apache%202.0-green" alt="License - Apache 2.0">
    </a>
  </h2>
</div>

# Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects/changes the host project through tools.

## Install

One command installs **`vis-agent`** — the only Vis command there is.

```bash
curl -fsSL https://github.com/Blockether/vis/releases/latest/download/install-vis-agent | bash
vis-agent help
```

`install-vis-agent` is a release asset: every `vX.Y.Z` release publishes the installer and the `vis-agent` wrapper alongside the platform bundles, so that URL always serves the installer of the newest release.

That installs the `vis-agent` command into `~/.local/bin` (adding it to your PATH when needed). `vis-agent` then downloads its own private `vis-agent-native` runtime beside itself — the native image is never a separate command you install. From then on vis-agent owns both:

```bash
vis-agent runtime show      # what is installed and selected
vis-agent update            # update the command and its runtime together
vis-agent update --native   # (re)download just the native runtime
```

Native runtimes are published for Linux x64 and arm64.

**Clojure library:**

```clojure
;; deps.edn
{:deps {com.blockether/vis {:mvn/version "0.1.27"}}}
```

`com.blockether/vis` already depends on every bundled extension, so that single coordinate gives the full agent. Depend on one package (`com.blockether/vis-channel-tui`, `com.blockether/vis-provider-anthropic`, `com.blockether/vis-language-python`, …) only when you embed a part of it.

## Companion app (iPhone / Android)

Vis Companion is the phone client for a Vis gateway you run yourself — it drives the same sessions as the TUI (see [Gateway & pairing](resources/vis-docs/gateway.md)). Both stores are in public testing; no invite, no tester list, just the link.

| Platform | Public test link | Status |
|---|---|---|
| iOS / iPadOS | **https://testflight.apple.com/join/4anYT4Wk** | TestFlight public link, open to anyone with the URL (requires the free TestFlight app) |
| Android | **https://play.google.com/apps/testing/com.blockether.viscompanion** | Play open testing (`beta` track), package `com.blockether.viscompanion` |

The app is useless on its own: it needs a gateway. Start one with `vis-agent gateway`, then pair by scanning the QR it prints. Feedback goes to `karol@blockether.com` (or the TestFlight feedback button).

## Choose the runtime

`vis-agent` follows the releases by default: the published native runtime, or
JVM source pinned to the newest `vX.Y.Z` tag. A live checkout is **dev mode** —
opt in; it is never picked for you, not even from inside a Vis checkout.

| Runtime | Runs |
|---|---|
| `native` | the private sidecar downloaded by `vis-agent update` |
| `jvm` | source pinned to the newest `vX.Y.Z` tag |
| `dev` | a live checkout (`~/vis`, or `$VIS_DEV_CHECKOUT`), tracking its branch |
| `auto` | no choice at all: native if installed, else tagged source |

Dev mode is the one runtime Vis fetches a checkout for: with nothing at that
path, `vis-agent update` clones the repository there on `main`.

```bash
vis-agent runtime show
vis-agent runtime use native|jvm|dev|auto   # persisted default
vis-agent --native|--jvm|--dev help         # one launch only
VIS_RUNTIME=dev vis-agent help              # one process only
```

A one-launch flag beats `VIS_RUNTIME`, which beats the persisted default in
`~/.vis/runtime`. `vis-agent update` updates whichever runtime is in effect —
the newest release bundle, or the checkout Vis owns moved onto the newest tag.
Only `update --dev` follows a moving branch, and any target that is not a
`vX.Y.Z` release pins the owned source to that git ref. A selected runtime that
is not installed is an error with the command that fixes it, never a silent
substitution. There is no jar runtime: `target/vis.jar` is a build artifact.
Full flag, update, state, and environment matrix:
[Runtime distributions](resources/vis-docs/distributions.md).

## License

Apache License 2.0 — see [LICENSE](LICENSE).
