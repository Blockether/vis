<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="logo.png">
    <img src="logo.png" alt="vis logo" width="240"/>
  </picture>
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
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
vis-agent help
```

```bash
vis-agent runtime           # what runs, and where it lives
vis-agent update            # move the command and its runtime to the newest commit
```

**Clojure library:**

```clojure
;; deps.edn
{:deps {com.blockether/vis {:mvn/version "0.1.41"}}}
```

## Companion app (iPhone / Android)

Vis Companion is the phone client for a Vis gateway you run yourself — it drives the same sessions as the TUI (see [Gateway & pairing](resources/vis-docs/gateway.md)). Both stores are in public testing; no invite, no tester list, just the link.

| Platform | Public test link | Status |
|---|---|---|
| iOS / iPadOS | **https://testflight.apple.com/join/4anYT4Wk** | TestFlight public link, open to anyone with the URL (requires the free TestFlight app) |
| Android | **https://play.google.com/apps/testing/com.blockether.viscompanion** | Play open testing (`beta` track), package `com.blockether.viscompanion` |

The app requires a gateway. Start one with `vis-agent gateway`, then pair by scanning the QR code it displays. For application questions and feedback, email `contact@blockether.com` (or use the TestFlight feedback button).

## The runtime

There is nothing to choose: how Vis was installed decides what runs, and everything it installs lives under `~/.vis`. Where the command file sits decides nothing either — a copy of the wrapper inside a checkout still runs the runtime Vis owns, never the tree around it.

| Runtime | Runs | Comes from |
|---|---|---|
| `native` | the private `vis-agent-native` sidecar beside the command | a release bundle or the container image |
| `jvm` | `clojure -M:vis` from the source Vis owns, pinned to main's newest commit | `install-vis-agent` |

```bash
vis-agent runtime           # what runs, where it lives, what the source is pinned to
vis-agent update            # move the command and its runtime to the newest
```

`vis-agent update` takes the installed runtime forward and carries the command with it; it never swaps one runtime for the other. A native build wins whenever one is installed. Full detail: [Runtime distributions](resources/vis-docs/distributions.md).

## License

Apache License 2.0 — see [LICENSE](LICENSE).

The models the voice extension can install are third parties' work and keep their own terms — every one of them is
credited in [THIRD_PARTY_MODELS.md](THIRD_PARTY_MODELS.md), which is generated from the manifest the installer reads.
