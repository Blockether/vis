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

## Quick start

```bash
vis-agent tui --jvm                                 # interactive terminal UI
vis-agent gateway start --host 0.0.0.0 --pair --jvm # gateway for the phone app; prints the pairing QR
```

`--jvm` runs the JVM source runtime — the checkout Vis owns — instead of an installed native binary (`VIS_JVM=1` is the same switch). A gateway bound to a non-loopback `--host` always requires a bearer token, and `--pair` prints the QR that carries it — see [Gateway & pairing](resources/vis-docs/gateway.md).

## Companion app (iPhone / Android)

The phone client for the gateway above: install it, scan the QR, and it drives the same sessions as the TUI. Both stores are in open public testing — no invite, no tester list.

- iOS / iPadOS — <https://testflight.apple.com/join/4anYT4Wk>
- Android — <https://play.google.com/apps/testing/com.blockether.viscompanion>

Questions and beta feedback: `contact@blockether.com`.

## License

Apache License 2.0 — see [LICENSE](LICENSE).

The models the voice extension can install are third parties' work and keep their own terms — every one of them is
credited in [THIRD_PARTY_MODELS.md](THIRD_PARTY_MODELS.md), which is generated from the manifest the installer reads.
