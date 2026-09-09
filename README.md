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
    <a href="https://vis.blockether.com/"><img src="https://img.shields.io/badge/Documentation-teal" alt="Documentation"></a>
    <a href="https://vis.blockether.com/extensions/"><img src="https://img.shields.io/badge/Extension_Center-dimgray" alt="Extension Center"></a>
  </h2>
</div>

# Vis

Vis is an AI coding agent that helps you understand, modify, and test your codebase. It runs Python in a sandboxed CPython runtime and stores session state outside the model's context window.

## Install

Install the `vis-agent` command:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
vis-agent help
```

```bash
vis-agent runtime           # show the active runtime and its location
vis-agent update            # update the command and runtime
```

**Clojure library:**

```clojure
;; deps.edn
{:deps {com.blockether/vis {:mvn/version "0.1.44"}}}
```

## Quick start

```bash
vis-agent tui --jvm                                 # interactive terminal UI
vis-agent gateway start --host 0.0.0.0 --pair --jvm # gateway for the phone app; prints the pairing QR
```

`vis-agent tui` opens the terminal UI and starts a local gateway if needed. `--jvm` uses Vis's managed source checkout instead of an installed native binary; `VIS_JVM=1` has the same effect. A non-loopback `--host` requires a bearer token. `--pair` prints a QR code containing the address and token. See [Remote access and the Companion app](resources/vis-docs/gateway.md).

## Companion app (iPhone / Android)

Install the app and scan the pairing QR code to access the same sessions as the terminal UI. Both stores offer public testing without an invitation.

- iOS / iPadOS — <https://testflight.apple.com/join/4anYT4Wk>
- Android — <https://play.google.com/apps/testing/com.blockether.viscompanion>

Questions and beta feedback: `contact@blockether.com`.

## License

Apache License 2.0 — see [LICENSE](LICENSE).

The speech service can install third-party models with separate license terms.
[THIRD_PARTY_MODELS.md](THIRD_PARTY_MODELS.md) lists their authors and licenses; it is generated from the installer's model manifest.
