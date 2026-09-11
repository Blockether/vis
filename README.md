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

Vis is a coding agent that combines tools into Python programs. It can chain operations, run them in parallel, and inspect results before deciding what belongs in the conversation.

## Why Vis

Models already use Python to orchestrate tasks. Vis makes Python the agent's
primary interface: one model-facing tool, `python_execution`, with host functions
discovered through `apropos()` and inspected through `doc()`. The model composes
operations as code instead of coordinating a growing catalog of separate tools.

That program can inspect workspace facts, permissions and context usage through
`session`, fold settled history, and reuse persistent Python helpers. Helper
definitions survive returning to the same session, gateway restarts and extension
reloads. The engine is also accessible through the [Python SDK](https://pypi.org/project/vis-agent/).

You know how your system should work. Encode that knowledge in small, tested,
composable [Python extensions](resources/vis-docs/extending.md). Make routine
operations deterministic and inspectable, and improve the agent's daily workflow
over time. Extensions run as trusted code with full CPython; the model-facing
execution environment is sandboxed.

For repeatable workflows, we encourage replacing broad shell access with focused
functions, then disabling `shell`. Keep `AGENTS.md` and skills lean: use them for
guidance, not as a substitute for checks your functions can enforce.
Read more in [Getting started](resources/vis-docs/index.md#why-vis).

## Install

Install the `vis-agent` command:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
vis-agent help
```

Tracks choose which version of Vis you install:

- `release` (default) installs the latest stable version and does not need Java.
- `beta` installs the latest published preview that passed automated checks and does not need Java.
- `dev` runs the latest code from `main` on the JVM and needs Git and JDK 25+.

```bash
vis-agent update
vis-agent update --track beta
vis-agent update --track dev
```

**Clojure library:**

```clojure
;; deps.edn
{:deps {com.blockether/vis {:mvn/version "0.1.44"}}}
```

## Quick start

```bash
vis-agent tui                                      # interactive terminal UI
vis-agent gateway start --host 10.0.0.5 --pair       # phone app gateway; prints a pairing QR
```

`vis-agent tui` opens the terminal UI and starts a local gateway if needed.
Every plain `vis-agent update` selects release; name beta or dev when updating those
tracks. See [Runtime distributions](resources/vis-docs/distributions.md).
A non-loopback `--host` requires a bearer token. `--pair` prints a QR code containing
the address and token. See [Remote access and the Companion app](resources/vis-docs/gateway.md).

## Companion app (iPhone / Android)

Install the app and scan the pairing QR code to access the same sessions as the terminal UI. Both stores offer public testing without an invitation.

- iOS / iPadOS — <https://testflight.apple.com/join/4anYT4Wk>
- Android — <https://play.google.com/apps/testing/com.blockether.viscompanion>

Questions and beta feedback: `contact@blockether.com`.

## License

Apache License 2.0 — see [LICENSE](LICENSE).

The speech service can install third-party models with separate license terms.
[THIRD_PARTY_MODELS.md](THIRD_PARTY_MODELS.md) lists their authors and licenses; it is generated from the installer's model manifest.
