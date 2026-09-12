<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="logo.png">
    <img src="logo.png" alt="vis logo" width="240"/>
  </picture>
</p>

<div align="center">
  <h2>
    <a href="https://clojars.org/com.blockether/vis"><img src="https://img.shields.io/clojars/v/com.blockether/vis?color=%23007ec6&label=clojars" alt="Clojars version"></a>
    <a href="https://pypi.org/project/vis-agent/"><img src="https://img.shields.io/pypi/v/vis-agent?color=%23007ec6&label=PyPI%20SDK" alt="Vis Python SDK version on PyPI"></a>
    <a href="https://github.com/Blockether/vis/blob/main/LICENSE">
      <img src="https://img.shields.io/badge/license-Apache%202.0-green" alt="License - Apache 2.0">
    </a>
    <a href="https://vis.blockether.com/"><img src="https://img.shields.io/badge/Documentation-teal" alt="Documentation"></a>
    <a href="https://vis.blockether.com/extensions/"><img src="https://img.shields.io/badge/Extension_Center-dimgray" alt="Extension Center"></a>
  </h2>
</div>

# Vis

Vis is a coding agent that combines tools into Python programs.
It can search your project, make changes and run tests, checking the results
as it goes.

## Why Vis

You know how your project should be built, tested and checked. Vis lets you
put that knowledge into functions the agent can use, so repeatable work
doesn't depend only on written instructions.

- **Put your expertise into code.** Give the agent small, tested
  [Python extensions](resources/vis-docs/extending.md), such as a function that
  runs the right test suite and reports failures. Keep `AGENTS.md` and skills
  for guidance; enforce an operation's rules in code.
- **Combine steps in Python.** Models already use Python to get things done.
  Vis gives the agent one tool, `python_execution`, where it can discover and
  combine functions, inspect intermediate results and print a useful summary.
- **Keep useful work when you return.** The agent can reuse Python helpers in
  the same session, even if you restart Vis or reload extensions. It can
  inspect workspace facts, permissions and context usage, and summarize
  completed work without deleting the stored history.

Once your functions cover a workflow, you can disable shell access. Extensions
run as trusted CPython code; the model's Python environment is sandboxed.
These functions make individual operations more predictable, not the model's
decisions.

Read more in [Getting started](resources/vis-docs/index.md#why-vis), or use the
[Python SDK](https://pypi.org/project/vis-agent/) to run sessions from your own code.

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

## Native vs JVM

**Choose native for everyday use.** Choose JVM if you're developing Vis itself
or want to run the latest code from `main`.

| What matters | Native | JVM |
| --- | --- | --- |
| Best for | Daily work and multiple sessions | Developing Vis and trying the latest changes |
| Version track | `release` (default) or `beta` | `dev` |
| Java / Git | Not needed | Git and JDK 25+ |
| Gateway startup | ~0.7 s | ~8.6 s |
| Gateway RAM | ~122 MiB | ~566 MiB |
| RAM per session (light use) | ~60 MiB | ~153 MiB |

*Performance figures: Vis v0.2.0 on Apple M4 Max.*

## Quick start

```bash
vis-agent tui                                      # interactive terminal UI
vis-agent desktop                                  # download once and open the desktop app
vis-agent gateway start --host 10.0.0.5 --pair       # phone app gateway; prints a pairing QR
```

`vis-agent tui` opens the terminal UI and starts a local gateway if needed.
`vis-agent desktop` downloads the stable desktop app on first use and opens it;
later launches reuse the download. Use `vis-agent desktop --update` to check for
a newer desktop release. Pair the desktop with your gateway in the app.
Every plain `vis-agent update` selects release; name beta or dev when updating those
tracks. See [Runtime distributions](resources/vis-docs/distributions.md).
A non-loopback `--host` requires a bearer token. `--pair` prints a QR code containing
the address and token. See [Remote access and the Companion app](resources/vis-docs/gateway.md).

## Companion app (iPhone / Android)

Install the app and scan the pairing QR code to access the same sessions as the terminal UI. Both stores offer public testing without an invitation.

<p>
<a class="store-apple" href="https://testflight.apple.com/join/4anYT4Wk"><img src="resources/vis-docs/assets/install-testflight.png" alt="TestFlight for iOS and iPadOS" width="224" height="56"></a>
<span aria-hidden="true">&nbsp;&nbsp;</span>
<a class="store-android" href="https://play.google.com/apps/testing/com.blockether.viscompanion"><img src="resources/vis-docs/assets/install-google-play.png" alt="Google Play beta for Android" width="224" height="56"></a>
</p>

Questions and beta feedback: `contact@blockether.com`.

## License

Apache License 2.0 — see [LICENSE](LICENSE).

The speech service can install third-party models with separate license terms.
[THIRD_PARTY_MODELS.md](THIRD_PARTY_MODELS.md) lists their authors and licenses; it is generated from the installer's model manifest.
