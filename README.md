<p align="center">
  <img src="logo.png" alt="vis logo" width="240"/>
</p>

# Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects/changes the host project through tools.

## Install

One command installs **`vis-agent`** — the only Vis command there is. (We deliberately do not install `vis`: Linux already ships an unrelated `vis` utility.)

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-vis-agent | bash
vis-agent help
```

That installs the `vis-agent` command into `~/.local/bin` (adding it to your PATH when needed). `vis-agent` then downloads its own private `vis-agent-native` runtime beside itself — the native image is never a separate command you install. From then on vis-agent owns both:

```bash
vis-agent runtime show      # what is installed and selected
vis-agent update            # update the command and its runtime together
vis-agent update --native   # (re)download just the native runtime
```

Native runtimes are published for Linux x64 and arm64.

### Variants

| Goal | Command |
|---|---|
| Install for all users | `curl -fsSL …/bin/install-vis-agent -o /tmp/install-vis-agent`<br>`sudo bash /tmp/install-vis-agent --install-dir /usr/local/bin` |
| Pin a release | `curl -fsSL …/bin/install-vis-agent \| bash -s -- --version vX.Y.Z` |
| Run on the JVM source runtime (needs Java 25+, the Clojure CLI, and git) | `curl -fsSL …/bin/install-vis-agent \| bash -s -- --runtime jvm` |

Corporate proxies often block `raw.githubusercontent.com`. Clone from `github.com` and run `bin/install-vis-agent` out of the checkout: it then installs that checkout's wrapper without touching the raw host.


### Hosts to allowlist

| Host | Needed for |
|---|---|
| `github.com` | git clone and release bytes |
| `api.github.com` | release resolution for install/update |
| `release-assets.githubusercontent.com` | release bundle bytes |
| `repo1.maven.org`, `repo.clojars.org` | JVM/source dependency resolution |
| `raw.githubusercontent.com` | one-line installers only |
| your model provider's API | running the agent |

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

## Build / develop

```bash
clojure -T:build native          # builds the private native runtime
clojure -M:format check
clojure -M:lint src extensions test build.clj
clojure -M:test
```
