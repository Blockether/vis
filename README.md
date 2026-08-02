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
| Run on the JVM source runtime (needs Java 25+ and the Clojure CLI) | `curl -fsSL …/bin/install-source \| bash` |

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

`vis-agent` is always the stable public wrapper, and by default it follows the
releases: the published native runtime, or JVM source pinned to the newest
release tag. A live, moving checkout is **dev mode** — you have to ask for it.

```bash
vis-agent runtime show
vis-agent runtime use native   # release runtime downloaded by `vis-agent update`
vis-agent runtime use jvm      # source pinned to the newest release tag
vis-agent runtime use dev      # live checkout (~/vis, or $VIS_DEV_CHECKOUT)
vis-agent runtime use auto     # back to automatic: native, else tagged source

vis-agent --native help        # one launch only
vis-agent --jvm help           # one launch only; --source is an alias
vis-agent --dev help           # one launch only; VIS_RUNTIME=dev does the same
```

With no persisted choice the wrapper uses the installed native runtime, else
release-tagged JVM source. `vis-agent update` follows the same rule: it installs
the newest release bundle, or moves the managed checkout onto the newest `vX.Y.Z`
tag. Only `vis-agent update --dev` (or dev mode) fast-forwards a live checkout's
branch. A selected but unavailable runtime fails with an actionable error rather
than silently changing runtimes.

There is deliberately no `--jar` distribution. `target/vis.jar` is only an intermediate/build artifact and is never shipped or selected.

## Build / develop

```bash
vis-agent native                 # builds the private target/vis runtime
clojure -M:format check
clojure -M:lint src extensions test build.clj
clojure -M:test
```
