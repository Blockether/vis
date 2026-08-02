<p align="center">
  <img src="logo.png" alt="vis logo" width="240"/>
</p>

# Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects/changes the host project through tools.

## Install

The installed command is **`vis-agent`**. We deliberately do not install `vis`: Linux already ships an unrelated `vis` utility, and taking that name would create a system-command collision.

Every distribution installs the same Bash wrapper. The native image is a private `vis-agent-native` sidecar; it is never presented as a standalone Vis command.

### Native runtime (recommended on supported Linux machines)

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-native | bash
vis-agent help
```

The release asset is a bundle named `vis-agent-<os>-<arch>-community.tar.gz` containing both the wrapper and its native runtime. Releases currently carry Linux x64 and arm64. The installer puts them in `~/.local/bin` and, when necessary, adds that directory to your shell profile. The command is therefore available from every directory in new shells—not only from the checkout.

For a system-wide installation shared by all users, download the installer first and run it with the required permission:

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-native -o /tmp/install-vis-agent
sudo bash /tmp/install-vis-agent --install-dir /usr/local/bin
```

### JVM source runtime

```bash
git clone https://github.com/Blockether/vis.git ~/.vis/sourcecode
~/.vis/sourcecode/bin/install-source
vis-agent help
```

`bin/install-source` checks Java 25+, the Clojure CLI, and git; updates an existing checkout safely; copies the same wrapper to `~/.local/bin/vis-agent`; records the checkout; and configures PATH when needed. Use `--dest PATH` or `--install-dir PATH` to override either location.

The one-line source equivalent is:

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

Corporate proxies often block `raw.githubusercontent.com`; cloning from `github.com` and running the checked-out installer avoids that host.

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

`vis-agent` is always the stable public wrapper. It supports exactly two runtimes:

```bash
vis-agent runtime show
vis-agent runtime use native   # persist the default
vis-agent runtime use jvm      # persist the source/JVM default

vis-agent --native help        # one launch only
vis-agent --jvm help           # one launch only; --source is an alias
```

With no persisted choice, the wrapper prefers live JVM source while invoked inside a Vis checkout (so edits win), otherwise native when installed, otherwise source. A selected but unavailable runtime fails with an actionable error rather than silently changing runtimes.

There is deliberately no `--jar` distribution. `target/vis.jar` is only an intermediate/build artifact and is never shipped or selected.

## Build / develop

```bash
vis-agent native                 # builds the private target/vis runtime
clojure -M:format check
clojure -M:lint src extensions test build.clj
clojure -M:test
```
