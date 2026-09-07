# Runtime distributions

Vis ships two executables:

- `vis-agent` — the engine, gateway, tools and embedded Python. It is a small
  wrapper on `PATH` that runs the runtime installed under `~/.vis`.
- `vis-tui` — an optional terminal client that talks to a gateway over HTTP.

## Installing

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

The installer puts `vis-agent` in `~/.local/bin` (`--install-dir PATH` to
change it, added to your shell profile if needed) and runs `vis-agent update`
to fetch the runtime. It needs `git` and `curl`. GraalVM CE and the Clojure CLI
are installed automatically when the runtime needs them; a matching GraalVM
already on the machine, including one from SDKMAN, is reused.
`VIS_NO_AUTO_INSTALL=1` disables automatic tool installation.

The script is published as a GitHub release asset because
`raw.githubusercontent.com` is blocked on many corporate networks. Running
`bin/install-vis-agent` from a clone works too.

## What runs

There are two runtimes. Whichever is installed runs; there is no selector.

| Runtime | Runs |
|---|---|
| `native` | a prebuilt native binary beside the command, from a release bundle or a local `--rebuild` |
| `jvm` | the source checkout Vis owns, pinned to the newest commit of `main` |

Currently releases ship no native bundle, so the installer sets up the `jvm`
runtime. A native binary, when present, always wins.

```bash
vis-agent runtime
```

```text
Runtime:      native
Native:       ~/.vis/install/vis-agent-native
Built:        0.1.40 49ccf1b155ec stable 2026-08-17T09:12:44Z
Track:        stable
Source:       ~/.vis/install/src
Pinned at:    49ccf1b155ec8fe18db7f48f00a30d1ac21be90d
```

A wrapper copied into a repository clone still runs the runtime under `~/.vis`.
To run a clone's own code, use `clojure -M:vis` inside it.

## Updating

| Command | Effect |
|---|---|
| `vis-agent update` | Newest release bundle (native) or newest commit of `main` (jvm) |
| `vis-agent update vX.Y.Z` | That release instead of the newest |
| `vis-agent update --rebuild` | Build the native binary locally from the installed source |
| `vis-agent update --track stable\|beta` | Follow that track from now on |
| `vis-agent update --keep-gateway` | Keep a running idle gateway instead of stopping it so the next session uses the new runtime |

An update never switches between the native and jvm runtimes, and never
changes track on its own. The `vis-agent` command is updated together with
the runtime. `beta` currently publishes nothing new.

## Files

| Path | Holds |
|---|---|
| `~/.vis/install/vis-agent-native` | the native runtime |
| `~/.vis/install/src` | the pinned source checkout: one detached commit, no branches |
| `~/.vis/install/ref` | the commit it is pinned at; `vis-agent runtime` reports `DRIFTED` when `HEAD` differs |

Deleting `~/.vis/install` is a full reset. `VIS_HOME` moves the whole `~/.vis`
directory. `vis-agent --measure` prints startup timings and `--jfr` records a
Java Flight Recorder profile into `VIS_HOME`.

## Native bundles

A release bundle is per platform:

```text
vis-agent-<os>-<arch>.tar.gz
├── vis-agent             # wrapper
├── vis-agent-native      # native runtime
├── vis-agent-python/     # embedded CPython
└── install-vis-agent

vis-tui-<os>-<arch>.tar.gz
└── vis-tui
```

Targets are Linux x86-64, Linux ARM64 and macOS ARM64. Building an image needs
the GraalVM CE version in `.graalvm-version` and about 32 GB of RAM;
`bin/release-native` builds and smoke-tests every asset a host can produce. On
Apple silicon it builds macOS natively, Linux ARM64 in a container and Linux
x86-64 through Rosetta (enable it in Docker Desktop or podman; qemu is refused
as too slow). `VIS_CONTAINER_CONNECTION` and `VIS_CONTAINER_CLI` pick the
container machine and engine.

On a platform with no published bundle, use the jvm runtime or
`vis-agent update --rebuild`.

## See also

- [Building the native binary](jvm-native-image.md) — how the native binary is built.
- [Remote access and the Companion app](gateway.md) — the gateway a runtime starts.
- [Configuration](configuration.md) — what a runtime reads at startup.
