# Runtime distributions

Vis provides two executables:

- `vis-agent` starts the engine, gateway, tools and embedded Python.
- `vis-tui` is the terminal client, installed alongside it and connected over HTTP.

## Installing

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

The installer puts the stable native engine, bundled Python and matching TUI in
`~/.local/bin` (`--install-dir PATH` changes it). It adds that directory to your
shell profile if needed. Installation requires `curl` and `tar`, not Java or Git.
An incomplete or unavailable release fails without installing a JVM fallback.

Source development is an explicit opt-in:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --jvm
vis-agent --jvm update
```

Source development needs Git and JDK 25+. The wrapper can install the Clojure CLI;
`VIS_NO_AUTO_INSTALL=1` disables that automatic installation.

The script is published as a GitHub release asset because
`raw.githubusercontent.com` is blocked on many corporate networks. Running
`bin/install-vis-agent` from a clone works too.

## Runtime selection

Vis supports native and JVM runtimes:

| Runtime | Runs |
|---|---|
| `native` | a prebuilt binary from a release bundle or local build |
| `jvm` | the managed source checkout under `~/.vis/install/src` |

The wrapper prefers an installed native binary. Use `--jvm` or `VIS_JVM=1` to
select the JVM runtime explicitly. An existing source checkout remains usable.

```bash
vis-agent runtime
```

```text
Runtime:      native
Native:       ~/.local/bin/vis-agent-native
Built:        0.1.40 49ccf1b155ec stable 2026-08-17T09:12:44Z
Track:        stable
Source:       not installed
Pinned at:    unpinned
```

A development checkout is used when no managed runtime is installed. To run its
code independently of installed runtimes, use `clojure -M:vis` inside it.

## Updating

| Command | Effect |
|---|---|
| `vis-agent update` | Newest release bundle (native) or newest commit of `main` (jvm) |
| `vis-agent update vX.Y.Z` | That release instead of the newest |
| `vis-agent --jvm update --rebuild` | Update managed source and build a native binary from it |
| `vis-agent update --track stable\|beta` | Explicitly select a published native distribution, including from a source install |
| `vis-agent update --keep-gateway` | Leave an idle managed gateway running after the update |

Native updates acquire the engine, bundled Python and TUI from the same release
before replacing installed files. `--jvm` cannot be combined with `--track`.
A missing native TUI fails explicitly rather than launching a JVM client.

`--rebuild` updates the managed source checkout before building. It does not
build edits in your working repository. To build those edits, run
`clojure -T:build native` from that repository.

## Files

| Path | Contents |
|---|---|
| `~/.local/bin/vis-agent-native` | the installed native engine |
| `~/.local/bin/vis-agent-python/` | its bundled Python runtime |
| `~/.local/bin/vis-tui` | the matching native terminal client |
| `~/.vis/install/vis-agent-native` | release engine when the launcher is a tracked checkout file |
| `~/.vis/install/src` | the pinned source checkout: one detached commit, no branches |
| `~/.vis/install/ref` | the commit it is pinned at; `vis-agent runtime` reports `DRIFTED` when `HEAD` differs |

`VIS_HOME` changes the base directory from `~/.vis`. `vis-agent --measure`
prints startup timings; `--jfr` saves a Java Flight Recorder profile there.

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
`bin/release-native` builds and smoke-tests the assets supported by the host.
On Apple silicon it builds macOS natively, Linux ARM64 in a container and Linux
x86-64 through Rosetta. Enable Rosetta in Docker Desktop or podman; the build
rejects qemu. `VIS_CONTAINER_CONNECTION` and `VIS_CONTAINER_CLI` select the
container machine and engine.

On a platform with no published bundle, use the jvm runtime or
`vis-agent --jvm update --rebuild`.

## See also

- [Building the native binary](jvm-native-image.md) — how the native binary is built.
- [Remote access and the Companion app](gateway.md) — the gateway a runtime starts.
- [Configuration](configuration.md) — what a runtime reads at startup.
