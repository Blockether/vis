# Runtime distributions

Vis provides two executables:

- `vis-agent` starts the engine, gateway, tools and embedded Python. A wrapper
  on `PATH` selects the runtime installed under `~/.vis`.
- `vis-tui` is an optional terminal client that connects to a gateway over HTTP.

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

## Runtime selection

Vis supports native and JVM runtimes:

| Runtime | Runs |
|---|---|
| `native` | a prebuilt binary from a release bundle or local build |
| `jvm` | the managed source checkout under `~/.vis/install/src` |

The wrapper prefers an installed native binary. Use `--jvm` or `VIS_JVM=1` to
select the JVM runtime explicitly. If no native bundle is available, the
installer sets up the JVM runtime.

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
| `vis-agent update --rebuild` | In JVM mode, update the managed source and build a native binary from it |
| `vis-agent update --track stable\|beta` | Select a published native distribution track; unavailable in JVM mode |
| `vis-agent update --keep-gateway` | Leave an idle managed gateway running after the update |

Updates use the selected runtime and update the wrapper too. Native mode
rejects `--rebuild`; use the source runtime to build. Track selection applies
only to published native bundles.

`--rebuild` updates the managed source checkout before building. It does not
build edits in your working repository. To build those edits, run
`clojure -T:build native` from that repository.

## Files

| Path | Contents |
|---|---|
| `~/.vis/install/vis-agent-native` | the native runtime |
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
`vis-agent update --rebuild`.

## See also

- [Building the native binary](jvm-native-image.md) — how the native binary is built.
- [Remote access and the Companion app](gateway.md) — the gateway a runtime starts.
- [Configuration](configuration.md) — what a runtime reads at startup.
