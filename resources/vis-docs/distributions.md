# Runtime distributions

`vis-agent` starts the engine, gateway, workers and embedded Python. `vis-tui`
is the terminal client, installed alongside it and connected over HTTP.

## Installing

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

By default the installer acquires the latest complete stable native release. It
puts the engine, bundled Python worker and matching TUI in `~/.local/bin` and adds
that directory to the shell profile if needed. Use `--install-dir PATH` to change it.
Native installation requires `curl` and `tar`, not Java or Git. Missing or incomplete
native packages fail without installing a JVM fallback.

The installer accepts the same tracks as update:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --track beta
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --track dev
```

The script is published as a GitHub release asset because some networks block
`raw.githubusercontent.com`. Running `bin/install-vis-agent` from a clone also works.

## Updating and selecting a track

```bash
vis-agent update                          # release, always the default
vis-agent update --track release          # latest complete stable native release
vis-agent update --track beta             # latest complete native beta
vis-agent update --track dev              # newest main source, always JVM
```

| Track | Source | Execution |
|---|---|---|
| `release` | Latest complete stable GitHub release | Native engine, gateway, workers and TUI |
| `beta` | Published beta of a main commit that passed CI and native checks | Native engine, gateway, workers and TUI |
| `dev` | Newest commit of `main` | JVM engine, gateway, workers and TUI |

Every update without `--track` selects `release`, including after beta or dev.
A successful update records the selection for subsequent launches. To stay on beta
or dev when updating, name that track each time. A failed download does not change
the recorded selection. Dev ignores any native binaries left from a previous install.

`vis-agent update vX.Y.Z` installs a specific stable version on the release track.
Version pins are not accepted with beta or dev. Native updates acquire the engine,
Python worker and TUI from the same immutable release before replacing installed files.
A missing native TUI fails rather than starting a JVM client.

By default an update releases an idle managed gateway using its old executable
before replacing it. Busy or user-owned gateways are never stopped. Add
`--keep-gateway` to leave even an idle gateway running.

Dev needs Git and JDK 25+. The launcher can install the Clojure CLI;
`VIS_NO_AUTO_INSTALL=1` disables that installation. Dev does not build native images.
To run local repository edits independently of installed tracks, use
`clojure -M:vis` inside that checkout. Build those edits with `clojure -T:build native`.

## Automatic native betas

The Beta Native workflow starts after successful push CI on `main`. It verifies
that CI passed for the exact commit in this repository and selects the latest
successful main run. A newer successful run cancels an older beta build. New
commits whose CI is pending or failed do not block publication of a green beta.

Beta uses the same native build and test workflow as stable releases on Linux
x86-64, Linux ARM64 and macOS ARM64. Each build has an immutable `beta-<commit>` tag.
Its prerelease remains a draft until all native tests, SDK checks and TUI checks
pass and all six engine/TUI archives are present and nonempty. Failed or cancelled
builds cannot replace the last published beta. Beta never becomes GitHub's latest
stable release, and does not publish mobile or desktop applications.

## Files

| Path | Contents |
|---|---|
| `~/.local/bin/vis-agent-native` | Installed native engine |
| `~/.local/bin/vis-agent-python/` | Its bundled Python worker and interpreter |
| `~/.local/bin/vis-tui` | Matching native terminal client |
| `~/.vis/install/track` | Selection used for subsequent launches |
| `~/.vis/install/src` | Managed dev checkout at a detached main commit |
| `~/.vis/install/ref` | Commit pinned by the last dev update |
| `~/.vis/install/vis-agent-native` | Native engine for a checkout-owned launcher |

Each native engine has a `vis-agent-native.build` file beside it, containing its
version, commit, build track and timestamp. `vis-agent --version` reports the version.
`VIS_HOME` changes the state directory from `~/.vis`. `vis-agent --measure` prints
startup timings; `--jfr` saves Java Flight Recorder profiles.

## Native bundles

```text
vis-agent-<os>-<arch>.tar.gz
├── vis-agent
├── vis-agent-native
├── vis-agent-native.build
├── vis-agent-python/
└── install-vis-agent

vis-tui-<os>-<arch>.tar.gz
└── vis-tui
```

Building an image needs the GraalVM CE version in `.graalvm-version` and about
32 GB of RAM. `bin/release-native` builds and smoke-tests the host's supported
assets. On Apple silicon it builds macOS natively, Linux ARM64 in a container and
Linux x86-64 through Rosetta. Enable Rosetta in Docker Desktop or podman; the build
rejects qemu. `VIS_CONTAINER_CONNECTION` and `VIS_CONTAINER_CLI` select the
container machine and engine. Use the dev track on platforms without native bundles.

## See also

- [Building the native binary](jvm-native-image.md)
- [Remote access and the Companion app](gateway.md)
- [Configuration](configuration.md)
