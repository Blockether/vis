# Runtime distributions

Vis ships two native executables with separate responsibilities:

- `vis-agent` owns the engine, gateway, tools and embedded CPython runtime.
- `vis-tui` is an optional terminal client that talks to a running gateway over HTTP/SSE.

`vis-agent` remains one Bash command on PATH for running and updating the engine.
Its native image is a private sidecar called `vis-agent-native`; it is never
installed as `vis` (Linux already has an unrelated `vis`) or invoked directly.
`target/vis.jar` is a build artifact, not a runtime. The TUI is deliberately not
inside that image: a terminal client does not need the engine classpath, CPython,
providers or speech runtime.

## Two runtimes, and no selector

| Runtime | Runs | Installed by |
|---|---|---|
| `native` | the private `vis-agent-native` sidecar beside the command | a release bundle, the container image, or a local `--rebuild` |
| `jvm` | `clojure -M:vis` from the checkout Vis owns, pinned to main's newest commit | `bin/install-vis-agent` |

Installing IS the decision. The native sidecar wins whenever one is installed,
because it is the published runtime; otherwise Vis runs the source it owns.
There is no flag, no environment variable and no persisted choice on top of
that — `vis-agent update` moves the installed runtime forward, and never swaps
one runtime for the other.

Where the command file sits decides nothing either. A copy of the wrapper
inside a repository checkout — this repository's own `bin/vis-agent` included —
is still only the command: it runs the runtime Vis owns under `~/.vis`, and
neither that checkout's source nor a `target/vis` built in it is a runtime.
Run a checkout's own code with `clojure -M:vis` from inside it.

## What runs

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

It reports; it never switches. When nothing is installed at all, a launch is an
error naming the command that fixes it — `vis-agent update` — and the wrapper
never silently substitutes another runtime.

## Updating

`vis-agent update` takes the installed runtime forward.

| Command | Updates |
|---|---|
| `vis-agent update` | a native install: the newest release bundle; a source install: main's newest commit |
| `vis-agent update vX.Y.Z` | that release instead of the newest: bundle for `native`, tag for `jvm` |
| `vis-agent update --rebuild` | after a source update, builds the sidecar locally (`clojure -T:build native`) |
| `vis-agent update --track NAME` | follows the `stable` or `beta` track from now on; the choice sticks |
| `vis-agent update --keep-gateway` | skips the post-install `vis-agent gateway stop --if-idle`, which otherwise releases an unused gateway so the next session starts on the runtime just installed |

### Tracks

A build carries the track it was made for in its stamp, and `vis-agent runtime`
prints it beside the commit that produced the binary.

| Track | Who builds it | Followable |
|---|---|---|
| `stable` | a release tag; the assets on the newest release | yes, and the default |
| `beta` | Linux only, and PAUSED: the 6-hourly schedule is off while every native platform is broken, so betas ship only on a manual `beta-native` dispatch | yes, but nothing new lands |
| `dev` | a build of your own: `clojure -T:build native`, or `vis-agent update --rebuild` | no |
| `dry-run` | a CI build that published nothing | no |

`dev` and `dry-run` are stamps, not destinations: nothing publishes them, so
`--track dev` is refused, and a runtime carrying one moves forward by being
rebuilt rather than updated. Switching track is explicit and it sticks — an
update never changes track on its own, or a beta tester silently falls back to
stable and files bugs against a build that was never running. Naming a version
is a one-off and leaves the remembered track where it was.

A track is not a UI surface: `vis-tui` is a separately released gateway client.

A target that is not a released version is refused: Vis installs what is
published, never source nobody published.

Every update carries the `vis-agent` command with it, because a command that is
older than its runtime is the one drift this design refuses: a native update
replaces it from the release bundle and a source update copies it out of the
source it just pinned. A wrapper that lives inside a checkout is source and is
only ever moved by git. If the installed command is not writable, the update
says so and leaves it alone.

Name at most one target per invocation; a conflict is an error rather than a
guess. A checkout of your own is never moved by an update: git owns it.

## Installing

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

`bin/install-vis-agent` installs exactly one runtime — **JVM source at the newest commit of `main`** — and takes one option: `--install-dir PATH` (default `~/.local/bin`, added to your shell profile when PATH lacks it).

It installs the wrapper and then hands off to `vis-agent update`, which acquires that runtime in the same command. Runtime acquisition always belongs to `vis-agent`, so the two can never drift apart.

What it needs on the machine:

- **git and curl** — required.
- **GraalVM CE 25.1.3** — a matching one already installed (including through SDKMAN) is reused; otherwise the pinned JDK is installed.
- **Clojure CLI** — installed automatically the first time the JVM runtime needs it.
- `VIS_NO_AUTO_INSTALL=1` disables every automatic tool installation.

**Nothing that install *runs* is tagged.** The source comes from the branch tip, because a published `vX.Y.Z` can be broken source and the fix lands on the branch first.

The two scripts are the exception: `curl` fetches the installer, and the installer fetches the wrapper, from `github.com/$VIS_REPO_SLUG/releases/download/installer/`.

- **Why a release asset** — `raw.githubusercontent.com` is blocked on many corporate networks, while `github.com` release downloads are not.
- **`installer` is a ROLLING release, not a version** — `.github/workflows/installer-assets.yml` force-moves that one tag and re-uploads both scripts on every commit on `main` that changes them, so the published one-liner is always the branch's own installer and no per-commit snapshot release is ever created.
- **It stays a prerelease** — so `releases/latest` still means the newest `vX.Y.Z`, which keeps its own copy of both scripts.
- **The asset is only a bootstrap** — `vis-agent update` immediately refreshes the wrapper from the source it pins.

Cloning the repository and running `bin/install-vis-agent` out of the checkout works too: it installs that checkout's own wrapper.

## Everything Vis owns

| Path | Holds |
|---|---|
| `~/.vis/install/vis-agent-native` | the private native runtime |
| `~/.vis/install/src` | the checkout Vis owns: one shallow, detached commit, no branches, no remote-tracking refs |
| `~/.vis/install/ref` | the commit that checkout sits at — always a SHA, never a branch; `vis-agent runtime` marks it `DRIFTED` when `HEAD` no longer matches |

That is the entire runtime state. Deleting `~/.vis/install` is a full reset.

That checkout is a pin, not a clone. An update fetches exactly one ref one commit
deep, checks it out detached, then deletes every branch and remote-tracking ref
and narrows the remote's fetch refspec to the branch it follows, so there is no `main` inside it
for a stray `git pull` to follow. An install made by an older, cloning `vis-agent`
is repaired into that shape by the next update.

| Variable | Effect |
|---|---|
| `VIS_HOME` | where Vis keeps its state (default `~/.vis`) |

The wrapper owns two diagnostics flags on any launch: `--measure` prints shell
and startup timings, `--jfr` records Java Flight Recorder profiles into
`$VIS_HOME`. Everything else is passed straight to Vis.

## The native release bundle

**Right now a release ships no native bundle.** v0.1.39 and v0.1.40 both published
without one — hosted macOS never finished the image and the Linux ARM64 builder ran
out of memory — so `native-release.yml` no longer runs on a tag; it is dispatched by
hand when a platform is being re-proven. A tag publishes the `vis-agent` launcher and
the Clojars deploy, and the launcher runs the JVM source runtime (`jvm` in the table
above) when no native sidecar is installed. Everything below describes that bundle for
when it is rebuilt, and `bin/release-native --tag vX.Y.Z --upload` still attaches one
to an existing release from a 32 GB machine.

Native-image output is platform-specific. Each supported platform can publish two
independent archives:

```text
vis-agent-<os>-<arch>-community.tar.gz
├── vis-agent             # public Bash wrapper
├── vis-agent-native      # private GraalVM native-image runtime
├── vis-agent-python/     # embedded CPython: cdylib + vendored interpreter
└── install-vis-agent     # installer for the source runtime

vis-tui-<os>-<arch>.tar.gz
└── vis-tui               # standalone terminal gateway client
```

The agent bundle travels together — `vis-agent update` replaces the wrapper, the
runtime and interpreter directory in one step — so those pieces cannot drift. The
TUI archive is separate because it has its own process and dependency graph; it can
be installed or upgraded without replacing the gateway. Its protocol headers make
an incompatible gateway fail explicitly rather than misrender.

`bin/stage-release-bundle` defines the agent layout. `bin/stage-tui-release` defines
the TUI archive.

| Platform | Agent bundle | TUI archive |
|---|---|---|
| Linux x86-64 | `vis-agent-linux-x64-community.tar.gz` | `vis-tui-linux-x64.tar.gz` |
| Linux ARM64 | `vis-agent-linux-arm64-community.tar.gz` | `vis-tui-linux-arm64.tar.gz` |
| macOS ARM64 | `vis-agent-macos-arm64-community.tar.gz` | `vis-tui-macos-arm64.tar.gz` |

Building the image needs two things exactly:

- **GraalVM Community Edition 25.1.3** — the repository pin is authoritative.
- **32 GB of RAM** — the points-to analysis live set is ~14 GiB, and a 16 GB host spends most of the build in GC.

On such a machine, `bin/release-native` builds every asset that host can produce, smoke-tests each one, and with `--tag vX.Y.Z --upload` attaches them to the release. On Apple silicon that is all three: macOS natively, Linux ARM64 in a container with no emulation, and Linux x86-64 through Rosetta.

Rosetta measures at 4.8x native for native-image — a hello-world image takes 15.8 s on `linux/arm64` against 1 m 16 s on `linux/amd64` — which is still well inside the 86–130 minutes the free x86-64 runner needs when it does not run out of memory. The emulator decides that, so the script measures it and refuses qemu-user, where the same analysis runs for hours and usually dies:

- **Docker Desktop** — turn on *Use Rosetta for x86_64/amd64 emulation*.
- **podman** — create the machine with `rosetta = true` under `[machine]` in `containers.conf`.

Where it was built does not change the asset: native-image targets the `x86-64-v3` baseline regardless of the builder's own CPU.

The container build also needs those 16+ GB **inside** the Linux VM, which a default
podman machine does not have. `bin/release-native` therefore looks past it: if the
default connection is too small, it uses the first other machine that is running and
large enough, and says which one it picked. `VIS_CONTAINER_CONNECTION=<machine>`
forces a specific podman machine or docker context; `VIS_CONTAINER_CLI` forces the
engine.

On a platform with no published bundle, run the source runtime, or build a
sidecar locally with `vis-agent update --rebuild`.

## See also

- [JVM & native-image](jvm-native-image.md) — how the native binary is built and what travels inside it.
- [Configuration](configuration.md) — what each runtime reads at startup.
- [Gateway, pairing & remote access](gateway.md) — the daemon a distribution starts for you.
