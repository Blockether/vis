# Runtime distributions

`vis-agent` is the whole product surface: one Bash command on PATH that does
two things and nothing else.

1. run Vis,
2. update Vis, and the command with it — `vis-agent update`.

There is no second command to learn, and no runtime to choose: what is
installed is what runs, and `vis-agent runtime` reports it. The native image is
a private sidecar called `vis-agent-native` that lives beside the wrapper; it is
never installed as `vis` (Linux already has an unrelated `vis`) and never
invoked directly. `target/vis.jar` is a build artifact, not a runtime.

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

### Tracks

A build carries the track it was made for in its stamp, and `vis-agent runtime`
prints it beside the commit that produced the binary.

| Track | Who builds it | Followable |
|---|---|---|
| `stable` | a release tag; the assets on the newest release | yes, and the default |
| `beta` | the rolling per-commit prerelease, built on free runners, so Linux only | yes |
| `dev` | a build of your own: `clojure -T:build native`, or `vis-agent update --rebuild` | no |
| `dry-run` | a CI build that published nothing | no |

`dev` and `dry-run` are stamps, not destinations: nothing publishes them, so
`--track dev` is refused, and a runtime carrying one moves forward by being
rebuilt rather than updated. Switching track is explicit and it sticks — an
update never changes track on its own, or a beta tester silently falls back to
stable and files bugs against a build that was never running. Naming a version
is a one-off and leaves the remembered track where it was.

A track is not a channel: in Vis a channel is a user interface an extension
registers (TUI, web, Telegram).

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

`bin/install-vis-agent` takes one option, `--install-dir PATH` (default
`~/.local/bin`, added to your shell profile when PATH lacks it), and installs
exactly one runtime: JVM source at the newest commit of `main`. It installs the
wrapper and then hands off to `vis-agent update`, which acquires that runtime in
the same command, so runtime acquisition always belongs to `vis-agent` and the
two cannot drift apart. It requires git and curl. A JVM launch reuses a matching GraalVM CE 25.1.3 already installed (including through SDKMAN), or installs the pinned JDK when no Java is available; the Clojure CLI is installed automatically when the JVM runtime first needs it. Set `VIS_NO_AUTO_INSTALL=1` to disable automatic tool installation.

Nothing that install *runs* is tagged: the source comes from the branch tip,
because a published `vX.Y.Z` can be broken source and a fix lands on the branch
first. The two scripts are the exception — `curl` fetches the installer, and the
installer fetches the wrapper, from
`github.com/$VIS_REPO_SLUG/releases/download/installer/`, because
`raw.githubusercontent.com` is blocked on many corporate networks while
`github.com` release downloads are not. `installer` is a ROLLING release, not a
version: `.github/workflows/installer-assets.yml` force-moves that one tag and
re-uploads both scripts on every commit on `main` that changes them, so the
published one-liner is always the branch's own installer and no per-commit
snapshot release is ever created. It stays a prerelease, so `releases/latest`
still means the newest `vX.Y.Z`, which keeps its own copy of both scripts. The
asset is only a bootstrap: `vis-agent update` immediately refreshes the wrapper
from the source it pins. Cloning the
repository and running `bin/install-vis-agent` out of the checkout works too — it
installs that checkout's own wrapper.

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

Native-image output is platform-specific, so a native release publishes exactly one
archive per platform:

```text
vis-agent-<os>-<arch>-community.tar.gz
├── vis-agent             # public Bash wrapper
├── vis-agent-native      # private GraalVM native-image runtime
├── vis-agent-resources/  # GraalPy/Truffle language resources
└── install-vis-agent     # installer for the source runtime
```

All of it travels together — `vis-agent update` replaces the wrapper, the
runtime and the resources directory in one step — so the launcher, the runtime and
the Python stdlib can never drift across versions. The resources directory is not
optional: the image keeps those resources beside itself instead of inside itself
(see [JVM & native-image](jvm-native-image.md)), and the wrapper starts the runtime
with `-Dpolyglot.engine.resourcePath` pointing at it. `VIS_NATIVE_RESOURCES`
points at a different copy when several installs share one.

`bin/stage-release-bundle` is the single definition of that layout, used by both
release CI and local builds, so a hand-built asset and a CI asset are identical.

| Platform | Bundle | Built by |
|---|---|---|
| Linux x86-64 | `vis-agent-linux-x64-community.tar.gz` | release CI, or a local container build (Rosetta-emulated on Apple silicon) |
| Linux ARM64 | `vis-agent-linux-arm64-community.tar.gz` | release CI, or a local container build |
| macOS ARM64 | `vis-agent-macos-arm64-community.tar.gz` | release CI on the repository's own Apple-silicon runner (label `vis-macos-arm64`), any Apple-silicon machine with 32 GB, or a cloud Apple-silicon runner with at least 16 GiB (repository variable `VIS_MACOS_ARM64_RUNNER`) — no GitHub-hosted macOS runner can finish this image |

Building the image needs GraalVM Community Edition 25.1.3 exactly (the repository
pin is authoritative) and a machine with **32 GB of RAM**: the points-to analysis
live set is ~14 GiB, and a 16 GB host spends most of the build in GC. On such a
machine, `bin/release-native` builds every asset that host can produce, smoke-tests
each one, and with `--tag vX.Y.Z --upload` attaches them to the release. On Apple
silicon that is all three: macOS natively, Linux ARM64 in a container with no
emulation, and Linux x86-64 through Rosetta — measured at 4.8x native for
native-image (a hello-world image takes 15.8 s on `linux/arm64` against 1 m 16 s on
`linux/amd64`), which is still well inside the 86–130 minutes the free x86-64 runner
needs when it does not run out of memory. The emulator decides that, so the script
measures it and refuses qemu-user, where the same analysis runs for hours and
usually dies: Docker Desktop needs *Use Rosetta for x86_64/amd64 emulation*, podman
needs a machine created with `rosetta = true` under `[machine]` in
`containers.conf`. Where it was built does not change the asset — native-image
targets the `x86-64-v3` baseline regardless of the builder's own CPU.

The container build also needs those 16+ GB **inside** the Linux VM, which a default
podman machine does not have. `bin/release-native` therefore looks past it: if the
default connection is too small, it uses the first other machine that is running and
large enough, and says which one it picked. `VIS_CONTAINER_CONNECTION=<machine>`
forces a specific podman machine or docker context; `VIS_CONTAINER_CLI` forces the
engine.

On a platform with no published bundle, run the source runtime, or build a
sidecar locally with `vis-agent update --rebuild`.
