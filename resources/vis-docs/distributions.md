# Runtime distributions

`vis-agent` is the whole product surface: one Bash command on PATH that does
three things and nothing else.

1. run Vis on the selected runtime,
2. show that selection — `vis-agent runtime`,
3. update — and select — a runtime — `vis-agent update`.

There is no second command to learn. The native image is a private sidecar
called `vis-agent-native` that lives beside the wrapper; it is never installed
as `vis` (Linux already has an unrelated `vis`) and never invoked directly.
`target/vis.jar` is a build artifact, not a runtime you can select.

## Three runtimes, one word each

| Runtime | Runs | Moves when |
|---|---|---|
| `native` | the private `vis-agent-native` sidecar | `vis-agent update` |
| `jvm` | `clojure -M:vis` from the checkout Vis owns, pinned to the newest `vX.Y.Z` tag | `vis-agent update` |
| `dev` | `clojure -M:vis` from your live checkout | you pull or commit |

`auto` is not a fourth runtime; it is the *absence* of a choice: native when a
sidecar is installed, otherwise tagged source. So Vis follows releases by
default, `dev` is the only runtime that follows a moving branch, and `dev` is
never selected for you — not even when you run Vis from inside a Vis checkout.

Running a checkout's own `bin/vis-agent` does run that checkout: invoking it
*is* the choice.

## Choosing a runtime

```bash
vis-agent update native|jvm|dev             # acquire it, update it, select it
vis-agent runtime show
vis-agent runtime use native|jvm|dev|auto   # switch only (auto = forget the choice)
vis-agent --native|--jvm|--dev help         # this launch only
VIS_RUNTIME=dev vis-agent help              # this process only
```

Precedence, highest first:

1. a one-launch flag: `--native`, `--jvm`, `--dev`;
2. `VIS_RUNTIME=native|jvm|dev` — any other value warns and is ignored, and
   there is no `VIS_RUNTIME=auto`: unset the variable instead;
3. `~/.vis/runtime`, written by `vis-agent update <runtime>` and by
   `vis-agent runtime use`;
4. automatic — follow the releases.

`runtime show` names the winner and who chose it:

```text
Runtime:      native (--native)          # or VIS_RUNTIME, ~/.vis/runtime, automatic
Native:       ~/.vis/install/vis-agent-native
Source:       ~/.vis/install/src
Pinned at:    v0.1.22
Dev checkout: ~/vis
```

A selected runtime that is not installed is an error with the command that
fixes it. The wrapper never silently substitutes another runtime.

## Updating

`vis-agent update` updates the runtime that is in effect; a flag updates a
different one. Naming a runtime also makes it the default, so `vis-agent update
dev` is the whole switch — there is no `runtime use` to follow it with.

| Command | Updates |
|---|---|
| `vis-agent update` | whichever runtime is in effect |
| `vis-agent update --native` | downloads the newest release bundle — wrapper and sidecar together |
| `vis-agent update --jvm` | fetches tags and checks the owned checkout out at the newest `vX.Y.Z`, then refreshes the `vis-agent` command from that source |
| `vis-agent update --dev` | clones the dev checkout on `main` when that path holds none, otherwise `git fetch` + `git pull --ff-only` — the only update that follows a branch |
| `vis-agent update vX.Y.Z` | that release instead of the newest: bundle for `native`, tag for `jvm` |
| `vis-agent update <sha\|branch>` | any target that is not `vX.Y.Z` is a git ref, so it pins the owned checkout and implies `--jvm` |
| `vis-agent update --rebuild` | after a source update, builds the sidecar locally (`clojure -T:build native`); pairs with `--jvm` or `--dev` |

Every update carries the `vis-agent` command with it, because a command that is
older than its runtime is the one drift this design refuses: `--native` replaces
it from the release bundle, `--jvm` copies it out of the source it just pinned,
and `dev` needs no copy — dev mode always execs your checkout's own
`bin/vis-agent`. A wrapper that lives inside a checkout is source and is only
ever moved by git. If the installed command is not writable, the update says so
and leaves it alone.

Name at most one runtime and at most one target per invocation; a conflict is
an error rather than a guess. Your own checkout is never moved unless you say
`--dev`, and a blocked fast-forward reports what blocked it — dirty tree or
diverged history, with the counts and the exact recovery command.

## Installing

```bash
curl -fsSL https://github.com/Blockether/vis/releases/latest/download/install-vis-agent | bash
curl -fsSL https://github.com/Blockether/vis/releases/latest/download/install-vis-agent | bash -s -- --runtime jvm
```

`bin/install-vis-agent` takes `--runtime native|jvm` (default `native`),
`--version vX.Y.Z|latest`, and `--install-dir PATH` (default `~/.local/bin`,
added to your shell profile when PATH lacks it). It installs the wrapper and
then hands off: `vis-agent update --native|--jvm` acquires that runtime and
persists the choice in the same command. Runtime acquisition therefore always belongs to
`vis-agent`, so wrapper and runtime cannot drift apart. `--runtime jvm`
additionally requires Java 25+, the Clojure CLI, and git.

`install-vis-agent` and the `vis-agent` wrapper are release assets, published on
every `vX.Y.Z` release beside the platform bundles: `releases/latest/download`
always serves the newest pair and `releases/download/vX.Y.Z` a pinned one, so the
installer never fetches from a raw source host. Cloning the repository and
running `bin/install-vis-agent` out of the checkout works too — it installs that
checkout's own wrapper.

## Everything Vis owns

| Path | Holds |
|---|---|
| `~/.vis/runtime` | the persisted selection: `native`, `jvm`, or `dev`; absent means automatic |
| `~/.vis/install/vis-agent-native` | the private native runtime |
| `~/.vis/install/src` | the checkout Vis owns: one shallow, detached commit, no branches, no remote-tracking refs |
| `~/.vis/install/ref` | the tag or commit that checkout sits at; `runtime show` marks it `DRIFTED` when `HEAD` no longer matches |
| `$VIS_DEV_CHECKOUT` (default `~/vis`) | the live checkout `dev` runs; `vis-agent update --dev` clones it there on `main` when it is missing |

That is the entire runtime state. Deleting `~/.vis/runtime` returns to
automatic; deleting `~/.vis/install` is a full reset.

That checkout is a pin, not a clone. An update fetches exactly one ref one commit
deep, checks it out detached, then deletes every branch and remote-tracking ref
and narrows the remote's fetch refspec to tags, so there is no `main` inside it
for a stray `git pull` to follow. An install made by an older, cloning `vis-agent`
is repaired into that shape by the next update.

| Variable | Effect |
|---|---|
| `VIS_RUNTIME` | runtime for this process: `native`, `jvm`, or `dev` |
| `VIS_DEV_CHECKOUT` | where the `dev` runtime lives (default `~/vis`) |
| `VIS_HOME` | where Vis keeps its state (default `~/.vis`) |

The wrapper owns two diagnostics flags on any launch: `--measure` prints shell
and startup timings, `--jfr` records Java Flight Recorder profiles into
`$VIS_HOME`. Everything else is passed straight to Vis.

## The native release bundle

Native-image output is platform-specific, so each release publishes exactly one
archive per platform:

```text
vis-agent-<os>-<arch>-community.tar.gz
├── vis-agent             # public Bash wrapper
├── vis-agent-native      # private GraalVM native-image runtime
├── vis-agent-resources/  # GraalPy/Truffle language resources
└── install-vis-agent     # installer for the same bundle shape
```

All of it travels together — `vis-agent update --native` replaces the wrapper, the
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
| macOS ARM64 | `vis-agent-macos-arm64-community.tar.gz` | an Apple-silicon machine — no hosted macOS runner has the RAM |

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

On a platform with no published bundle, use `jvm`, or build a sidecar locally with
`vis-agent update --jvm --rebuild`.
