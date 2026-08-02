# Runtime distributions

`vis-agent` is the whole product surface: one Bash command on PATH that does
three things and nothing else.

1. run Vis on the selected runtime,
2. show or persist that selection — `vis-agent runtime`,
3. update that same runtime — `vis-agent update`.

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
vis-agent runtime show
vis-agent runtime use native|jvm|dev|auto   # persisted default (auto = forget it)
vis-agent --native|--jvm|--dev help         # this launch only
VIS_RUNTIME=dev vis-agent help              # this process only
```

Precedence, highest first:

1. a one-launch flag: `--native`, `--jvm`, `--dev`;
2. `VIS_RUNTIME=native|jvm|dev` — any other value warns and is ignored, and
   there is no `VIS_RUNTIME=auto`: unset the variable instead;
3. `~/.vis/runtime`, written by `vis-agent runtime use`;
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
different one.

| Command | Updates |
|---|---|
| `vis-agent update` | whichever runtime is in effect |
| `vis-agent update --native` | downloads the newest release bundle — wrapper and sidecar together |
| `vis-agent update --jvm` | fetches tags and checks the owned checkout out at the newest `vX.Y.Z` |
| `vis-agent update --dev` | `git fetch` + `git pull --ff-only` in the dev checkout — the only update that follows a branch |
| `vis-agent update vX.Y.Z` | that release instead of the newest: bundle for `native`, tag for `jvm` |
| `vis-agent update <sha\|branch>` | any target that is not `vX.Y.Z` is a git ref, so it pins the owned checkout and implies `--jvm` |
| `vis-agent update --rebuild` | after a source update, builds the sidecar locally (`clojure -T:build native`); pairs with `--jvm` or `--dev` |

Name at most one runtime and at most one target per invocation; a conflict is
an error rather than a guess. Your own checkout is never moved unless you say
`--dev`, and a blocked fast-forward reports what blocked it — dirty tree or
diverged history, with the counts and the exact recovery command.

## Installing

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-vis-agent | bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-vis-agent | bash -s -- --runtime jvm
```

`bin/install-vis-agent` takes `--runtime native|jvm` (default `native`),
`--version vX.Y.Z|latest`, and `--install-dir PATH` (default `~/.local/bin`,
added to your shell profile when PATH lacks it). It installs the wrapper and
then hands off: `vis-agent update` acquires the runtime and `vis-agent runtime
use` persists the choice. Runtime acquisition therefore always belongs to
`vis-agent`, so wrapper and runtime cannot drift apart. `--runtime jvm`
additionally requires Java 25+, the Clojure CLI, and git.

Corporate proxies often block `raw.githubusercontent.com`. Clone from
`github.com` and run `bin/install-vis-agent` out of the checkout: it installs
that checkout's own wrapper without touching the raw host.

## Everything Vis owns

| Path | Holds |
|---|---|
| `~/.vis/runtime` | the persisted selection: `native`, `jvm`, or `dev`; absent means automatic |
| `~/.vis/install/vis-agent-native` | the private native runtime |
| `~/.vis/install/src` | the checkout Vis owns |
| `~/.vis/install/ref` | the tag or commit that checkout sits at |
| `$VIS_DEV_CHECKOUT` (default `~/vis`) | the live checkout `dev` runs |

That is the entire runtime state. Deleting `~/.vis/runtime` returns to
automatic; deleting `~/.vis/install` is a full reset.

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
├── vis-agent          # public Bash wrapper
├── vis-agent-native   # private GraalVM native-image runtime
└── install-vis-agent  # installer for the same bundle shape
```

The two executables travel together — `vis-agent update --native` replaces both
— so the launcher and runtime contract can never drift across versions.

Release CI builds and smoke-tests:

| Platform | Bundle |
|---|---|
| Linux x86-64 | `vis-agent-linux-x64-community.tar.gz` |
| Linux ARM64 | `vis-agent-linux-arm64-community.tar.gz` |

Elsewhere — macOS today — use `jvm`, or build a sidecar locally with
`vis-agent update --jvm --rebuild`. Native builds require GraalVM Community
Edition 25.1.3 exactly (the repository pin is authoritative) and at least
16 GB of RAM.
