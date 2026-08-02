# Runtime distributions

Vis Agent has one public distribution shape: a **Bash wrapper** named
`vis-agent`, plus whichever runtime that installation provides. Users never
install or invoke the native image as `vis`.

## The wrapper is the product boundary

The wrapper keeps one collision-free command on PATH, selects the runtime,
preserves the invocation working directory, and produces a clear error when the
selected runtime is unavailable — it never silently substitutes another.

It **follows releases** by default: the published native runtime when one is
installed, otherwise JVM source pinned to the newest `vX.Y.Z` tag. A live,
moving checkout is **dev mode** — opt-in, and the only mode that tracks a branch
or hands off to the developer checkout at `$VIS_DEV_CHECKOUT` (default `~/vis`).
Being *inside* a Vis checkout does not change the runtime by itself.

There is no jar runtime and no `--jar` selector. The AOT jar produced during a
native build is an implementation artifact only.

## Selecting a runtime

| Runtime | Runs |
|---|---|
| `native` | the private `vis-agent-native` sidecar installed by `vis-agent update` |
| `jvm` | `clojure -M:vis` from source pinned to the newest release tag |
| `dev` | `clojure -M:vis` from a live checkout, tracking its branch |
| `auto` | no persisted choice: native if installed, else `jvm` |

Precedence, highest first:

1. a one-launch flag — `--native`, `--jvm` (`--source` is an alias), `--dev`;
2. `VIS_RUNTIME=native|jvm|dev` (any other value is ignored with a warning;
   there is no `VIS_RUNTIME=auto` — unset the variable instead);
3. the persisted default in `~/.vis/runtime`, written by `vis-agent runtime use`;
4. automatic: the installed native runtime, else release-tagged source.

```bash
vis-agent runtime show
vis-agent runtime use native|jvm|dev|auto   # persisted default (auto = forget it)
vis-agent --dev help                        # this launch only
VIS_RUNTIME=jvm vis-agent help              # this process only
```

`runtime show` reports the configured default, the effective runtime, the native
and JVM source paths it discovered, `Source pinned at: <tag|sha>`, and the dev
checkout.

## Updating follows the same channel

`vis-agent update` updates the channel that is effective; the flags say *what*
to update.

| Command | Effect |
|---|---|
| `vis-agent update` | native channel: install the newest release bundle. JVM channel: move the managed checkout onto the newest `vX.Y.Z` tag |
| `vis-agent update --native` | (re)download the release bundle — wrapper and sidecar together |
| `vis-agent update --jvm` | fetch tags and check the managed checkout out at the newest release tag (`--source` is an alias) |
| `vis-agent update --dev` | the only form that fast-forwards a live checkout (`git pull --ff-only`); implied when dev mode is the default |
| `vis-agent update --rebuild` | after the source update, build the native runtime locally (`clojure -T:build native`) |
| `vis-agent update <sha>` | pin the managed checkout to an exact commit and select the JVM runtime |
| `vis-agent update vX.Y.Z` | install that release's bundle instead of the newest |

A blocked `--dev` fast-forward says whether the tree is dirty or the branch has
diverged, with the counts and the exact recovery command.

## Where the state lives

| Path | Meaning |
|---|---|
| `~/.vis/runtime` | persisted runtime choice: `native`, `jvm`, or `dev` |
| `~/.vis/install/src` | the checkout Vis owns and moves between refs |
| `~/.vis/install/mode` | `jvm-tag` or `jvm-sha` — how that checkout is pinned |
| `~/.vis/install/ref`, `~/.vis/install/sha` | the tag or commit it is pinned to |
| `~/.vis/source-dir` → `~/.vis/sourcecode` | checkout recorded by `bin/install-source` |
| `$VIS_DEV_CHECKOUT` (default `~/vis`) | the live checkout dev mode runs |

Your own working checkout is never moved by `vis-agent update`; that is what dev
mode is for.

## Native release bundle

Native-image output is platform-specific, but it is a **private runtime
sidecar**, not a standalone Vis distribution. Each release archive is named:

```text
vis-agent-<os>-<arch>-community.tar.gz
```

and contains:

```text
vis-agent          # public Bash wrapper
vis-agent-native   # private GraalVM native-image runtime
install-vis-agent  # installer for the same bundle shape
```

The two executables stay next to each other. `bin/install-vis-agent` installs the
wrapper first and then has it download the runtime (`vis-agent update --native`),
and `vis-agent update` replaces both together. This prevents the launcher and
runtime contract from drifting across versions.

Release CI currently builds and smoke-tests:

| Platform | Bundle |
|---|---|
| Linux x86-64 | `vis-agent-linux-x64-community.tar.gz` |
| Linux ARM64 | `vis-agent-linux-arm64-community.tar.gz` |

macOS users use the JVM source distribution unless a maintainer provides a
matching locally built sidecar. Native builds require GraalVM Community Edition
25.1.3 exactly; the repository pin is authoritative.

## JVM source distribution

`bin/install-source` clones or updates `~/.vis/sourcecode` (override with
`VIS_SOURCE_DIR`), copies the same wrapper onto PATH, records the checkout in
`~/.vis/source-dir`, and persists `jvm` as the default runtime. The runtime is
`clojure -M:vis` from that checkout; no jar is copied or selected.

A source installation and a native sidecar can coexist, and switching the
persisted runtime reinstalls nothing:

```bash
vis-agent runtime use jvm      # source at the newest release tag
vis-agent runtime use native
vis-agent runtime use dev      # live checkout, tracking its branch
vis-agent runtime use auto     # no persisted choice
```

`vis-agent update` keeps that checkout — like `~/.vis/install/src` — on the
newest release tag and records it in `~/.vis/install/ref`.
