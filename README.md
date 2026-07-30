<p align="center">
  <img src="logo.png" alt="vis logo" width="240"/>
</p>

# Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects/changes the host project through tools.

## Install

Three supported methods. Pick by your network, not by taste — all three end with `vis` on your `PATH`, confirmed by `vis help`.

**In a company network, use method 1 (source) or method 2 (native binary).** The one-line installer is fetched from `raw.githubusercontent.com`, which corporate proxies very commonly block.

### 1. Git clone — source install (recommended everywhere, required behind most corporate proxies)

```bash
git clone https://github.com/Blockether/vis.git ~/.vis/sourcecode
~/.vis/sourcecode/bin/install-source   # checks java/clojure/git, symlinks ~/.local/bin/vis
vis help
```

Touches `github.com` only (plus Maven Central and Clojars when dependencies are first resolved). `bin/install-source` is idempotent: on an existing checkout it fast-forwards it and re-points the symlink, so rerunning is safe. Prefer another location? `bin/install-source --dest /path/to/checkout`, or clone anywhere and symlink `<checkout>/bin/vis` onto your `PATH` yourself — the launcher resolves its repo through the symlink. If `~/.local/bin` is not on `PATH`, add it: `echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc`.

### 2. Prebuilt native binary — no JVM, no Clojure CLI, one file

Assets on the [Releases page](https://github.com/Blockether/vis/releases) are named `vis-<os>-<arch>-community`:

```bash
curl -fL -o ~/.local/bin/vis \
  https://github.com/Blockether/vis/releases/download/v0.1.13/vis-linux-arm64-community
chmod +x ~/.local/bin/vis
vis help
```

Releases currently carry **Linux x64 and arm64 only**, and not every tag carries both — open the tag and take the asset that is actually there. There is deliberately no macOS job (free macOS runners have too little RAM, and there is no community JDK for macOS x64); **on macOS, install from source (method 1) and build the binary once with `vis native`**. From an existing checkout, `vis update --native` performs the same download via `api.github.com`. Bytes are served by `github.com` → `release-assets.githubusercontent.com`.

### 3. One-line installer — personal machines

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

Exactly the script from method 1, only fetched over `raw.githubusercontent.com`. A hang or `curl: (22)/(35)` here is your proxy blocking that host — switch to method 1 rather than fighting it.

### Hosts to allowlist

| Host | Needed for |
|---|---|
| `github.com` | `git clone`, release-asset downloads |
| `api.github.com` | `vis update` (release lookup) |
| `release-assets.githubusercontent.com` | release binary bytes (redirect target) |
| `repo1.maven.org`, `repo.clojars.org` | JVM/source dependency resolution |
| `raw.githubusercontent.com` | **only** the one-line installer (method 3) |
| your model provider's API | running the agent at all |

Prereqs for methods 1 and 3: `git`, `java` 25+, and the [Clojure CLI](https://clojure.org/guides/install_clojure) 1.12+ — the installer checks for them and tells you what's missing (`brew install openjdk@25 clojure/tools/clojure git`). The native binary (method 2) needs none of them. **Building** the native binary needs **GraalVM Community Edition 25.2.4** — exactly that edition and version, pinned in [`.graalvm-version`](.graalvm-version) — plus at least 16 GB RAM. Get it with `bin/require-graalvm --install`, then `sdk env` (the repo ships a `.sdkmanrc`). Oracle GraalVM and stock JDKs are rejected by the build, on purpose: CE's Classpath Exception is what keeps the shipped binary FOSS, and Truffle hard-refuses any other version against the `org.graalvm.*` pins in `deps.edn`.

## What `vis` runs

`vis` is the stable command. It proxies to the best available distribution, in this order:

1. **inside a Vis checkout** — live source (`clojure -M:vis`), so your working-tree edits always win (set `VIS_PREBUILT=1` to prefer a built binary instead)
2. managed native binary from `vis update` (`$VIS_HOME/install`, default `~/.vis/install`)
3. repo native binary (`target/vis`)
4. live source (`clojure -M:vis`)

The AOT uberjar is **never** auto-selected — it would silently shadow working-tree edits. Force a distribution with `vis --source ...` (live source; `--jvm` is the back-compat alias), `vis --jar ...` (`target/vis.jar`, built by `vis uber`) or `vis --native ...` (`target/vis`). The JVM path needs a **stock JDK 25** (e.g. `sdk install java 25.0.3-tem`) or the pinned GraalVM (`graal-25.2.4`); JDK 21–24 are too old. If a long-lived runner inherited a mismatched GraalVM, the launcher automatically selects an already-installed pinned CE before starting Clojure; when the pin is not installed it stops with the exact install command instead of reaching Truffle.

## Build / develop

```bash
vis native                       # builds target/vis and target/vis.jar
clojure -M:format check
clojure -M:lint src extensions test build.clj
clojure -M:test
```
