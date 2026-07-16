<p align="center">
  <img src="logo.png" alt="vis logo" width="240"/>
</p>

# Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects/changes the host project through tools.

## Install

One command clones Vis, checks the runtime tools, and puts the `vis` launcher on your `PATH`.

**macOS & Linux** (bash):

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

Then confirm:

```bash
vis help
```

Prereqs: `git`, `java` 21+, and the [Clojure CLI](https://clojure.org/guides/install_clojure) 1.12+ — the installer checks for them and tells you what's missing. Building the native binary locally needs Oracle GraalVM or GraalVM CE 25+ with at least 16 GB RAM.

## What `vis` runs

`vis` is the stable command. It proxies to the best available distribution, in this order:

1. managed native binary from `vis update` (`$VIS_HOME/install`, default `~/.vis/install`)
2. repo native binary (`target/vis` or `target/vis.exe`)
3. repo JVM uberjar (`target/vis.jar`)
4. live source (`clojure -M:vis`)

Use `vis --jvm ...` to skip native and force the JVM path. The JVM path needs a **stock JDK 25** (e.g. `sdk install java 25.0.3-tem`) or a GraalVM matching the pinned Truffle line (`graal-25.1.3`); a mismatched GraalVM (e.g. CE 25.0.2) is rejected at startup with a clear message.

## Build / develop

```bash
vis native          # builds target/vis(.exe) and target/vis.jar
./bin/dev nrepl    # project nREPL
./verify.sh --quick
```

Contributor rules: [AGENTS.md](AGENTS.md).
