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

Prereqs: `git`, `java` 25+, and the [Clojure CLI](https://clojure.org/guides/install_clojure) 1.12+ — the installer checks for them and tells you what's missing. **Building** the native binary needs **GraalVM Community Edition 25.2.4** — exactly that edition and version, pinned in [`.graalvm-version`](.graalvm-version) — plus at least 16 GB RAM. Get it with `bin/require-graalvm --install`, then `sdk env` (the repo ships a `.sdkmanrc`). Oracle GraalVM and stock JDKs are rejected by the build, on purpose: CE's Classpath Exception is what keeps the shipped binary FOSS, and Truffle hard-refuses any other version against the `org.graalvm.*` pins in `deps.edn`.

## What `vis` runs

`vis` is the stable command. It proxies to the best available distribution, in this order:

1. managed native binary from `vis update` (`$VIS_HOME/install`, default `~/.vis/install`)
2. repo native binary (`target/vis`)
3. repo JVM uberjar (`target/vis.jar`)
4. live source (`clojure -M:vis`)

Use `vis --jvm ...` to skip native and force the JVM path. The JVM path needs a **stock JDK 25** (e.g. `sdk install java 25.0.3-tem`) or the pinned GraalVM (`graal-25.2.4`). If a long-lived runner inherited a mismatched GraalVM, the launcher automatically selects an already-installed pinned CE before starting Clojure; when the pin is not installed it stops with the exact install command instead of reaching Truffle.

## Build / develop

```bash
vis native                       # builds target/vis and target/vis.jar
clojure -M:format check
clojure -M:lint src extensions test build.clj
clojure -M:test
```
