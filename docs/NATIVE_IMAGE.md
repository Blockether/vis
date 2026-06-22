# GraalVM native-image

vis compiles to a standalone native binary (`target/vis`, no JVM required) with
GraalVM `native-image` 25+. This is non-trivial because vis embeds **GraalPy**
(Truffle/polyglot) and uses **FFM** (rift) — both are native-access heavy — and
loads extensions **dynamically** at runtime via a classpath manifest scan.

## Build

```bash
clojure -T:build uber     # AOT every ns (core + extensions) -> target/vis.jar
clojure -T:build native   # uber, then native-image -> target/vis  (target/vis.exe on Windows)
# or, self-hosting once a binary exists / from the wrapper:
bin/vis native
```

Requirements: Oracle GraalVM or GraalVM CE **25+** with `native-image` on
`PATH`, and **≥16 GB RAM** — GraalPy's `libpythonvm` needs `-Xms14g` for the
builder (GraalPy sets this itself; see below). The build takes several minutes.

CI runs the same two tasks on Linux/macOS/Windows (`.github/workflows/native.yml`).

## How configuration is delivered (the important part)

native-image **auto-discovers** config from every jar/dir on the image
classpath under:

```
META-INF/native-image/<groupId>/<artifactId>/
    ├── native-image.properties      # build args (Args = …, JavaArgs = …)
    └── reachability-metadata.json    # unified reflection / resources / jni / …
```

`reachability-metadata.json` is the **modern unified format** (GraalVM/JDK 23+,
schema v1.x). It REPLACES the legacy split files (`reflect-config.json`,
`resource-config.json`, `jni-config.json`, `proxy-config.json`,
`serialization-config.json`) — those are deprecated but still read for old libs.
Don't write the legacy files for new config; write the unified one.

Because this config travels INSIDE the image classpath, the `native-image`
command line stays tiny (`build.clj` passes only `-jar`/`-o` + the
`vis-extension`/`.edn` resource includes). Each library and each vis artifact
owns its slice:

| Owner | Path | Carries |
|-------|------|---------|
| **GraalPy** (`org.graalvm.python/python-language`) | shipped in its jar | `--initialize-at-build-time=com.oracle.graal.python,…`, `--features=…BouncyCastleFeature`, `-H:MaxRuntimeCompileMethods=20000`, `JavaArgs=-Xms14g`, its reflect/resource config |
| Other Java libs (sqlite-jdbc, jetty, jackson, icu4j, …) | shipped in their jars | their own metadata |
| **vis main** (`com.blockether/vis`) | `resources/META-INF/native-image/com.blockether/vis/` | vis's build args (graal-build-time feature, `--no-fallback`, native access) + the **app-wide** `reachability-metadata.json` (Clojure/babashka/nREPL/sqlite/app reflection + resources) |
| **each extension** (`com.blockether/vis-…`) | `<ext>/resources/META-INF/native-image/com.blockether/vis-…/` | a `native-image.properties` + `reachability-metadata.json` for any JAVA-library reachability that extension uniquely pulls in |

**Rule: never duplicate config a library already ships.** GraalPy is officially
native-image-supported and ships everything it needs; vis only adds the app-level
reflection the tracing agent can't attribute to a library, plus its own flags.

### Why no per-namespace `--initialize-at-build-time`

`graal-build-time` (`com.github.clj-easy/graal-build-time`, the `:native` alias)
provides the `InitClojureClasses` feature, enabled once in main's
`native-image.properties`. It initializes **every** Clojure-generated class at
build time, so no extension needs its own `--initialize-at-build-time` for
Clojure code. Add `--initialize-at-build-time`/`--initialize-at-run-time` to an
extension's `native-image.properties` only for a **Java** class it pulls in.

### Why there's a custom build-time Feature

graal-build-time registers every Clojure package for **build-time class init**,
but GraalVM runs each `<clinit>` raw on a parallel analysis worker thread with no
Clojure thread-binding frame. Any namespace with a top-level
`(set! *warn-on-reflection* true)` (most libraries: babashka.fs, next.jdbc,
rewrite-clj, honeysql, nippy, …) then dies with:

```
java.lang.IllegalStateException: Can't change/establish root binding of:
*warn-on-reflection* with set
```

`set!` on a dynamic var needs a thread binding. Core namespaces survive only
because clojure.core's bootstrap loads them through `require` (which pushes that
binding); libraries reached directly by the analysis don't.

Fix: `com.blockether.vis.internal.nativeimage` — a native-image `Feature` that,
in `beforeAnalysis` (builder JVM, before the analysis raw-inits anything),
`require`s every app + extension namespace with `*warn-on-reflection*` bound. Each
class is then initialized through Clojure's loader (binding active), so its `set!`
succeeds before build-time init touches it. It's enabled next to graal-build-time
in main's `native-image.properties`. **A new library that does top-level
`(set! *warn-on-reflection* …)` is handled automatically** as long as it's
reachable via `require` from the app or an extension.

### Why the manifest is merged at build time

Each extension ships `META-INF/vis-extension/vis.edn`; at runtime
`manifest.clj` enumerates one-per-classpath-entry. In a single uberjar they all
collide to one path, so `build.clj`'s `merge-extension-manifests!` combines them
into ONE multi-id map before AOT. `manifest.clj` already iterates a multi-id map.

## Class-initialization flags (build-time vs run-time)

GraalVM 25 initializes classes at **run time** by default; graal-build-time flips
Clojure-generated classes to **build time**. Two failure modes then need explicit
flags, all kept in main's `native-image.properties`:

1. **"An object of type X was found in the image heap … initialized at run time"** —
   a build-time-initialized namespace holds an immutable instance of a Java lib
   whose class defaults to run-time. Fix: `--initialize-at-build-time=<package>`
   for that lib (we do this for malli, jackson, commonmark, lanterna, jtokkit,
   slf4j, the borkdude/taoensso/honey/charred/… Clojure libs, …). Use the real
   Java **package** — for a Clojure ns `foo.bar` the classes live in package
   `foo` (path `foo/bar$x.class`), so target `foo`, not `foo.bar`.

2. **"Detected an instance of Random/SplittableRandom"** or a live resource
   (HttpClient, thread pool, file cache) **in the image heap** — something
   stateful was created at build time and would bake a fixed seed / dead handle
   into the binary. Fix the SOURCE when it's ours (make it a `delay` — see the
   MCP client's `http-client`), or `--initialize-at-run-time=<class>` for library
   classes (e.g. JGit's `WindowCache`). When a whole library is full of such
   state (JGit), don't build-time-init it at all — leave it run-time (default).

Iterate: `--no-fallback` fails the build on the first offender and names it; add
a flag and rebuild. The analysis fails fast (~20s), so this converges quickly.

## Regenerating reachability metadata (the tracing agent)

Hand-writing reflection config is error-prone. Generate it by running the app
under the native-image **tracing agent**, which records every reflective access,
resource load, and JNI call:

```bash
# app-wide (main): exercise core paths, MERGE across runs into main's dir
DIR=resources/META-INF/native-image/com.blockether/vis
java -agentlib:native-image-agent=config-output-dir=$DIR -jar target/vis.jar --version
java -agentlib:native-image-agent=config-merge-dir=$DIR  -jar target/vis.jar help
# … exercise more real flows (a session, web serve, a tool call) with config-merge-dir
```

For a single extension, run THAT extension's tests under the agent with
`config-merge-dir=<ext>/resources/META-INF/native-image/com.blockether/<artifact>`.

The agent emits the unified `reachability-metadata.json`. Re-run after adding
reflective code or a new dynamically-loaded dependency.

**Platform caveat:** some captured resources are OS/arch-specific (e.g. GraalPy's
`META-INF/resources/.../darwin/aarch64/…` native-lib hashes). Metadata captured
on macOS is incomplete for Linux/Windows resource globs. GraalPy handles its OWN
per-platform native libs via its build-time feature, so vis's captured globs are
a best-effort baseline; if a native build fails on another OS for a missing
resource, regenerate on that OS (or broaden the glob) and merge.

## Debugging a failed build

- `--no-fallback` is on, so a missing reflection/resource entry fails the build
  (or, with `--report-unsupported-elements-at-runtime`, throws at runtime). The
  error names the class/method/resource — add it to the right
  `reachability-metadata.json` (prefer regenerating via the agent).
- `-H:+ReportExceptionStackTraces` (on via main's properties) gives full traces.
- Out-of-memory in the builder → GraalPy's `-Xms14g` needs real RAM; close other
  apps or use a bigger CI runner.
