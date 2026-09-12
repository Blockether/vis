# Native builds for Java and Clojure extensions

Use this guide when you are adding **Java or Clojure capabilities inside Vis**
and want to ship them in a native engine without requiring a JVM at runtime.
This is an in-tree engine build, not a drop-in JAR plugin system.

You do **not** need a native build to use the Python SDK, connect a Java/Clojure
client, run a gateway or add a Python extension. Use a [prebuilt runtime](distributions.md)
for those tasks; start with [Extending Vis](extending.md) for Python tools.

## Prerequisites

- A source checkout, the Clojure CLI and the usual build tools for your platform.
  Run the commands below from the repository root.
- GraalVM Community Edition at the version in `.graalvm-version`.
  `bin/require-graalvm` installs and selects it. The build rejects Oracle
  GraalVM and mismatched versions before starting `native-image`.
- At least 16 GB RAM for the builder JVM, plus room for the operating system,
  dependency cache and build outputs. Build on a larger machine rather than a
  small production VPS. `VIS_NATIVE_BUILDER_HEAP` can bound the builder heap,
  for example `12g`; a smaller heap is not a substitute for enough memory.

Build for the target operating system and architecture. A macOS binary does not
run on Linux. `vis-agent update --track dev` updates managed JVM source; it does
not compile a native image.

## Add and test your JVM capability

Make the capability work on the JVM before compiling a native image:

1. Add your code to the relevant domain under `src/com/blockether/vis/internal/`
   and any required library to `deps.edn`. Java libraries must be on the build
   classpath; adding a JAR beside an already-built binary does not load new code.
2. Register tool bindings through `com.blockether.vis.internal.extension.core`;
   a Clojure binding can call your Java API. Give each exported tool an explicit
   input/output contract and a human-readable Activity presentation.
3. If you add a module initializer, include its qualified registration symbol in
   `resources/META-INF/vis/manifest.edn`'s ordered `:initialization` vector, after
   its dependencies. This manifest registers built-in modules and supplies the
   native build's entry points; it is not the Python extension loader.
4. Add tests under `test/` for registration and actual calls, including failure
   cases. Run the affected JVM tests with `clojure -M:test --namespace your.test-ns`.
   Add matching execution coverage under `test-native/` for the compiled engine.

Native compilation cannot discover arbitrary runtime-loaded classes. Check your
library's reflection, resources and foreign-function requirements in
[Native-image configuration](#native-image-configuration), then rebuild whenever
you change JVM code or dependencies. Python extensions remain separately loadable.

## Build and test the image

Select the pinned toolchain, then compile:

```bash
eval "$(bin/require-graalvm --export)"
clojure -T:build native
```

The build performs Clojure ahead-of-time compilation and invokes GraalVM
Native Image. Expect a substantial build, not the startup time of a normal
Java program. On success you get:

| Output | Purpose |
| --- | --- |
| `target/vis` | Compiled native engine |
| `target/vis.build` | Version, source revision, release track and build time |
| `target/vis-agent-python/` | Required Python worker, native library and interpreter files |
| `target/vis.jar` | Intermediate JVM build artifact, not a native release bundle |

Run the native suite against the image you just built:

```bash
clojure -M:test-native
```

To test another built image, set `VIS_NATIVE_BIN` to its path. Keep its matching
Python sidecar available too. These tests execute the binary; passing JVM tests
or reaching the end of `native-image` does not prove that tools, HTTP or Python
work inside it.

## Package and run your build

Stage the complete engine bundle. This command **replaces**
`target/release-bundle` and writes the named archive; keep unrelated files out of
that staging directory. Create a fresh test home outside your project:

```bash
bin/stage-release-bundle target/vis target/vis-agent-local.tar.gz
export VIS_TEST_HOME="$(mktemp -d)"
export VIS_HOME="$VIS_TEST_HOME/.vis"
HOME="$VIS_TEST_HOME" target/release-bundle/vis-agent \
  -Duser.home="$VIS_TEST_HOME" --version
```

The staging helper renames `target/vis` to `vis-agent-native` and adds the
`vis-agent` launcher, build stamp, Python sidecar and installer. Ship these
files together. The terminal client is a separate release component; this
engine bundle is enough for gateway and SDK stdio use, not a complete TUI
installation.

`VIS_HOME` selects launcher installation state, so an installed `dev` track cannot
silently select the JVM. It does **not** relocate all engine state. The explicit
`-Duser.home` selects the engine's home; `HOME` matches it for child processes.

This test home is temporary. Prepare its provider configuration and credentials
before starting a gateway; use a persistent service-account home for deployment.
Choose an unused port and run:

```bash
unset VIS_GATEWAY_URL VIS_GATEWAY_TOKEN VIS_DB_PATH
HOME="$VIS_TEST_HOME" target/release-bundle/vis-agent \
  -Duser.home="$VIS_TEST_HOME" gateway start \
  --host 127.0.0.1 --port 7890 --require-token
```

Its default state is under `$VIS_TEST_HOME/.vis`. Separate state is not a security
sandbox: the process can still use allowed project files and inherited environment
variables. Do not replace or stop a shared gateway for this test.

For an owned Python job, pass the absolute staged launcher path as
`Agent(executable=...)` or `LocalEngine(executable=...)`. These use a temporary
session database; see the [Python SDK](python-sdk.md).

To use your new capability, connect an SDK client to this custom gateway or wrap
the staged launcher with Agent. The client still needs no native compilation;
only the engine containing your Java/Clojure code was rebuilt.

## Native-image configuration

The build uses AOT classes and the original dependency jars, not just a flattened
uberjar. GraalVM discovers each jar's
`META-INF/native-image/<group>/<artifact>/` directory at build time. Libraries
supply their own configuration:

- Reflection, resources and FFM downcalls are declared in
  `reachability-metadata.json`.
- Clojure-generated classes are initialized at build time, so no
  per-namespace `--initialize-at-build-time` flags are needed.
- The manifest's initialization vector is the native root set; `build.clj`
  derives entry points from it.
- Native libraries such as the tree-sitter language pack are reached through
  the Foreign Function and Memory API, enabled by the pack's own
  `-H:+ForeignAPISupport` and `-H:+SharedArenaSupport`.

The Python runtime files are copied into `target/vis-agent-python/` beside the
binary rather than included in it. CPython loads files from this directory.
Release bundles include it, and `bin/vis-agent` sets `VIS_PYTHON_NATIVE_PATH`
to its location.

## Regenerating reachability metadata

Run the relevant code under the tracing agent
(`-agentlib:native-image-agent=config-merge-dir=…`), then use the repository's
filter to remove Clojure-internal entries. `native_reachability_test` checks
the engine's metadata. Also run `clojure -M:test-native` against the built
binary; passing JVM tests do not verify native execution.

## Building behind a corporate TLS proxy

A fresh GraalVM trusts only public roots, so a network that intercepts TLS
fails with `SunCertPathBuilderException: unable to find valid certification
path to requested target`. Point the build at the extra root instead of
patching the JDK:

```bash
export VIS_CA_CERT=/etc/ssl/certs/corporate-ca.pem
eval "$(bin/require-graalvm --export)"
clojure -T:build native
```

`bin/require-graalvm` imports the PEM into a copy of the JDK's `cacerts` under
`${XDG_CACHE_HOME:-~/.cache}/vis`, passes it to `curl`, and exports it through
`JAVA_TOOL_OPTIONS` so every forked JVM and the `native-image` builder use it.
`bin/require-graalvm --truststore` prints the path. To use an existing
keystore, set `VIS_TRUSTSTORE=/path/store.p12` with `VIS_TRUSTSTORE_PASSWORD`
and `VIS_TRUSTSTORE_TYPE` (defaults `changeit` and `PKCS12`).

## See also

- [Extending Vis](extending.md) — add Python tools without rebuilding the engine.
- [Runtime distributions](distributions.md) — use a prebuilt runtime when you do not change JVM capabilities.
- [Running a gateway](gateway-service.md) — supervise and secure your custom engine.
- [Python SDK](python-sdk.md) — call your capability through an owned engine or gateway.
- [Java and Clojure SDK](jvm-sdk.md) — connect an external JVM application without rebuilding Vis.
