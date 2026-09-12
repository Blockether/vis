# Building the native binary

Build a native runtime when you want to ship your own Vis changes without
requiring a JVM on the machine that runs them. This guide takes you from a
source checkout to a tested engine bundle for a gateway or SDK wrapper. If you
just want to use Vis, install a [prebuilt release](distributions.md) instead.

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

A separate Java application can connect to this gateway without itself being
compiled to native code. If you embed Vis in a custom Java image, you also own
its AOT setup, reachability metadata and native execution tests. See
[Java and Clojure SDK](jvm-sdk.md#package-a-jvm-application-or-a-native-runtime).

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

- [Runtime distributions](distributions.md) — choose and update an installed runtime.
- [Running a gateway](gateway-service.md) — supervise and secure the engine you ship.
- [Python SDK](python-sdk.md) — wrap the built engine in a Python application.
- [Java and Clojure SDK](jvm-sdk.md) — connect a JVM application or plan an embedding integration.
- [Python sandbox](python-sandbox.md) — understand the Python runtime included with the binary.
