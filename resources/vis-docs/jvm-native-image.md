# Native builds for Java and Clojure extensions

This guide shows how to add Java or Clojure code to the Vis engine and compile a GraalVM native
image that runs without a JVM. Your code becomes part of the engine build. Vis does not load a JAR
as a plugin.

## When to use

- **You add Java or Clojure code to the Vis engine.** First
  [add and test it on the JVM](#add-and-test-your-jvm-capability).
- **People must run your build without Java.** [Build and test the image](#build-and-test-the-image),
  then [package it](#package-and-run-your-build).
- **Code that works on the JVM fails in the native image.**
  [Regenerate the reachability metadata](#regenerate-the-reachability-metadata).
- **The build fails with a certificate error.**
  [Build behind a corporate TLS proxy](#build-behind-a-corporate-tls-proxy).

A gateway, the Python SDK, and a Python extension run on a [prebuilt runtime](distributions.md)
and need no native build. Java and Clojure programs can call the [HTTP API](http-api.md). For
Python tools, read [Extending Vis](extending.md).

## Prerequisites

- A source checkout, the Clojure CLI, and the build tools of your platform. Run each command from
  the repository root.
- GraalVM Community Edition at the version in `.graalvm-version`. `bin/require-graalvm` installs
  and selects it. The build rejects Oracle GraalVM and other versions before `native-image` starts.
- 16 GB of RAM or more for the builder JVM, plus room for the operating system, the dependency
  cache, and the build outputs. Do not build on a small production VPS. `VIS_NATIVE_BUILDER_HEAP`
  limits the builder heap, for example `12g`. A smaller heap does not replace memory.
- A build machine with the target operating system and architecture. A macOS binary does not run
  on Linux.

`vis-agent update --track dev` updates the managed JVM source. It compiles no native image.

## Add and test your JVM capability

1. Add your code to the matching domain under `src/com/blockether/vis/internal/`, and each library
   that it needs to `deps.edn`. A Java library must be on the build classpath. A JAR next to a
   built binary loads no new code.
2. Register the tool bindings through `com.blockether.vis.internal.extension.core`. A Clojure
   binding can call your Java API. Give each exported tool an explicit input and output contract,
   and an Activity presentation that a person can read.
3. For a module initializer, add its qualified registration symbol to the ordered
   `:initialization` vector of `resources/META-INF/vis/manifest.edn`, after its dependencies. This
   manifest registers the built-in modules and gives the native build its entry points. It does
   not load Python extensions.
4. Add tests under `test/` for registration and for real calls, failure cases included. Run them
   with `clojure -M:test --namespace your.test-ns`. Test the same calls under `test-native/` for
   the compiled engine.

Native compilation cannot find a class that code loads dynamically at run time. Check the
reflection, resource, and foreign-function needs of your library in
[Native-image configuration](#native-image-configuration). Build again after each change to JVM
code or dependencies. A change to a Python extension needs no build.

## Build and test the image

Select the pinned toolchain, then compile:

```bash
eval "$(bin/require-graalvm --export)"
clojure -T:build native
```

The build compiles Clojure ahead of time, then runs GraalVM Native Image. It takes a long time and
writes these files:

| Output | Purpose |
| --- | --- |
| `target/vis` | Compiled native engine |
| `target/vis.build` | Version, source revision, release track, and build time |
| `target/vis-agent-python/` | Required Python worker, native library, and interpreter files |
| `target/vis.jar` | Intermediate JVM build artifact, not a native release bundle |

Run the native suite against the new image:

```bash
clojure -M:test-native
```

Only these tests run the binary. Green JVM tests and a completed `native-image` run do not prove
that tools, HTTP, or Python work in the image. To test another image, set `VIS_NATIVE_BIN` to its
path and keep its `vis-agent-python/` directory next to it.

## Package and run your build

`bin/stage-release-bundle` replaces `target/release-bundle` and writes the archive that you name.
Keep unrelated files out of that directory. Stage the bundle and create a test home outside your
project:

```bash
bin/stage-release-bundle target/vis target/vis-agent-local.tar.gz
export VIS_TEST_HOME="$(mktemp -d)"
export VIS_HOME="$VIS_TEST_HOME/.vis"
HOME="$VIS_TEST_HOME" target/release-bundle/vis-agent \
  -Duser.home="$VIS_TEST_HOME" --version
```

The helper renames `target/vis` to `vis-agent-native`. It adds the `vis-agent` launcher, the build
stamp, the embedded Python runtime (`vis-agent-python/`), and the installer. Ship these files
together. The bundle is enough for a gateway and for SDK stdio use. The terminal client is a
separate release component, so the bundle is not a complete TUI installation.

`VIS_HOME` keeps the launcher away from your installed Vis. `-Duser.home` moves the state of the
engine, and `HOME` moves the state of child processes.

Add the provider configuration and the credentials to the test home before you start a gateway. For a deployment, use the permanent home of a service account. Do not replace or
stop a shared gateway for this test. Choose an unused port and run:

```bash
unset VIS_GATEWAY_URL VIS_GATEWAY_TOKEN VIS_DB_PATH
HOME="$VIS_TEST_HOME" target/release-bundle/vis-agent \
  -Duser.home="$VIS_TEST_HOME" gateway start \
  --host 127.0.0.1 --port 7890 --require-token
```

The gateway keeps its state under `$VIS_TEST_HOME/.vis`. Separate state is not a security sandbox.
The process can still use the permitted project files and its inherited environment variables.

To use the new capability, connect an SDK client to this gateway. The client needs no native
compilation. For a Python job that owns its engine, pass the absolute path of the staged launcher
as `Agent(executable=...)` or `LocalEngine(executable=...)`. Both use a temporary session
database. [Python SDK](python-sdk.md) has the details.

## Native-image configuration

The build uses the AOT classes and the original dependency jars. A flattened uberjar is not its
only input. At build time, GraalVM finds the `META-INF/native-image/<group>/<artifact>/` directory
of each jar. Each library supplies its own configuration:

- `reachability-metadata.json` declares reflection, resources, and FFM downcalls.
- The build initializes Clojure-generated classes at build time. You need no
  `--initialize-at-build-time` flag for each namespace.
- The initialization vector of the manifest is the root set of the native build. `build.clj`
  derives the entry points from it.
- Native libraries such as fff and imaging run through the Foreign Function and Memory API. Each
  binding jar enables that API with its own `-H:+ForeignAPISupport` and `--enable-native-access`.

The Python runtime files are not in the binary. The build copies them to
`target/vis-agent-python/`, and CPython loads them from there. Release bundles include the
directory, and `bin/vis-agent` sets `VIS_PYTHON_NATIVE_PATH` to its location.

## Regenerate the reachability metadata

1. Run the code under the tracing agent (`-agentlib:native-image-agent=config-merge-dir=…`).
2. Remove the Clojure-internal entries with the filter of the repository.
3. Run `clojure -M:test-native` against the built binary.

`native_reachability_test` checks the metadata of the engine.

## Build behind a corporate TLS proxy

A new GraalVM installation trusts only public root certificates. On a network that intercepts TLS,
the build fails with
`SunCertPathBuilderException: unable to find valid certification path to requested target`. Do not
patch the JDK. Give the build the extra root certificate:

```bash
export VIS_CA_CERT=/etc/ssl/certs/corporate-ca.pem
eval "$(bin/require-graalvm --export)"
clojure -T:build native
```

`bin/require-graalvm` imports the PEM file into a copy of the JDK `cacerts` file under
`${XDG_CACHE_HOME:-~/.cache}/vis`. It gives that copy to `curl`. It also exports the copy through
`JAVA_TOOL_OPTIONS`, so each forked JVM and the `native-image` builder use it.
`bin/require-graalvm --truststore` prints the path of the copy. To use an existing keystore, set
`VIS_TRUSTSTORE=/path/store.p12` with `VIS_TRUSTSTORE_PASSWORD` and `VIS_TRUSTSTORE_TYPE`. Their
defaults are `changeit` and `PKCS12`.

## See also

- To add Python tools without an engine build, read [Extending Vis](extending.md).
- To use a prebuilt runtime when you do not change JVM code, read
  [Runtime distributions](distributions.md).
- To supervise and secure your custom engine, read [Running a gateway](gateway-service.md).
- To call your capability through an owned engine or a gateway, read [Python SDK](python-sdk.md).
