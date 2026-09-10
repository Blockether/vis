# Building the native binary

The Vis engine runs Clojure on the JVM. `clojure -T:build native` compiles it
with GraalVM into `vis-agent-native`. This page covers build prerequisites,
reachability metadata and TLS proxies. To install Vis without building it,
see [Runtime distributions](distributions.md).

## Prerequisites

- GraalVM Community Edition at the version in `.graalvm-version`.
  `bin/require-graalvm` installs and selects it. Run
  `eval "$(bin/require-graalvm --export)"` to set `JAVA_HOME`. The build rejects
  Oracle GraalVM and mismatched versions before starting `native-image`.
- A source checkout. Run `clojure -T:build native` there to build local changes.
  `vis-agent update --track dev` updates managed JVM source; it does not build native images.

## Native-image configuration

GraalVM discovers each jar's `META-INF/native-image/<group>/<artifact>/`
directory at build time. Libraries supply their own configuration:

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

- [Runtime distributions](distributions.md) — choosing and updating an installed runtime.
- [Python sandbox](python-sandbox.md) — the Python runtime included with the binary.
