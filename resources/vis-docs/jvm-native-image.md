# Building the native binary

The Vis engine is Clojure on the JVM. `clojure -T:build native` compiles it
with GraalVM into `vis-agent-native`, which releases ship beside the
`vis-agent` wrapper. This page covers what a contributor needs to build that
binary: the GraalVM pin, how configuration reaches the image, and building
behind a TLS-intercepting proxy. Installing a released binary needs none of
this; see [Runtime distributions](distributions.md).

## Prerequisites

- GraalVM Community Edition at the exact version in `.graalvm-version`.
  `bin/require-graalvm` installs and selects it; `eval "$(bin/require-graalvm
  --export)"` exports `JAVA_HOME`. Oracle GraalVM is rejected because it would
  relicense the shipped binary, and a different version fails the JDK gate
  before `native-image` starts.
- A source runtime. `vis-agent update --rebuild` runs the same build from an
  installed source checkout.

## How configuration reaches the image

Each jar carries its own `META-INF/native-image/<group>/<artifact>/`
directory, discovered automatically at build time, so there is no long command
line and no duplicated library configuration:

- Reflection, resources and FFM downcalls live in one
  `reachability-metadata.json` per jar.
- Clojure-generated classes are initialized at build time, so no
  per-namespace `--initialize-at-build-time` flags are needed.
- The manifest's initialization vector is the native root set; `build.clj`
  derives entry points from it.
- Native libraries such as the tree-sitter language pack are reached through
  the Foreign Function and Memory API, enabled by the pack's own
  `-H:+ForeignAPISupport` and `-H:+SharedArenaSupport`.

The Python interpreter is not embedded. The build stages it into
`target/vis-agent-python/` beside the binary, because the tree is tens of
megabytes and CPython opens its own files. A release bundle ships that
directory, and `bin/vis-agent` points `VIS_PYTHON_NATIVE_PATH` at it.

## Regenerating reachability metadata

Run the code paths under the tracing agent
(`-agentlib:native-image-agent=config-merge-dir=…`), then run the repository's
filter to strip Clojure-internal entries so the committed file stays
reviewable. `native_reachability_test` pins the engine's metadata. A green JVM
suite is not proof the binary runs: `clojure -M:test-native` exercises the
built image.

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
- [Clojure extensions](clojure-extensions.md#native-image-rules) — what an extension must avoid to stay AOT-safe.
- [Python sandbox](python-sandbox.md) — the interpreter staged beside the binary.
