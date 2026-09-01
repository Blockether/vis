# JVM & native-image

Vis's core is Clojure on the JVM. `clojure -T:build native` compiles that core into a fast GraalVM native runtime, but the native executable is never the public distribution by itself: releases always place it beside the `vis-agent` Bash wrapper as `vis-agent-native`, and `vis-agent update --rebuild` is how you build one locally from an installed source runtime.

## Config travels inside the image

Each jar on the classpath carries its own `META-INF/native-image/<group>/<artifact>/` directory, auto-discovered at build time. Vis's own args + reachability metadata live there; library jars (CPython, sqlite, jetty, …) contribute their own. No giant command line.

- **Unified `reachability-metadata.json`** — reflection, resources, and FFM downcalls in one file (the legacy `reflect-config.json` split is gone).
- **`InitClojureClasses`** — graal-build-time initializes every Clojure-generated class at build time, so there are no per-namespace `--initialize-at-build-time` flags, and runtime-reflection entries for `*__init` / `$fn__NNNN` classes are pure noise (a filter strips them after agent regeneration).
- **Don't duplicate a library's config** — each library ships its own args; Vis adds only app-level reflection and its own flags.
- **The Python interpreter travels *beside* the binary** — the embedded CPython is a cdylib plus a vendored interpreter tree, staged by `clojure -T:build native` (`stage-python-sidecar!`) into `target/vis-agent-python/` instead of being embedded as image resources.
  - **Why** — the tree is tens of megabytes; embedding it would push the builder's live set past what a 16 GB CI runner survives, and none of it is read through `io/resource` anyway — CPython opens its own files.
  - **In a release bundle** — the directory ships as `vis-agent-python/`, `bin/stage-release-bundle` refuses a bundle without it, and `bin/vis-agent` exports `VIS_PYTHON_NATIVE_PATH` pointing at the cdylib inside it (the interpreter home is the `python/` tree beside that file).
  - **On the JVM** — the same tree ships inside `com.blockether/vis-python-runtime-native-<platform>` and is resolved from the classpath, so a source checkout needs no extra step.

## FFM, not JNI

Native libraries are reached through the JDK Foreign Function & Memory API. The tree-sitter language pack, for example, loads its native parser via FFM downcalls — which native-image supports with `-H:+ForeignAPISupport` and `-H:+SharedArenaSupport`, both shipped in the pack's own config so they apply automatically.

## Reachability metadata is generated, then cleaned

Metadata is captured by the tracing agent (`-agentlib:native-image-agent`) and merged. Because merging accumulates, a deterministic filter removes the agent's Clojure-internal noise so the committed config stays lean and reviewable. See the contributor guide for the exact commands.

## Building behind a corporate TLS proxy

A freshly installed GraalVM trusts the public roots and nothing else, so on a network that intercepts TLS the build fails with `SunCertPathBuilderException: unable to find valid certification path to requested target` — dependency resolution, `native-image`, or the JDK download itself — even though the system JDK works, because its `cacerts` was patched by the corporate installer.

Point vis at the extra root instead of patching the JDK (a patched `cacerts` is silently lost on the next reinstall):

```bash
export VIS_CA_CERT=/etc/ssl/certs/corporate-ca.pem   # PEM bundle
eval "$(bin/require-graalvm --export)"               # JAVA_HOME + JAVA_TOOL_OPTIONS
clojure -T:build native
```

`bin/require-graalvm` is the single owner of that policy:

- `curl` gets `--cacert`, so the pinned JDK downloads.
- The PEM is imported into a **copy** of that JDK's `cacerts`, cached under `${XDG_CACHE_HOME:-~/.cache}/vis`, so the public roots keep working and the JDK is never modified. Run `bin/require-graalvm --truststore` to print the path.
- `--export` adds `-Djavax.net.ssl.trustStore*` to `JAVA_TOOL_OPTIONS` (which every forked JVM reads, unlike `JDK_JAVA_OPTIONS`) plus `CURL_CA_BUNDLE`/`SSL_CERT_FILE`.
- `build.clj` forwards the same store to the JDK re-exec and to the `native-image` builder, so one setting covers the whole build.

Already have a keystore? Use it verbatim with `VIS_TRUSTSTORE=/path/store.p12`, plus `VIS_TRUSTSTORE_PASSWORD` and `VIS_TRUSTSTORE_TYPE` (defaults: `changeit`, `PKCS12`).

The distribution is not selectable: vis builds on GraalVM **Community Edition** at the exact version in `.graalvm-version`. Oracle GraalVM would relicense the shipped binary under GFTC, and a different version is rejected by the repository's own JDK gate before `native-image` starts.

## See also

- [Runtime distributions](distributions.md) — which of the two runtimes you are actually running.
- [Configuration](configuration.md) — the config the image carries and the config it reads.
- [Extending Vis → Native image rules](extending.md#native-image-rules) — what a Clojure extension must avoid to stay AOT-safe.
