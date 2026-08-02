# JVM & native-image

Vis's core is Clojure on the JVM. `clojure -T:build native` compiles that core into a fast GraalVM native runtime, but the native executable is never the public distribution by itself: releases always place it beside the `vis-agent` Bash wrapper as `vis-agent-native`, and `vis-agent update --jvm --rebuild` is how you build one locally.

## Config travels inside the image

Each jar on the classpath carries its own `META-INF/native-image/<group>/<artifact>/` directory, auto-discovered at build time. Vis's own args + reachability metadata live there; library jars (GraalPy, sqlite, jetty, …) contribute their own. No giant command line.

- **Unified `reachability-metadata.json`** — reflection, resources, and FFM downcalls in one file (the legacy `reflect-config.json` split is gone).
- **`InitClojureClasses`** — graal-build-time initializes every Clojure-generated class at build time, so there are no per-namespace `--initialize-at-build-time` flags, and runtime-reflection entries for `*__init` / `$fn__NNNN` classes are pure noise (a filter strips them after agent regeneration).
- **Don't duplicate a library's config** — GraalPy ships its heavy args (build-time init, a large build heap); Vis adds only app-level reflection and its own flags.
- **Internal resources are baked into the runtime** — the image is built with `-H:+IncludeLanguageResources` (the default since Graal Languages 24.2, pinned explicitly in Vis's `native-image.properties`): GraalPy's Python stdlib and every Truffle internal resource live inside the private sidecar. No resource directory is shipped and `PythonHome` is never used. On first use the runtime unpacks versioned resources into a per-user cache (`~/.cache/org.graalvm.polyglot` by default); Vis falls back to a writable directory automatically. The root is configurable via `python.resource-cache` in `vis.yml` or `-Dpolyglot.engine.userResourceCache`. See [Configuration](configuration.md#graalpy-internal-resource-cache).

## FFM, not JNI

Native libraries are reached through the JDK Foreign Function & Memory API. The tree-sitter language pack, for example, loads its native parser via FFM downcalls — which native-image supports with `-H:+ForeignAPISupport` and `-H:+SharedArenaSupport`, both shipped in the pack's own config so they apply automatically.

## Reachability metadata is generated, then cleaned

Metadata is captured by the tracing agent (`-agentlib:native-image-agent`) and merged. Because merging accumulates, a deterministic filter removes the agent's Clojure-internal noise so the committed config stays lean and reviewable. See the contributor guide for the exact commands.
