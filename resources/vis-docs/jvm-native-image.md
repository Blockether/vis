# JVM & native-image

Vis's core is Clojure on the JVM, but it ships as a **GraalVM native-image** — a standalone binary with fast startup and no JVM dependency. `clojure -T:build native` (or `bin/vis native`) produces it.

## Config travels inside the image

Each jar on the classpath carries its own `META-INF/native-image/<group>/<artifact>/` directory, auto-discovered at build time. Vis's own args + reachability metadata live there; library jars (GraalPy, sqlite, jetty, …) contribute their own. No giant command line.

- **Unified `reachability-metadata.json`** — reflection, resources, and FFM downcalls in one file (the legacy `reflect-config.json` split is gone).
- **`InitClojureClasses`** — graal-build-time initializes every Clojure-generated class at build time, so there are no per-namespace `--initialize-at-build-time` flags, and runtime-reflection entries for `*__init` / `$fn__NNNN` classes are pure noise (a filter strips them after agent regeneration).
- **Don't duplicate a library's config** — GraalPy ships its heavy args (build-time init, a large build heap); Vis adds only app-level reflection and its own flags.

## FFM, not JNI

Native libraries are reached through the JDK Foreign Function & Memory API. The tree-sitter language pack, for example, loads its native parser via FFM downcalls — which native-image supports with `-H:+ForeignAPISupport` and `-H:+SharedArenaSupport`, both shipped in the pack's own config so they apply automatically.

## Reachability metadata is generated, then cleaned

Metadata is captured by the tracing agent (`-agentlib:native-image-agent`) and merged. Because merging accumulates, a deterministic filter removes the agent's Clojure-internal noise so the committed config stays lean and reviewable. See the contributor guide for the exact commands.
