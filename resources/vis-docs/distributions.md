# Runtime distributions

Vis Agent has one public distribution shape: a **Bash wrapper** named
`vis-agent`, plus whichever runtime that installation provides. Users never
install or invoke the native image as `vis`.

## The wrapper is the product boundary

The wrapper is responsible for:

- keeping one collision-free command on PATH (`vis-agent`);
- selecting native or live JVM source;
- persisting the default with `vis-agent runtime use native|jvm`;
- applying one-launch `--native` / `--jvm` overrides;
- preserving the invocation working directory; and
- producing a clear error when the selected runtime is unavailable.

`vis-agent runtime show` reports the configured default, effective runtime, and
the paths it discovered. There is no jar runtime. The AOT jar produced during a
native build is an implementation artifact only.

## Native release bundle

Native-image output is platform-specific, but it is a **private runtime
sidecar**, not a standalone Vis distribution. Each release archive is named:

```text
vis-agent-<os>-<arch>-community.tar.gz
```

and contains:

```text
vis-agent          # public Bash wrapper
vis-agent-native   # private GraalVM native-image runtime
install-native     # installer for the same bundle shape
```

The two executables stay next to each other. `bin/install-native` installs them
together, and `vis-agent update` replaces them together. This prevents the
launcher and runtime contract from drifting across versions.

Release CI currently builds and smoke-tests:

| Platform | Bundle |
|---|---|
| Linux x86-64 | `vis-agent-linux-x64-community.tar.gz` |
| Linux ARM64 | `vis-agent-linux-arm64-community.tar.gz` |

macOS users use the JVM source distribution unless a maintainer provides a
matching locally built sidecar. Native builds require GraalVM Community Edition
25.1.3 exactly; the repository pin is authoritative.

## JVM source distribution

`bin/install-source` clones or updates a source checkout, copies the same wrapper
onto PATH, and records the checkout in `~/.vis/source-dir`. The runtime is
`clojure -M:vis` from that checkout. No jar is copied or selected.

A source installation and a native sidecar can coexist. Switching the persisted
runtime does not reinstall anything:

```bash
vis-agent runtime use jvm
vis-agent runtime use native
```
