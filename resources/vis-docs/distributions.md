# Custom distributions

A native-image is platform-specific: the binary that runs on Apple Silicon is not the one that runs on Linux x86-64. Vis builds a **per-platform distribution** for each target and publishes them, so users get a single native binary for their machine — no JVM, no runtime download for the core.

## Per-platform matrix

The release builds across the platforms that matter:

| Platform | Target triple |
| --- | --- |
| Linux x86-64 | `x86_64-unknown-linux-gnu` |
| Linux ARM64 | `aarch64-unknown-linux-gnu` |
| macOS ARM64 | `aarch64-apple-darwin` |
| macOS x86-64 | `x86_64-apple-darwin` |

The Intel-mac artifact is **cross-built on the Apple-Silicon runner** (Xcode's SDK is universal) rather than waiting on a scarce Intel runner — fast and reliable.

## Native dependencies, split out

Heavy native dependencies follow the same per-platform model rather than bloating one fat artifact. The tree-sitter language pack, for instance, publishes a small main jar plus one native jar per platform (`…-native-macos-arm64`, `…-native-linux-x86_64`, …); a resolver selects the right one at runtime, or it's embedded for the native build. The principle: **one small portable artifact, plus exactly the native payload your platform needs.**

## Build once, verify everywhere

Every platform's binary is built and smoke-tested in CI before release. A distribution is never published unless its native build is green on its own platform — so a broken grammar or a missing reachability entry surfaces in CI, not on a user's machine.
