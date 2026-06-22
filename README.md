<p align="center">
  <img src="logo.png" alt="vis logo" width="240"/>
</p>

# Vis

Vis is a from-the-ground-up coding agent inspired by [Recursive Language Models](https://arxiv.org/abs/2512.24601) (Zhang, Kraska & Khattab, 2025): instead of spending the context window on an ever-growing transcript, the model writes Python into a sandboxed [GraalPy](https://www.graalvm.org/python/) runtime, keeps state in named variables and SQLite, and treats context as an external environment it can inspect and change.

## Install

Clone the repo and put `bin/` on your `PATH`:

```bash
git clone https://github.com/blockether/vis.git
cd vis
echo 'export PATH="'"$PWD"'/bin:$PATH"' >> ~/.zshrc   # or symlink bin/vis onto your PATH
exec $SHELL
vis help
```

`vis` runs the agent from anywhere. It picks the fastest distribution it finds, in order:

1. **native binary** (`target/vis`) — instant startup, no JVM. Build it once (below).
2. **JVM uberjar** (`target/vis.jar`) — portable, needs a JRE.
3. **live source** (`clojure -M:vis`) — the dev path, used when nothing is built.

Pass `--jvm` to skip the native binary and use the JVM (`vis --jvm help`).

### Prerequisites

- [Clojure CLI](https://clojure.org/guides/install_clojure) 1.12+ (for building / live source)
- Oracle GraalVM or GraalVM CE **25+** (JDK 25). The native build also needs ≥16 GB RAM.

## Build a standalone binary

One command builds **both** the native binary and the JVM uberjar:

```bash
vis native          # -> target/vis (target/vis.exe on Windows) + target/vis.jar
```

Afterwards `vis …` transparently runs the native binary. The binary is
self-contained — copy `target/vis` anywhere and run it without the repo.

Cross-platform: the same command works on Linux, macOS, and Windows; CI builds
and smoke-tests it on all three (`.github/workflows/native.yml`). For how the
GraalVM reachability metadata is laid out and regenerated, see
[docs/NATIVE_IMAGE.md](docs/NATIVE_IMAGE.md).

## Develop

```bash
./bin/dev nrepl        # boots the project nREPL on :7888, writes .nrepl-port
./verify.sh --quick    # quick checks before committing
```

Project rules for contributors (and agents) live in [AGENTS.md](AGENTS.md).
