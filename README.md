<p align="center">
  <img src="logo.png" alt="vis logo" width="240"/>
</p>

# Vis

Vis is a coding agent that writes Python into a sandboxed GraalPy runtime, keeps durable state outside the context window, and inspects/changes the host project through tools.

## Install

Canonical install: clone once, put the launcher on `PATH`, then let `vis` install the right runtime for this machine.

```bash
git clone https://github.com/Blockether/vis.git
cd vis
echo 'export PATH="'"$PWD"'/bin:$PATH"' >> ~/.zshrc   # bash/zsh; or symlink bin/vis
exec $SHELL
vis update latest
vis help
```

Windows `cmd.exe` uses the same repo and launcher:

```bat
git clone https://github.com/Blockether/vis.git
cd vis
setx PATH "%CD%\bin;%PATH%"
vis update latest
vis help
```

Prereqs:

- Daily/native install: Git plus `curl` (macOS/Linux) or PowerShell (Windows).
- JVM/source fallback or `vis update <git-sha>`: [Clojure CLI](https://clojure.org/guides/install_clojure) 1.12+ and a JRE/JDK.
- Building native locally: Oracle GraalVM or GraalVM CE 25+ with at least 16 GB RAM.

## What `vis` runs

`vis` is the stable command. It proxies to the best available distribution, in this order:

1. managed native binary from `vis update` (`$VIS_HOME/install`, default `~/.vis/install`)
2. repo native binary (`target/vis` or `target/vis.exe`)
3. repo JVM uberjar (`target/vis.jar`)
4. live source (`clojure -M:vis`)

Use `vis --jvm ...` to skip native and force the JVM path.

## Build / develop

```bash
vis native          # builds target/vis(.exe) and target/vis.jar
./bin/dev nrepl    # project nREPL
./verify.sh --quick
```

Native-image details: [docs/src/NATIVE_IMAGE.md](docs/src/NATIVE_IMAGE.md). Contributor rules: [AGENTS.md](AGENTS.md).
