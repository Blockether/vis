# syntax=docker/dockerfile:1

# =============================================================================
# vis — everything in one container
# =============================================================================
# The gateway and the toolchain the agent drives from inside it. Nothing has to
# pre-exist on the host: no JDK, no clojure, no python, no chrome, no ASR model.
# `docker run` is the whole install.
#
# Stages:
#   jdk           GraalVM CE, at the version in .graalvm-version, shared by
#                 build and runtime.
#   builder       clojure CLI + `clojure -T:build native`, produces target/vis.
#                 The runtime image runs that binary, so every build compiles it.
#   native-export build-only: the release bundle as a bare filesystem, for
#                 `docker buildx --output type=local` cross-platform releases,
#                 and the exact layout the runtime stage installs.
#   model         the Parakeet ASR model, fetched once into its own cache layer.
#   runtime       that native runtime, the model, and the agent toolchain.
#
# Version pins are ARGs; bump them here. The GraalVM pin is the exception and
# lives in .graalvm-version.
#
#   docker build -t vis-gateway:local .
#   docker build -t vis-gateway:lean --build-arg WITH_CHROME=false .
#
# Extending this image
# `runtime` is the last stage, so `docker build .` produces it, and it is the
# base every deployment extends. It carries vis and the toolchain vis itself
# drives, nothing site-specific. A tool only your deployment needs (a GitHub
# CLI, a cloud CLI, an internal CA, a company apt repo, a credential helper)
# belongs in your own repository, not in this file — otherwise every user of vis
# pays download time and attack surface for it:
#
#   FROM vis-gateway:local
#   USER root
#   RUN apt-get update && apt-get install -y --no-install-recommends gh \
#       && rm -rf /var/lib/apt/lists/*
#   RUN mkdir -p /home/vis/.config/gh && chown -R vis:vis /home/vis/.config
#   USER vis
#
# A derived image may rely on: user `vis`, uid 10001, HOME=/home/vis,
# WORKDIR /work, the wrapper on PATH at /usr/local/bin/vis-agent, and the
# ENTRYPOINT/CMD at the bottom of this file (inherited unless overridden).
# Seed a dotfile directory the way this file does — `mkdir -p` then
# `chown vis:vis` — because docker seeds a named volume from the image's
# directory and inherits its owner and mode.
#
# Build cost: native-image takes roughly twenty minutes and a ~12 GiB live set
# in `builder`, on every build, because the gateway this image serves is that
# binary. Build where the RAM is, or give the builder its own limits with
# `--build-arg VIS_NATIVE_EXTRA_ARGS='-J-Xmx6g -J-Xms2g'`. The default is the
# lean interpreter build (VIS_ORACLE_NATIVE_IMAGE=false); the JIT variant
# (:oracle-native-image true) pulls in libpythonvm, which forces -Xms14g on
# the builder JVM — enable it only on a host with >=16 GB of free RAM.
# =============================================================================

# ── Version pins (global scope: re-declare `ARG x` inside a stage to use it) ──
# The GraalVM pin is not here: it lives in `.graalvm-version`, the one file the
# CI action, build.clj and bin/require-graalvm also read. The jdk stage copies
# and sources it, so this image cannot drift from what CI builds with.
ARG GRAAL_ARCH=x64
ARG CLOJURE_VERSION=1.12.5.1654
ARG MAVEN_VERSION=3.9.16
ARG MAVEN_SHA512=831a8591fe20c8243b1dbe7d71e3244f31d1665b0804b2e825e38cbbe5ce0cafb8338851f90780735568773e0a6cd07bbec107cda0b896b008b861075358b6f6
ARG PARAKEET_MODEL=sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8
ARG PARAKEET_RELEASE=asr-models

# The container ships the same public wrapper as every other distribution and
# runs the same native runtime a release publishes (see the runtime stage), with
# all channels and speech ASR. There is no leaner feature profile to select, so
# these two knobs only tune the native build the image runs.
ARG VIS_ORACLE_NATIVE_IMAGE=false
ARG VIS_NATIVE_EXTRA_ARGS=
ARG WITH_CHROME=true
# Exported native artifacts must run on Ubuntu 22.04 / glibc 2.35, even though
# the container runtime can use a newer distribution for its optional tools.
ARG BUILD_IMAGE=ubuntu:22.04
ARG BASE_IMAGE=debian:bookworm-slim

# ── Stage: jdk ───────────────────────────────────────────────────────────────
# GraalVM Community Edition, at the exact version pinned in `.graalvm-version`.
# Community, not Oracle, on purpose:
#   * CE is GPLv2 + Classpath Exception — the Classpath Exception frees the
#     binary we ship, so redistribution stays FOSS (audit/README.md §4.1 states
#     CE only). Oracle GraalVM is GFTC-licensed and was deliberately removed.
#   * CE's version is the Graal train version and matches `.graalvm-version`
#     as-is. Oracle's version is its JDK version, which is why this file used to
#     rewrite deps.edn on the way past — that hack is gone with it.
# The versioned graalvm-ce-builds asset is used deliberately over any moving
# URL: a moving URL cannot carry a checksum, and the checksum is in the pin.
FROM ${BUILD_IMAGE} AS jdk
ARG GRAAL_ARCH
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
COPY .graalvm-version /etc/graalvm-version
RUN set -eux; \
    . /etc/graalvm-version; \
    eval "sha=\${GRAAL_SHA256_linux_${GRAAL_ARCH}}"; \
    file="graalvm-community-jdk-${GRAAL_ASSET_VERSION}_linux-${GRAAL_ARCH}_bin.tar.gz"; \
    url="https://github.com/graalvm/graalvm-ce-builds/releases/download/${GRAAL_TAG}/${file}"; \
    curl -fL --retry 3 --retry-delay 5 -o /tmp/graalvm.tar.gz "$url"; \
    echo "${sha}  /tmp/graalvm.tar.gz" | sha256sum -c -; \
    mkdir -p /opt/graalvm; \
    tar -xzf /tmp/graalvm.tar.gz -C /opt/graalvm --strip-components=1; \
    rm /tmp/graalvm.tar.gz; \
    got="$(/opt/graalvm/bin/java -XshowSettings:properties -version 2>&1 \
            | sed -n 's/^ *java\.vendor\.version = //p' | head -n1)"; \
    test "$got" = "${GRAAL_VENDOR_VERSION}"; \
    test -x /opt/graalvm/bin/native-image

# ── Stage: builder ───────────────────────────────────────────────────────────
FROM jdk AS builder
ARG CLOJURE_VERSION
ARG VIS_ORACLE_NATIVE_IMAGE
ARG VIS_NATIVE_EXTRA_ARGS

# HOME here decides where the build's caches land (~/.m2, ~/.gitconfig). It is
# /home/vis so the builder JVM's `user.home` is already the runtime user's home:
# native-image initializes Clojure namespaces at build time, and anything that
# captured a home path then would otherwise capture root's.
ENV HOME=/home/vis \
    GRAALVM_HOME=/opt/graalvm \
    JAVA_HOME=/opt/graalvm \
    PATH=/opt/graalvm/bin:/usr/local/bin:/usr/bin:/bin \
    VIS_ORACLE_NATIVE_IMAGE=${VIS_ORACLE_NATIVE_IMAGE} \
    VIS_NATIVE_EXTRA_ARGS=${VIS_NATIVE_EXTRA_ARGS}

# build-essential + zlib headers are native-image's C toolchain, not optional.
RUN apt-get update && apt-get install -y --no-install-recommends \
        git bash rlwrap build-essential binutils zlib1g-dev \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /home/vis

RUN set -eux; \
    curl -fL --retry 3 -o /tmp/linux-install.sh \
        "https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/linux-install.sh"; \
    bash /tmp/linux-install.sh; \
    rm /tmp/linux-install.sh; \
    clojure --version

WORKDIR /build

# Dependency cache first, so a source-only edit does not refetch the world.
COPY deps.edn build.clj VIS_VERSION ./
COPY packages/ ./packages/

RUN clojure -P -T:build || true

COPY . .

# The checkout is in place: verify .graalvm-version, its lock and .sdkmanrc
# agree before paying for a native build.
RUN bin/require-graalvm --check-pins

# Git dependencies can compile Java in :deps/prep-lib. Prepping the copied checkout
# before AOT is required on a clean builder (vis-python-runtime.HostFunction).
RUN clojure -X:deps prep

# `native` honours VIS_ORACLE_NATIVE_IMAGE / VIS_NATIVE_EXTRA_ARGS from the env.
#
# `-Duser.home=/home/vis` is for the build's own initialization: native-image
# runs static initializers while building, and the builder's getpwuid() home is
# root's whatever HOME says, so a value captured then folds to /home/vis only
# this way. Vis' own code never needs it — `config-dir` is a function on purpose
# (see its docstring in internal/config.clj), read per call, so the effective
# `~/.vis` comes from the wrapper's `-Duser.home=$HOME` at launch. The runtime
# stage proves the result instead of trusting it: it runs the binary and asserts
# it wrote ~vis/.vis, never /root.
#
# `vis/VERSION` — what `vis-agent --version` prints — is the repo-root
# VIS_VERSION, verbatim: that file is the only version source, and the build
# below refuses to ship a binary that reports anything else.
RUN VIS_NATIVE_EXTRA_ARGS="-Duser.home=/home/vis ${VIS_NATIVE_EXTRA_ARGS}" \
    clojure -T:build native \
    && test -x target/vis \
    && test -d target/vis-agent-python \
    && bin/verify-linux-abi target/vis target/vis-agent-python \
    && ./target/vis --version \
    && { [ "$(./target/vis --version | tr -d '[:space:]')" = "vis-agent$(tr -d '[:space:]' < VIS_VERSION)" ] \
         || { echo "native image does not report exactly VIS_VERSION=$(tr -d '[:space:]' < VIS_VERSION)" >&2; exit 1; }; }

# ── Stage: native-export ─────────────────────────────────────────────────────
# Not part of the runtime image: a build-only stage whose whole filesystem is
# the release bundle, so a machine with docker can produce another platform's
# asset without a GitHub runner:
#
#   docker buildx build --target native-export --platform linux/arm64 \
#     --build-arg GRAAL_ARCH=aarch64 --output type=local,dest=out .
#
# `bin/release-native` drives exactly that, which is how an Apple-silicon Mac
# builds the linux-arm64 asset natively, without qemu emulation. The layout
# matches what `bin/vis-agent update` unpacks.
FROM scratch AS native-export
COPY --from=builder /build/target/vis /vis-agent-native
COPY --from=builder /build/target/vis.build /vis-agent-native.build
COPY --from=builder /build/target/vis-agent-python /vis-agent-python
COPY --from=builder /build/bin/vis-agent /vis-agent
COPY --from=builder /build/bin/install-vis-agent /install-vis-agent

# ── Stage: model ─────────────────────────────────────────────────────────────
# The Parakeet ASR model, published on the k2-fsa/sherpa-onnx `asr-models`
# release rather than Hugging Face. It must stay the exact model
# src/com/blockether/vis/internal/speech/asr.clj resolves.
#
# Baked into the image as its own layer. The model is always distributed
# separately from the binary, so fetching it once here means every container
# shares one copy and reads it in place via VIS_PARAKEET_MODEL_DIR, instead of
# each one re-downloading 465 MB into its own volume on first use.
#
# The archive has a top-level directory; --strip-components=1 puts the four
# files asr.clj looks for (encoder/decoder/joiner .int8.onnx + tokens.txt)
# directly in the directory that env var names.
FROM ${BASE_IMAGE} AS model
ARG PARAKEET_MODEL
ARG PARAKEET_RELEASE
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl bzip2 tar \
    && rm -rf /var/lib/apt/lists/*
RUN set -eux; \
    url="https://github.com/k2-fsa/sherpa-onnx/releases/download/${PARAKEET_RELEASE}/${PARAKEET_MODEL}.tar.bz2"; \
    mkdir -p "/opt/vis/models/${PARAKEET_MODEL}"; \
    curl -fL --retry 3 --retry-delay 5 -o /tmp/parakeet.tar.bz2 "$url"; \
    tar -xjf /tmp/parakeet.tar.bz2 -C "/opt/vis/models/${PARAKEET_MODEL}" --strip-components=1; \
    rm /tmp/parakeet.tar.bz2; \
    for f in encoder.int8.onnx decoder.int8.onnx joiner.int8.onnx tokens.txt; do \
        test -s "/opt/vis/models/${PARAKEET_MODEL}/$f" || { echo "MISSING $f"; exit 1; }; \
    done; \
    du -sh "/opt/vis/models/${PARAKEET_MODEL}"

# ── Stage: runtime ───────────────────────────────────────────────────────────
FROM ${BASE_IMAGE} AS runtime
ARG CLOJURE_VERSION
ARG MAVEN_VERSION
ARG MAVEN_SHA512
ARG PARAKEET_MODEL
ARG WITH_CHROME

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8

# Three groups, and the reason each is here:
#  1. the native binary + embedded CPython dlopen these at startup
#     (zlib1g, libstdc++6); onnxruntime/sherpa additionally need libgomp1.
#  2. the agent's own toolbelt — git, ssh, curl, ripgrep, jq, unzip, less, procps.
#     openssh-client is listed explicitly: it is only a *Recommends* of git and
#     this install is --no-install-recommends, so without it the image has no
#     ssh and no ssh-keygen at all, and every git@github.com remote fails with
#     "ssh: not found".
#  3. voice: ffmpeg. The gateway transcribes uploaded audio (a container has no
#     capture device and needs none), and without ffmpeg it cannot convert
#     .oga/.opus to the WAV the ASR consumes; `vis-agent doctor` reports it as
#     missing.
#
# No `gh`, no cloud CLI, no operator-specific package: this is the base image,
# and site tooling is a layer in the deployment's own repository (see the
# header). The list below is what vis drives — its own git/ssh/ffmpeg/rg use and
# the language extensions that shell out to python, node, clojure and maven.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates zlib1g libstdc++6 libgomp1 \
        bash git openssh-client curl wget gnupg ripgrep jq unzip xz-utils less procps tini \
        ffmpeg \
        python3 python3-pip python3-venv python3-dev python-is-python3 pipx \
        nodejs npm \
        rlwrap build-essential \
    && rm -rf /var/lib/apt/lists/*

# github.com's SSH host keys, pinned into the system known_hosts at build time.
# A fresh container has an empty ~/.ssh, so the first `git fetch git@github.com:`
# would have nothing to verify against: with no tty it cannot answer the TOFU
# prompt and simply fails. Pinning here means ssh works on first boot without
# StrictHostKeyChecking=no ever being tempting. GitHub publishes the same keys
# at https://api.github.com/meta; re-check them there when a rotation is
# announced.
RUN set -eux; \
    mkdir -p /etc/ssh; \
    printf '%s\n' \
        'github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl' \
        'github.com ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEmKSENjQEezOmxkZMy7opKgwFB9nkt5YRrYMjNuG5N87uRgg6CLrbo5wAdT/y6v0mKV0U2w0WZ2YB/++Tpockg=' \
        'github.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCj7ndNxQowgcQnjshcLrqPEiiphnt+VTTvDP6mHBL9j1aNUkY4Ue1gvwnGLVlOhGeYrnZaMgRK6+PKCUXaDbC7qtbW8gIkhL7aGCsOr/C56SJMy/BCZfxd1nWzAOxSDPgVsmerOBYfNqltV9/hWCqBywINIR+5dIg6JTJ72pcEpEjcYgXkE2YEFXV1JHnsKgbLWNlhScqb2UmyRkQyytRLtL+38TGxkxCflmO+5Z8CSSNY7GidjMIZ7Q4zMjA2n1nGrlTDkzwDCsw+wqFPGQA179cnfGWOWRVruj16z6XyvxvjJwbz0wQZ75XK5tKSb7FNyeIEs4TT4jk+S4dhPeAUC5y+bDYirYgM4GC7uEnztnZyaVWQ7B381AK4Qdrwt51ZqExKbQpTUNn+EjqoTwvqNj4kqx5QUCI0ThS/YkOxJCXmPUWZbhjpCg56i+2aB6CmK2JGhn57K5mj0MNdBXA4/WnwH6XoPWJzK5Nyu2zB3nAZp+S5hpQs+p1vN1/wsjk=' \
        > /etc/ssh/ssh_known_hosts; \
    chmod 0644 /etc/ssh/ssh_known_hosts; \
    ssh -V

# Google Chrome. The keyring file is named `google-chrome` on purpose: the
# .deb's own postinst writes a source list under that name, and a differently
# named keyring makes apt list the repo twice and warn on every update.
RUN set -eux; \
    if [ "${WITH_CHROME}" = "true" ]; then \
        curl -fsSL https://dl.google.com/linux/linux_signing_key.pub \
            | gpg --dearmor -o /usr/share/keyrings/google-chrome.gpg; \
        echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-chrome.gpg] https://dl.google.com/linux/chrome/deb/ stable main" \
            > /etc/apt/sources.list.d/google-chrome.list; \
        apt-get update && apt-get install -y --no-install-recommends google-chrome-stable; \
        rm -rf /var/lib/apt/lists/*; \
        google-chrome-stable --version; \
    else \
        echo "WITH_CHROME=false — chrome skipped"; \
    fi
ENV CHROME_BIN=/usr/bin/google-chrome-stable

# JDK + clojure + maven: the Clojure extension shells out to `clojure`
# (`clojure -M:test`), which is unusable without a JDK on PATH.
COPY --from=jdk /opt/graalvm /opt/graalvm
ENV GRAALVM_HOME=/opt/graalvm \
    JAVA_HOME=/opt/graalvm \
    MAVEN_HOME=/opt/maven \
    PATH=/opt/graalvm/bin:/opt/maven/bin:/usr/local/bin:/usr/bin:/bin

RUN set -eux; \
    curl -fL --retry 3 -o /tmp/linux-install.sh \
        "https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/linux-install.sh"; \
    bash /tmp/linux-install.sh; \
    rm /tmp/linux-install.sh; \
    clojure --version

# Upstream tarball, not `apt install maven` — that drags default-jdk and a
# second, non-GraalVM JVM into the image.
RUN set -eux; \
    url="https://dlcdn.apache.org/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz"; \
    curl -fL --retry 3 -o /tmp/maven.tar.gz "$url"; \
    echo "${MAVEN_SHA512}  /tmp/maven.tar.gz" | sha512sum -c -; \
    mkdir -p /opt/maven; \
    tar -xzf /tmp/maven.tar.gz -C /opt/maven --strip-components=1; \
    rm /tmp/maven.tar.gz; \
    mvn -v

# ── Voice model ──
COPY --from=model /opt/vis/models /opt/vis/models
ENV VIS_PARAKEET_MODEL_DIR=/opt/vis/models/${PARAKEET_MODEL}

# ── Unprivileged user ──
# The gateway never runs as root, and neither does anything the agent spawns.
# /work is the default workspace mount point. Created before the agent bundle
# below, so the wrapper, the native runtime and its interpreter all belong to
# the user that runs them.
# `useradd` is called by absolute path because the PATH set above deliberately
# omits /usr/sbin, where the vis user has no business.
# .ssh and .config are created here, owned by vis: docker seeds a named volume
# from the image's directory and inherits its owner and mode. Mount a volume on
# a path the image does not have and it lands root-owned 0755 — ssh-keygen then
# cannot write, and ssh refuses a group-readable ~/.ssh.
RUN /usr/sbin/useradd --create-home --shell /bin/bash --uid 10001 vis \
    && mkdir -p /home/vis/.vis /home/vis/.ssh /home/vis/.config/git /work \
    && chmod 0700 /home/vis/.ssh \
    && chown -R vis:vis /home/vis /work

# ── Vis Agent: the native runtime this source builds ──
# The gateway process is `vis-agent-native` — the same binary a release
# publishes — installed in exactly the layout a release bundle unpacks into:
#
#   /opt/vis/agent/vis-agent             the public Bash wrapper
#   /opt/vis/agent/vis-agent-native      the runtime it execs
#   /opt/vis/agent/vis-agent-python/     the embedded CPython interpreter
#
# The wrapper finds the runtime and the interpreter beside itself, which is why
# the whole bundle is copied as one directory and only the wrapper is linked
# onto PATH. The agent's home is the `vis` user's: HOME=/home/vis, so the
# wrapper hands the runtime `-Duser.home=/home/vis` and every `~/.vis` path
# lands there.
#
# Because the container serves the artifact every user installs, a gap in
# `reachability-metadata.json` or a constant that native-image folded in at
# build time fails in this build rather than only in someone's release.
#
# The JDK, the Clojure CLI and Maven stay in this image, but they are the
# agent's toolchain for the projects it works on — nothing here runs Vis itself
# on them, and the image carries no Vis source at all.
COPY --from=native-export --chown=vis:vis / /opt/vis/agent/
RUN ln -sf /opt/vis/agent/vis-agent /usr/local/bin/vis-agent

USER vis
WORKDIR /work
# GIT_CONFIG_GLOBAL: ~/.gitconfig would sit in the container layer and vanish on
# every `compose up` recreate, taking user.name/user.email with it. Point git at
# the persisted .config volume instead; `git config --global` then writes there
# too, so the identity survives a rebuild.
ENV HOME=/home/vis \
    VIS_HOME=/home/vis/.vis \
    GIT_CONFIG_GLOBAL=/home/vis/.config/git/config

# Prove, at build time, that the assembled image is what it claims to be: the
# toolchain resolves, the runtime that will serve is the native one, its Python
# stdlib loads through the staged interpreter (without it every Python tool dies
# with "No module named 'ast'"), and the built-in speech runtime can see the
# model. Proving the engine binary itself works belongs to `test-native/`, which
# drives target/vis, not to one container packaging of it.
RUN set -eux; \
    java -version; clojure --version; mvn -v | head -1; \
    python3 --version; node --version; \
    ffmpeg -version | head -1; git --version; ssh -V; \
    vis-agent --version; \
    test "$(od -An -tx1 -N4 /opt/vis/agent/vis-agent-native | tr -d ' \n')" = '7f454c46'; \
    test -x /opt/vis/agent/vis-agent-native; \
    test -d /opt/vis/agent/vis-agent-python; \
    vis-agent python -c "import ast, json, os; print('py-ok')" | grep -qx 'py-ok'; \
    vis-agent speech models status; \
    test ! -e /root/.vis; \
    test -d /home/vis/.vis/logs; \
    test "$(stat -c '%U %a' /home/vis/.ssh)" = 'vis 700'; \
    test "$(stat -c '%U' /home/vis/.config)" = 'vis'

EXPOSE 7890

# A non-loopback bind makes a bearer token mandatory in server.clj start!, so
# --require-token is explicit but redundant. The token is auto-generated into
# --token-file on first boot (there is no token env var) — keep that file on
# the state volume or every restart invalidates every client:
#   docker exec vis-gateway cat /home/vis/.vis/gateway-token
# tini reaps the processes the agent spawns; without a real init, PID 1 is the
# gateway and every abandoned child becomes a zombie.
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["vis-agent", "gateway", "start", "--host", "0.0.0.0", "--port", "7890", \
     "--require-token", "--token-file", "/home/vis/.vis/gateway-token"]
