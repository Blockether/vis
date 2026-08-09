# syntax=docker/dockerfile:1

# =============================================================================
# vis — everything in one container
# =============================================================================
# The gateway AND the toolchain the agent drives from inside it. Nothing is
# expected to pre-exist on the host: no JDK, no clojure, no python, no chrome,
# no ASR model. `docker run` is the whole install.
#
# Stages:
#   jdk      — GraalVM CE 25.1.3 (see .graalvm-version), shared by build+runtime.
#   builder  — clojure CLI + `clojure -T:build native`, produces target/vis.
#              The runtime image RUNS that binary, so every build compiles it.
#   native-export — build-only: the release bundle as a bare filesystem, for
#              `docker buildx --output type=local` cross-platform releases, and
#              the exact layout the runtime stage installs.
#   model    — the Parakeet ASR model, fetched once into its own cache layer.
#   browsers — spel (Playwright) + its browser bundles, in its own layer.
#   runtime  — that native runtime, the model, and the agent toolchain.
#
# Version pins are ARGs. These ARGs and this header are the only place a
# version is written down in this repo; bump them here.
#
#   docker build -t vis-gateway:local .
#   docker build -t vis-gateway:lean --build-arg WITH_BROWSERS=false \
#                                    --build-arg WITH_CHROME=false .
#
# EXTENDING THIS IMAGE
# `runtime` is the LAST stage, so `docker build .` produces it, and it is the
# BASE every deployment extends. It carries vis and the toolchain vis itself
# drives — nothing site-specific. A tool only YOUR deployment needs (a GitHub
# CLI, a cloud CLI, an internal CA, a company apt repo, a credential helper) is
# a layer in YOUR OWN repository, never a line in this file — otherwise every
# user of vis pays download time and attack surface for one operator's habits:
#
#   FROM vis-gateway:local
#   USER root
#   RUN apt-get update && apt-get install -y --no-install-recommends gh \
#       && rm -rf /var/lib/apt/lists/*
#   RUN mkdir -p /home/vis/.config/gh && chown -R vis:vis /home/vis/.config
#   USER vis
#
# What a derived image may rely on: user `vis`, uid 10001, HOME=/home/vis,
# WORKDIR /work, the wrapper on PATH at /usr/local/bin/vis-agent, and the
# ENTRYPOINT/CMD at the bottom of this file (inherited unless overridden).
# Seed a dotfile directory the way this file does — `mkdir -p` then
# `chown vis:vis` — because docker seeds a named volume from the image's
# directory and inherits its owner and mode.
#
# Build cost: native-image is the expensive part — roughly twenty minutes and a
# ~12 GiB live set in `builder`, and it is paid on every build because the
# gateway this image serves IS that binary. Build where the RAM is, or hand the
# builder its own limits with
# `--build-arg VIS_NATIVE_EXTRA_ARGS='-J-Xmx6g -J-Xms2g'`. The default is the
# LEAN interpreter build (VIS_ORACLE_NATIVE_IMAGE=false); the JIT variant
# (:oracle-native-image true) pulls in libpythonvm, which forces -Xms14g on
# the builder JVM — only enable it on a host with >=16 GB of FREE RAM.
# =============================================================================

# ── Version pins (global scope: re-declare `ARG x` inside a stage to use it) ──
# The GraalVM pin is NOT here: it lives in `.graalvm-version`, the one file the
# CI action, build.clj and bin/require-graalvm also read. The jdk stage copies
# and sources it, so this image can never drift from what CI builds with.
ARG GRAAL_ARCH=x64
ARG CLOJURE_VERSION=1.12.5.1654
ARG MAVEN_VERSION=3.9.16
ARG MAVEN_SHA512=831a8591fe20c8243b1dbe7d71e3244f31d1665b0804b2e825e38cbbe5ce0cafb8338851f90780735568773e0a6cd07bbec107cda0b896b008b861075358b6f6
ARG SPEL_VERSION=0.9.22
ARG SPEL_SHA256=9b1ec00c85823d3b42bbe5249a2ae3e7b96a7feed6e9a4f9817988d8a55d58e7
ARG PARAKEET_MODEL=sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8
ARG PARAKEET_RELEASE=asr-models

# The container ships the same public wrapper as every other distribution and
# runs the same native runtime a release publishes (see the runtime stage), with
# all channels and voice ASR. There is no leaner feature profile to select, so
# nothing to configure. These two knobs tune the native build the image runs.
ARG VIS_ORACLE_NATIVE_IMAGE=false
ARG VIS_NATIVE_EXTRA_ARGS=
ARG WITH_BROWSERS=true
ARG WITH_CHROME=true
ARG BASE_IMAGE=debian:bookworm-slim

# ── Stage: jdk ───────────────────────────────────────────────────────────────
# GraalVM COMMUNITY Edition, at the exact version pinned in `.graalvm-version`.
# Community, not Oracle, on purpose:
#   * CE is GPLv2 + Classpath Exception — the Classpath Exception frees the
#     binary we ship, so redistribution stays FOSS (audit/README.md §4.1 states
#     CE only). Oracle GraalVM is GFTC-licensed and was deliberately removed.
#   * CE's version IS the Graal/Truffle train (25.1.x), so it matches the
#     org.graalvm.* jars pinned in deps.edn as-is. Oracle's version is its JDK
#     version (25.0.x), which is why this file used to rewrite deps.edn on the
#     way past — that hack is gone with it.
# The versioned graalvm-ce-builds asset is used deliberately over any moving
# URL: a moving URL cannot carry a checksum, and the checksum is in the pin.
FROM ${BASE_IMAGE} AS jdk
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

# HOME here is only about where the BUILD's caches land (~/.m2, ~/.gitconfig).
# It is /home/vis so the builder JVM's `user.home` is already the runtime user's
# home: native-image initializes Clojure namespaces at BUILD time, and anything
# that captured a home path then would otherwise capture root's.
ENV HOME=/home/vis \
    GRAALVM_HOME=/opt/graalvm \
    JAVA_HOME=/opt/graalvm \
    PATH=/opt/graalvm/bin:/usr/local/bin:/usr/bin:/bin \
    VIS_ORACLE_NATIVE_IMAGE=${VIS_ORACLE_NATIVE_IMAGE} \
    VIS_NATIVE_EXTRA_ARGS=${VIS_NATIVE_EXTRA_ARGS}

# build-essential + zlib headers are native-image's C toolchain, not optional.
RUN apt-get update && apt-get install -y --no-install-recommends \
        git bash rlwrap build-essential zlib1g-dev \
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
COPY extensions/ ./extensions/

# The org.graalvm.* Maven artifacts must match the Graal compiler in the JDK
# running the build; mismatched, Truffle throws NullPointerException inside
# HotSpotTruffleRuntimeAccess.getCompilerVersion, minutes into the image build.
# With GraalVM CE the two agree by construction (CE's version IS the Graal
# train), so this is a CHECK, not the deps.edn rewrite it used to be — and it
# fails here, in seconds, instead of inside TruffleAPIFeature much later.
# Quoted heredoc: the body is literal, so the variables are expanded by the
# script at RUN time (from /etc/graalvm-version), not by the builder.
COPY --chmod=0755 <<'EOF' /usr/local/bin/check-graal-pins
#!/bin/sh
set -eu
. /etc/graalvm-version
grep -n 'org\.graalvm\.' deps.edn
# every org.graalvm pin must name the JDK this image builds with
all=$(grep -c 'org\.graalvm\.' deps.edn)
ok=$(grep -c "org\.graalvm\..*\"${GRAAL_VERSION}\"" deps.edn)
if [ "$all" != "$ok" ]; then
  echo "deps.edn org.graalvm.* pins must all be ${GRAAL_VERSION} (.graalvm-version)" >&2
  exit 1
fi
EOF

RUN check-graal-pins

RUN clojure -P -T:build || true

COPY . .

# `COPY . .` just put the repo's own deps.edn back — re-check it.
RUN check-graal-pins

# `native` honours VIS_ORACLE_NATIVE_IMAGE / VIS_NATIVE_EXTRA_ARGS from the env.
#
# `-Duser.home=/home/vis` keeps the BUILDER JVM's home equal to the runtime
# user's home. Vis' own code never needs it — `config-dir` is a FUNCTION on
# purpose (see its docstring in internal/config.clj), read per call, so the
# effective `~/.vis` comes from the wrapper's `-Duser.home=$HOME` at launch.
# The flag exists for the build's OWN initialization: native-image runs static
# initializers while building, and a value some initializer captured from the
# builder (whose getpwuid() home is root's, HOME notwithstanding) can only ever
# fold to /home/vis this way. The runtime stage then PROVES the home instead of
# trusting it: it runs the binary and asserts it wrote ~vis/.vis, never /root.
#
# `vis/VERSION` — what `vis-agent --version` prints — is the repo-root
# VIS_VERSION, verbatim: that file is the only version source, and the build
# below refuses to ship a binary that reports anything else.
RUN VIS_NATIVE_EXTRA_ARGS="-Duser.home=/home/vis ${VIS_NATIVE_EXTRA_ARGS}" \
    clojure -T:build native \
    && test -x target/vis \
    && test -d target/resources \
    && ./target/vis --version \
    && { [ "$(./target/vis --version | tr -d '[:space:]')" = "vis-agent$(tr -d '[:space:]' < VIS_VERSION)" ] \
         || { echo "native image does not report exactly VIS_VERSION=$(tr -d '[:space:]' < VIS_VERSION)" >&2; exit 1; }; }

# ── Stage: native-export ─────────────────────────────────────────────────────
# Not part of the runtime image: a BUILD-ONLY stage whose whole filesystem is the
# release bundle, so a machine with docker can produce another platform's asset
# without a GitHub runner:
#
#   docker buildx build --target native-export --platform linux/arm64 \
#     --build-arg GRAAL_ARCH=aarch64 --output type=local,dest=out .
#
# `bin/release-native` drives exactly that (it is how an Apple-silicon Mac builds
# the linux-arm64 asset natively, with no qemu emulation). Layout matches what
# `bin/vis-agent update` unpacks.
FROM scratch AS native-export
COPY --from=builder /build/target/vis /vis-agent-native
COPY --from=builder /build/target/resources /vis-agent-resources
COPY --from=builder /build/bin/vis-agent /vis-agent
COPY --from=builder /build/bin/install-vis-agent /install-vis-agent

# ── Stage: model ─────────────────────────────────────────────────────────────
# The Parakeet ASR model. Published on the k2-fsa/sherpa-onnx `asr-models`
# release — NOT Hugging Face — and it must stay the exact model
# extensions/common/vis-foundation-voice/asr.clj resolves.
#
# Baked into the image as its own layer. The model is ALWAYS distributed
# separately from the binary (nothing embeds it any more), so fetching it once
# here means every container shares one copy and reads it in place via
# VIS_PARAKEET_MODEL_DIR, instead of each one re-downloading 465 MB into its
# own volume on first use.
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

# ── Stage: browsers ──────────────────────────────────────────────────────────
# spel (Clojure Playwright CLI) plus its driver and browser bundles. Its own
# stage so the ~1.5 GB download is cached independently of the vis build and
# can be dropped wholesale with --build-arg WITH_BROWSERS=false.
FROM ${BASE_IMAGE} AS browsers
ARG SPEL_VERSION
ARG SPEL_SHA256
ARG WITH_BROWSERS
ENV DEBIAN_FRONTEND=noninteractive \
    SPEL_DRIVER_DIR=/opt/vis/spel/driver \
    PLAYWRIGHT_BROWSERS_PATH=/opt/vis/playwright
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
RUN set -eux; \
    mkdir -p /opt/vis/spel /opt/vis/playwright "$SPEL_DRIVER_DIR"; \
    url="https://github.com/Blockether/spel/releases/download/v${SPEL_VERSION}/spel-linux-amd64"; \
    curl -fL --retry 3 --retry-delay 5 -o /opt/vis/spel/spel "$url"; \
    echo "${SPEL_SHA256}  /opt/vis/spel/spel" | sha256sum -c -; \
    chmod 0755 /opt/vis/spel/spel; \
    /opt/vis/spel/spel version
# `spel install --with-deps` also apt-installs the shared libraries the browser
# bundles link against — which is why this runs as root and why the runtime
# stage repeats it for its own filesystem.
RUN set -eux; \
    if [ "${WITH_BROWSERS}" = "true" ]; then \
        apt-get update; \
        /opt/vis/spel/spel install --with-deps; \
        rm -rf /var/lib/apt/lists/*; \
        du -sh /opt/vis/playwright; \
    else \
        echo "WITH_BROWSERS=false — browser bundles skipped"; \
    fi

# ── Stage: runtime ───────────────────────────────────────────────────────────
FROM ${BASE_IMAGE} AS runtime
ARG CLOJURE_VERSION
ARG MAVEN_VERSION
ARG MAVEN_SHA512
ARG PARAKEET_MODEL
ARG WITH_BROWSERS
ARG WITH_CHROME

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8

# Three groups, and the reason each is here:
#  1. the native binary + embedded GraalPy dlopen these at startup
#     (zlib1g, libstdc++6); onnxruntime/sherpa additionally need libgomp1.
#  2. the agent's own toolbelt — git, ssh, curl, ripgrep, jq, unzip, less, procps.
#     openssh-client is listed EXPLICITLY: it is only a *Recommends* of git and
#     this install is --no-install-recommends, so without it the image has no
#     ssh and no ssh-keygen at all, and every git@github.com remote dies with
#     "ssh: not found" (measured on debian:bookworm-slim, not assumed).
#  3. voice: ffmpeg. The gateway TRANSCRIBES uploaded audio (there is no
#     capture device in a container, and it needs none), and without ffmpeg it
#     cannot convert .oga/.opus to the WAV the ASR consumes. `vis-agent doctor`
#     reports it as missing — so it ships.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates zlib1g libstdc++6 libgomp1 \
        bash git openssh-client curl wget gnupg ripgrep jq unzip xz-utils less procps tini \
        ffmpeg \
        python3 python3-pip python3-venv python3-dev python-is-python3 pipx \
        nodejs npm \
        rlwrap build-essential \
    && rm -rf /var/lib/apt/lists/*

# No `gh`, no cloud CLI, no operator-specific package: this is the base image,
# and site tooling is a layer in the deployment's own repository (see the
# header). The list above is what VIS drives — its own git/ssh/ffmpeg/rg use
# and the language packs that shell out to python, node, clojure and maven.
# github.com's SSH host keys, pinned into the SYSTEM known_hosts at build time.
# A fresh container has an empty ~/.ssh, so the first `git fetch git@github.com:`
# would have nothing to verify against: with no tty it cannot answer the TOFU
# prompt and simply fails. Pinning here means ssh works on first boot without
# StrictHostKeyChecking=no ever being tempting.
# GitHub publishes the same keys at https://api.github.com/meta; re-check them
# there when a rotation is announced.
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

# JDK + clojure + maven: the Clojure language pack shells out to `clojure`
# (`clojure -M:test`), which is unusable without a JDK on PATH — the exact
# breakage found on the host, where GraalVM was installed but `java` was on
# nobody's PATH.
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

# spel + Playwright. The shared caches live under /opt (root-owned, world
# readable) rather than in the vis user's home, so a wiped state volume does
# not cost a 1.5 GB re-download. Browsers are copied even when WITH_BROWSERS
# is false — the directory is then just the spel binary and an empty cache.
# The Playwright env has to be in force for the RUN below: `spel install`
# resolves its cache from PLAYWRIGHT_BROWSERS_PATH, so declaring it after the
# RUN makes spel re-download the ~500 MB of browsers into the default cache and
# throws away the copy we just made.
#
# The native image bakes its TrustStore at build time, which is why spel reads
# SPEL_CA_BUNDLE; the Playwright driver is a Node subprocess and reads
# NODE_EXTRA_CA_CERTS instead. Both are set, at the system bundle, so mounting
# an extra CA into /usr/local/share/ca-certificates and running
# update-ca-certificates is all an inspecting proxy needs.
ENV SPEL_DRIVER_DIR=/opt/vis/spel/driver \
    PLAYWRIGHT_BROWSERS_PATH=/opt/vis/playwright \
    SPEL_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt \
    NODE_EXTRA_CA_CERTS=/etc/ssl/certs/ca-certificates.crt \
    CHROME_BIN=/usr/bin/google-chrome-stable

COPY --from=browsers /opt/vis/spel /opt/vis/spel
COPY --from=browsers /opt/vis/playwright /opt/vis/playwright
RUN ln -sf /opt/vis/spel/spel /usr/local/bin/spel
# The browser bundles link against system libraries that live in the browsers
# stage's filesystem, not in this one. Re-run the dependency half here.
RUN set -eux; \
    if [ "${WITH_BROWSERS}" = "true" ]; then \
        apt-get update; \
        /opt/vis/spel/spel install --with-deps; \
        rm -rf /var/lib/apt/lists/*; \
    fi; \
    # `spel install` assembles the Node driver as root and leaves
    # <driver>/<platform> at 0700. The runtime user cannot traverse it, so
    # `driver-ready?` reads false and spel re-assembles into the root-owned
    # driver directory — an AccessDeniedException instead of a browser. These
    # caches are shared and read-only at runtime; make the tree traversable.
    chmod -R a+rX /opt/vis/spel /opt/vis/playwright; \
    spel version

# Prove the copied bundles are visible through the env set above.
RUN test "${WITH_BROWSERS}" != "true" || ls /opt/vis/playwright

# ── voice model ──
COPY --from=model /opt/vis/models /opt/vis/models
ENV VIS_PARAKEET_MODEL_DIR=/opt/vis/models/${PARAKEET_MODEL}

# ── Unprivileged user ──
# The gateway never runs as root, and neither does anything the agent spawns.
# /work is the default workspace mount point. Created BEFORE the agent bundle
# below, so the wrapper, the native runtime and its language resources all
# belong to the user that runs them.
# Absolute path on purpose: the PATH set above deliberately omits /usr/sbin
# (the vis user has no business there), so a bare `useradd` is "not found".
# .ssh and .config are created HERE, owned by vis: docker seeds a named
# volume from the image's directory and inherits its owner and mode. Mount a
# volume on a path the image does not have and it lands root-owned 0755 —
# ssh-keygen then cannot write, and ssh refuses a group-readable ~/.ssh.
RUN /usr/sbin/useradd --create-home --shell /bin/bash --uid 10001 vis \
    && mkdir -p /home/vis/.vis /home/vis/.ssh /home/vis/.config/git /work \
    && chmod 0700 /home/vis/.ssh \
    && chown -R vis:vis /home/vis /work

# The runtime user, not root, is what launches browsers: an unreadable driver
# is what turns `spel open` into an AccessDeniedException at 03:00.
RUN test "${WITH_BROWSERS}" != "true" \
    || su -s /bin/sh vis -c 'test -r /opt/vis/spel/driver/linux/package/cli.js \
                            && test -x /opt/vis/spel/driver/linux/node'

# ── Vis Agent: the native runtime this source builds ──
# The gateway process IS `vis-agent-native` — the same binary a release
# publishes — installed in exactly the layout `vis-agent update --native`
# unpacks a bundle into:
#
#   /opt/vis/agent/vis-agent             the public Bash wrapper
#   /opt/vis/agent/vis-agent-native      the runtime it execs
#   /opt/vis/agent/vis-agent-resources/  GraalPy/Truffle language resources
#
# The wrapper finds the runtime and the resources beside itself, which is why
# the whole bundle is copied as one directory and only the wrapper is linked
# onto PATH.
#
# This is what makes a deployment worth trusting: the container serves the
# artifact every user installs, so a gap in `reachability-metadata.json` or a
# constant that native-image folded in at BUILD time fails in this build,
# loudly, instead of only in someone's release. The agent's home is the `vis`
# user's: HOME=/home/vis, so the wrapper hands the runtime
# `-Duser.home=/home/vis` and every `~/.vis` path lands there.
#
# The JDK, the Clojure CLI and Maven stay in this image, but they are the
# AGENT's toolchain for the projects it works on — nothing here runs Vis itself
# on them, and the image carries no Vis source at all.
COPY --from=native-export --chown=vis:vis / /opt/vis/agent/
RUN ln -sf /opt/vis/agent/vis-agent /usr/local/bin/vis-agent

USER vis
WORKDIR /work
# GIT_CONFIG_GLOBAL: ~/.gitconfig would sit in the container layer and vanish on
# every `compose up` recreate, taking user.name/user.email with it. Point git at
# the persisted .config volume instead; `git config --global` then writes there
# too, so the identity survives a rebuild.
# VIS_RUNTIME=native: the wrapper execs /opt/vis/agent/vis-agent-native. It is
# also what `find_native` would decide on its own here, said out loud so
# `docker exec … vis-agent` can never drift.
ENV HOME=/home/vis \
    VIS_HOME=/home/vis/.vis \
    GIT_CONFIG_GLOBAL=/home/vis/.config/git/config \
    VIS_RUNTIME=native

# Prove, at build time, that the assembled image is what it claims to be: the
# toolchain resolves, the runtime that will serve is the native one, its Python
# stdlib loads THROUGH the staged language resources (without them every Python
# tool dies with "No module named 'ast'"), and the voice extension can actually
# SEE the model.
RUN set -eux; \
    java -version; clojure --version; mvn -v | head -1; \
    python3 --version; node --version; \
    ffmpeg -version | head -1; git --version; ssh -V; \
    vis-agent --version; \
    vis-agent runtime | grep -Eq '^Runtime: +native'; \
    test -x /opt/vis/agent/vis-agent-native; \
    test -d /opt/vis/agent/vis-agent-resources; \
    vis-agent python -c "import ast, json, os; print('py-ok')" | grep -qx 'py-ok'; \
    vis-agent extension voice models status; \
    test ! -e /root/.vis; \
    test -d /home/vis/.vis/logs; \
    test "$(stat -c '%U %a' /home/vis/.ssh)" = 'vis 700'; \
    test "$(stat -c '%U' /home/vis/.config)" = 'vis'

# Prove the image RUNS, not only that it assembled. These three are the parts
# native-image is most likely to have broken, and no unit test can reach them:
# they only exist once the binary is linked.
#
#   TUI     Lanterna drives a real terminal through JNI and reflection, and a
#           missing reachability entry does not fail the compile — it fails the
#           first frame. `script` hands the process a pty (a build has none),
#           `timeout` ends it, and exit 124 IS the assertion: 124 means the TUI
#           was still painting when the clock ran out. Any other status is a
#           TUI that died on the way up.
#   agent   The one-shot entrypoint boots the session store, the tool registry
#           and provider selection. With no credential anywhere it must stop at
#           exactly one place and SAY so; that sentence is proof the whole
#           agent path ran from the native binary.
#   zai     Provider extensions are compiled INTO the binary. A deployment can
#           configure `zai-coding-plan` from outside it (an API key in the
#           environment, a `providers:` entry in ~/.vis/config.yml) — but only
#           if the extension is in here, so this is where its absence is cheap.
#
# HOME is a throwaway: these runs must not leave a draft session, a log or a
# gateway registry entry on the image's own /home/vis.
RUN set -eux; \
    export TERM=xterm-256color; \
    proof_home=/tmp/vis-proof; mkdir -p "$proof_home"; \
    set +e; \
    HOME="$proof_home" script -qec 'timeout 25 vis-agent channels tui' /dev/null > /tmp/tui.log 2>&1; \
    tui_rc=$?; \
    set -e; \
    test "$tui_rc" -eq 124 \
      || { echo "the native TUI left early (exit $tui_rc)"; cat /tmp/tui.log; exit 1; }; \
    test -s /tmp/tui.log; \
    ! grep -qE 'ClassNotFoundException|NoClassDefFoundError|UnsatisfiedLinkError|NoSuchMethodError' /tmp/tui.log; \
    set +e; \
    HOME="$proof_home" vis-agent --db :memory --raw 'hello world' < /dev/null > /tmp/agent.log 2>&1; \
    agent_rc=$?; \
    set -e; \
    { test "$agent_rc" -ne 0 && grep -q 'needs an AI provider' /tmp/agent.log; } \
      || { echo "the one-shot agent did not stop at provider selection (exit $agent_rc)"; cat /tmp/agent.log; exit 1; }; \
    vis-agent providers list | grep -q 'zai-coding-plan'; \
    rm -rf "$proof_home" /tmp/tui.log /tmp/agent.log

EXPOSE 7890

# A non-loopback bind MAKES a bearer token mandatory in server.clj start!, so
# --require-token is explicit but redundant. The token is AUTO-GENERATED into
# --token-file on first boot (there is no token env var) — keep that file on
# the state volume or every restart invalidates every client.
#   docker exec vis-gateway cat /home/vis/.vis/gateway-token
# tini reaps the processes the agent spawns; without a real init, PID 1 is the
# gateway and every abandoned child becomes a zombie.
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["vis-agent", "gateway", "start", "--host", "0.0.0.0", "--port", "7890", \
     "--require-token", "--token-file", "/home/vis/.vis/gateway-token"]
