# syntax=docker/dockerfile:1

# =============================================================================
# vis — everything in one container
# =============================================================================
# The gateway AND the toolchain the agent drives from inside it. Nothing is
# expected to pre-exist on the host: no JDK, no clojure, no python, no chrome,
# no ASR model. `docker run` is the whole install.
#
# Stages:
#   jdk      — Oracle GraalVM (JDK 25 LTS), shared by the build and the runtime.
#   builder  — clojure CLI + `clojure -T:build native`, produces target/vis.
#   model    — the Parakeet ASR model, fetched once into its own cache layer.
#   browsers — spel (Playwright) + its browser bundles, in its own layer.
#   runtime  — the binary, the model, and the agent toolchain.
#
# Version pins are ARGs and mirror
# ~/infrastructure/playbooks/vars/vis-toolchain-versions.yml — that file and
# this header are the only two places a version is written down. Bump both.
#
#   docker build -t vis-gateway:local .
#   docker build -t vis-gateway:lean --build-arg WITH_BROWSERS=false \
#                                    --build-arg WITH_CHROME=false .
#
# Build cost: native-image is CPU- and RAM-hungry. The default here is the
# LEAN interpreter build (VIS_ORACLE_NATIVE_IMAGE=false). The JIT variant
# (:oracle-native-image true) pulls in libpythonvm, which forces -Xms14g on
# the builder JVM — only enable it on a host with >=16 GB of FREE RAM.
# =============================================================================

# ── Version pins (global scope: re-declare `ARG x` inside a stage to use it) ──
ARG GRAAL_VERSION=25.0.4
ARG GRAAL_ARCH=x64
ARG GRAAL_SHA256=76007c309f821aaf435bce63162ea0395587fc77350801c81643fe7feea37276
ARG CLOJURE_VERSION=1.12.5.1654
ARG MAVEN_VERSION=3.9.16
ARG MAVEN_SHA512=831a8591fe20c8243b1dbe7d71e3244f31d1665b0804b2e825e38cbbe5ce0cafb8338851f90780735568773e0a6cd07bbec107cda0b896b008b861075358b6f6
ARG SPEL_VERSION=0.9.11
ARG SPEL_SHA256=5fc16873fdd879522fe75a7ada5aeb57e3310bc1927571c60d6b9b2578444059
ARG PARAKEET_MODEL=sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8
ARG PARAKEET_RELEASE=asr-models

# `:community` — the FULL agent: every channel AND voice ASR. NOT `:cross`,
# which drops com.blockether/vis-foundation-voice entirely: with that profile
# the Parakeet model below would be dead weight the binary can never load.
ARG VIS_PROFILE=community
ARG VIS_ORACLE_NATIVE_IMAGE=false
ARG VIS_NATIVE_EXTRA_ARGS=
ARG WITH_BROWSERS=true
ARG WITH_CHROME=true
ARG BASE_IMAGE=debian:bookworm-slim

# ── Stage: jdk ───────────────────────────────────────────────────────────────
# Oracle GraalVM, not graalvm-community. Same JDK base plus the optimizing
# native-image. Licensed under the GraalVM Free Terms and Conditions: free for
# production use AND redistribution, which is what makes baking it into a
# shipped image legitimate. Served from download.oracle.com with no login and
# no accept-licence cookie, so the fetch works unattended.
#   Checksum: curl -sSL https://download.oracle.com/graalvm/25/archive/graalvm-jdk-<v>_linux-<arch>_bin.tar.gz.sha256
# The versioned archive/ URL is used deliberately over latest/: a moving URL
# cannot carry a checksum.
FROM ${BASE_IMAGE} AS jdk
ARG GRAAL_VERSION
ARG GRAAL_ARCH
ARG GRAAL_SHA256
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
RUN set -eux; \
    url="https://download.oracle.com/graalvm/25/archive/graalvm-jdk-${GRAAL_VERSION}_linux-${GRAAL_ARCH}_bin.tar.gz"; \
    curl -fL --retry 3 --retry-delay 5 -o /tmp/graalvm.tar.gz "$url"; \
    echo "${GRAAL_SHA256}  /tmp/graalvm.tar.gz" | sha256sum -c -; \
    mkdir -p /opt/graalvm; \
    tar -xzf /tmp/graalvm.tar.gz -C /opt/graalvm --strip-components=1; \
    rm /tmp/graalvm.tar.gz; \
    /opt/graalvm/bin/java -version 2>&1 | grep -q 'Oracle GraalVM'

# ── Stage: builder ───────────────────────────────────────────────────────────
FROM jdk AS builder
ARG CLOJURE_VERSION
ARG GRAAL_VERSION
ARG VIS_PROFILE
ARG VIS_ORACLE_NATIVE_IMAGE
ARG VIS_NATIVE_EXTRA_ARGS

ENV GRAALVM_HOME=/opt/graalvm \
    JAVA_HOME=/opt/graalvm \
    PATH=/opt/graalvm/bin:/usr/local/bin:/usr/bin:/bin \
    VIS_ORACLE_NATIVE_IMAGE=${VIS_ORACLE_NATIVE_IMAGE} \
    VIS_NATIVE_EXTRA_ARGS=${VIS_NATIVE_EXTRA_ARGS}

# build-essential + zlib headers are native-image's C toolchain, not optional.
RUN apt-get update && apt-get install -y --no-install-recommends \
        git bash rlwrap build-essential zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN set -eux; \
    curl -fL --retry 3 -o /tmp/linux-install.sh \
        "https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/linux-install.sh"; \
    bash /tmp/linux-install.sh; \
    rm /tmp/linux-install.sh; \
    clojure --version

WORKDIR /build

# Dependency cache first, so a source-only edit does not refetch the world.
COPY deps.edn build.clj VERSION ./
COPY extensions/ ./extensions/

# The org.graalvm.* Maven artifacts must match the Graal compiler in the JDK
# running the build. deps.edn pins the COMMUNITY train (25.1.x); Oracle's
# version IS its JDK version (25.0.x). Mismatched, Truffle throws
# NullPointerException inside HotSpotTruffleRuntimeAccess.getCompilerVersion.
# Rewritten here, in the build context only — the repo is never modified.
# Factored into a script because it has to run TWICE: once before the dependency
# prefetch (so `clojure -P` warms the right jars) and once after `COPY . .`,
# which restores the pristine deps.edn over the rewritten one. Missing the
# second call is silent — the build simply resolves 25.1.x and dies minutes
# later inside TruffleAPIFeature with "Version check failed".
# Quoted heredoc: the body is literal, so ${GRAAL_VERSION} is expanded by the
# script at RUN time (build args are in the RUN environment), not by the builder.
COPY --chmod=0755 <<'EOF' /usr/local/bin/pin-graal
#!/bin/sh
set -eu
sed -i -E "s|(org\.graalvm\.[a-z]+/[a-z-]+ \{:mvn/version \")[^\"]+(\")|\1${GRAAL_VERSION}\2|" deps.edn
grep -n 'org\.graalvm\.' deps.edn
# every org.graalvm pin must now name the JDK this image builds with
all=$(grep -c 'org\.graalvm\.' deps.edn)
ok=$(grep -c "org\.graalvm\..*\"${GRAAL_VERSION}\"" deps.edn)
test "$all" = "$ok"
EOF

RUN pin-graal

RUN clojure -P -T:build || true

COPY . .

# `COPY . .` just put the repo's own deps.edn back. Re-pin.
RUN pin-graal

# `native` honours VIS_ORACLE_NATIVE_IMAGE / VIS_NATIVE_EXTRA_ARGS from the env.
RUN clojure -T:build native :profile ${VIS_PROFILE} \
    && test -x target/vis \
    && ./target/vis --version

# ── Stage: model ─────────────────────────────────────────────────────────────
# The Parakeet ASR model. Published on the k2-fsa/sherpa-onnx `asr-models`
# release — NOT Hugging Face — and it must stay the exact model
# extensions/common/vis-foundation-voice/asr.clj resolves.
#
# Baked into the image rather than embedded in the binary with
# `:with-assets true`: that flag also embeds it, but asr.clj then EXTRACTS it
# to ~/.vis/models on first run, so the same 465 MB is paid twice — once in the
# binary, once in the volume — and re-paid on every container recreate. As an
# image layer it is downloaded once, shared by every container, and read
# in place via VIS_PARAKEET_MODEL_DIR.
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
#  2. the agent's own toolbelt — git, curl, ripgrep, jq, unzip, less, procps.
#  3. voice: ffmpeg. The gateway TRANSCRIBES uploaded audio (there is no
#     capture device in a container, and it needs none), and without ffmpeg it
#     cannot convert .oga/.opus to the WAV the ASR consumes. `vis doctor`
#     reports it as missing — so it ships.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates zlib1g libstdc++6 libgomp1 \
        bash git curl wget gnupg ripgrep jq unzip xz-utils less procps tini \
        ffmpeg \
        python3 python3-pip python3-venv python3-dev python-is-python3 pipx \
        nodejs npm \
        rlwrap build-essential \
    && rm -rf /var/lib/apt/lists/*

# gh — from GitHub's own apt repo (Debian's archive does not carry it).
RUN set -eux; \
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
        -o /usr/share/keyrings/githubcli-archive-keyring.gpg; \
    chmod 0644 /usr/share/keyrings/githubcli-archive-keyring.gpg; \
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
        > /etc/apt/sources.list.d/github-cli.list; \
    apt-get update && apt-get install -y --no-install-recommends gh; \
    rm -rf /var/lib/apt/lists/*; \
    gh --version

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
    spel version

# Prove the copied bundles are visible through the env set above.
RUN test "${WITH_BROWSERS}" != "true" || ls /opt/vis/playwright

# ── voice model ──
COPY --from=model /opt/vis/models /opt/vis/models
ENV VIS_PARAKEET_MODEL_DIR=/opt/vis/models/${PARAKEET_MODEL}

# ── the binary ──
COPY --from=builder /build/target/vis /usr/local/bin/vis
RUN chmod 0755 /usr/local/bin/vis

# Unprivileged. The gateway never runs as root, and neither does anything the
# agent spawns. /work is the default workspace mount point.
RUN useradd --create-home --shell /bin/bash --uid 10001 vis \
    && mkdir -p /home/vis/.vis /work \
    && chown -R vis:vis /home/vis /work

USER vis
WORKDIR /work
ENV HOME=/home/vis \
    VIS_HOME=/home/vis/.vis

# Prove, at build time, that the assembled image is what it claims to be:
# the toolchain resolves, and the voice extension can actually SEE the model.
RUN set -eux; \
    java -version; clojure --version; mvn -v | head -1; \
    python3 --version; node --version; gh --version | head -1; \
    ffmpeg -version | head -1; git --version; \
    vis --version; \
    vis extension voice models status

EXPOSE 7890

# A non-loopback bind MAKES a bearer token mandatory in server.clj start!, so
# --require-token is explicit but redundant. The token is AUTO-GENERATED into
# --token-file on first boot (there is no token env var) — keep that file on
# the state volume or every restart invalidates every client.
#   docker exec vis-gateway cat /home/vis/.vis/gateway-token
# tini reaps the processes the agent spawns; without a real init, PID 1 is the
# gateway and every abandoned child becomes a zombie.
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["vis", "gateway", "start", "--host", "0.0.0.0", "--port", "7890", \
     "--require-token", "--token-file", "/home/vis/.vis/gateway-token"]
