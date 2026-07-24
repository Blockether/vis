# syntax=docker/dockerfile:1

# =============================================================================
# vis gateway — native-image build for Dokku (general server)
# =============================================================================
# Two stages:
#   1. builder — GraalVM CE 25.1.3 (the EXACT JDK the org.graalvm.* 25.1.3 maven
#      pins in deps.edn require; Truffle refuses a mismatched JDK) + clojure CLI,
#      runs `clojure -T:build native` to produce the standalone `target/vis`.
#   2. runtime — debian slim, glibc-compatible with the builder, runs the binary
#      as the unprivileged `vis` user in `gateway start` mode.
#
# Build resources: native-image with the GraalPy JIT kept in the image
# (:oracle-native-image true) needs >=16 GB RAM (libpythonvm forces -Xms14g) and
# several minutes. The Dokku build host MUST have that headroom or the build OOMs.
# =============================================================================

# ── Stage 1: builder ─────────────────────────────────────────────────────────
FROM debian:bookworm-slim AS builder

# GraalVM CE 25.1.3 — release tag `graal-25.1.3`, asset id `25i1-25.0.3`
# (JDK 25.0.3 base). Kept in lockstep with .github/actions/setup-graalvm-25 and
# the deps.edn org.graalvm.* pins; bump all three together.
ARG GRAAL_TAG=graal-25.1.3
ARG GRAAL_ASSET=25i1-25.0.3
ARG CLOJURE_VERSION=1.12.1.1550

ENV DEBIAN_FRONTEND=noninteractive \
    GRAALVM_HOME=/opt/graalvm \
    JAVA_HOME=/opt/graalvm \
    PATH=/opt/graalvm/bin:/usr/local/bin:/usr/bin:/bin

# Build toolchain: curl+git for sources, build-essential/zlib for native-image's
# C linker, rlwrap for the clojure launcher, ca-certificates for TLS fetches.
RUN apt-get update && apt-get install -y --no-install-recommends \
        curl git bash rlwrap ca-certificates \
        build-essential zlib1g-dev libz-dev \
    && rm -rf /var/lib/apt/lists/*

# Install GraalVM CE 25.1.3 (linux-x64).
RUN set -eux; \
    file="graalvm-community-jdk-${GRAAL_ASSET}_linux-x64_bin.tar.gz"; \
    url="https://github.com/graalvm/graalvm-ce-builds/releases/download/${GRAAL_TAG}/${file}"; \
    curl -fL --retry 3 --retry-delay 5 -o /tmp/graalvm.tar.gz "$url"; \
    mkdir -p /opt/graalvm; \
    tar -xzf /tmp/graalvm.tar.gz -C /opt/graalvm --strip-components=1; \
    rm /tmp/graalvm.tar.gz; \
    java -version; native-image --version

# Install the Clojure CLI (tools.deps).
RUN set -eux; \
    curl -fL --retry 3 -o /tmp/linux-install.sh \
        "https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/linux-install.sh"; \
    bash /tmp/linux-install.sh; \
    rm /tmp/linux-install.sh; \
    clojure --version

WORKDIR /build

# Warm the dependency cache first (better layer reuse across source-only edits).
COPY deps.edn build.clj VERSION ./
COPY extensions/ ./extensions/
RUN clojure -P -T:build || true

# Full sources, then the native build.
COPY . .

# :profile :cross  — every channel (incl. the web channel the gateway serves),
#                    voice ASR dropped (no 465 MB model, no sherpa/onnx natives).
# :oracle-native-image true — keep the GraalPy JIT in the image ("native compiled
#                    on oracle"): bigger/slower build, faster CPU-bound Python.
RUN clojure -T:build native :profile :cross :oracle-native-image true \
    && test -x target/vis

# ── Stage 2: runtime ─────────────────────────────────────────────────────────
FROM debian:bookworm-slim AS runtime

ENV DEBIAN_FRONTEND=noninteractive \
    VIS_HOME=/home/vis/.vis

# Runtime shared libs the native binary + embedded GraalPy dlopen at startup.
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates zlib1g libstdc++6 \
    && rm -rf /var/lib/apt/lists/*

# Separate unprivileged user — the gateway never runs as root.
RUN useradd --create-home --shell /usr/sbin/nologin --uid 10001 vis

COPY --from=builder /build/target/vis /usr/local/bin/vis
RUN chmod 0755 /usr/local/bin/vis

USER vis
WORKDIR /home/vis

# The gateway HTTP/SSE port (server.clj DEFAULT_PORT 7890).
EXPOSE 7890

# A non-loopback bind (--host 0.0.0.0) MAKES a bearer token mandatory in
# server.clj start!, so --require-token is explicit but redundant here. The
# token is AUTO-GENERATED into --token-file on first boot (there is no token
# env var); point it at the Dokku persistent-storage mount so it survives
# redeploys. Read it back with `dokku run vis-gateway cat /home/vis/.vis/gateway-token`
# (or `vis gateway status`) to hand to the companion/web client.
CMD ["vis", "gateway", "start", "--host", "0.0.0.0", "--port", "7890", "--require-token", "--token-file", "/home/vis/.vis/gateway-token"]
