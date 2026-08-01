FROM debian:bookworm-slim

ARG GRAALVM_URL=https://github.com/graalvm/graalvm-ce-builds/releases/download/graal-25.1.3/graalvm-community-jdk-25i1-25.0.3_linux-aarch64_bin.tar.gz
ARG CLOJURE_INSTALL_URL=https://download.clojure.org/install/linux-install-1.12.1.1550.sh

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        bash \
        ca-certificates \
        curl \
        gcc \
        git \
        libc6-dev \
        make \
        tar \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/graalvm \
    && curl -fsSL "$GRAALVM_URL" -o /tmp/graalvm.tar.gz \
    && tar -xzf /tmp/graalvm.tar.gz -C /opt/graalvm --strip-components=1 \
    && rm /tmp/graalvm.tar.gz

ENV JAVA_HOME=/opt/graalvm
ENV PATH=/opt/graalvm/bin:/usr/local/bin:$PATH

# Root login shells in debian:bookworm-slim reset PATH and can drop
# /opt/graalvm/bin. The Clojure CLI still finds Java through JAVA_HOME, but
# clojure.tools.build later execs `native-image` by name. Keep these launchers
# visible through /usr/local/bin for login-shell Docker invocations too.
RUN ln -sf /opt/graalvm/bin/java /usr/local/bin/java \
    && ln -sf /opt/graalvm/bin/native-image /usr/local/bin/native-image

RUN curl -fsSL "$CLOJURE_INSTALL_URL" -o /tmp/install-clojure.sh \
    && chmod +x /tmp/install-clojure.sh \
    && /tmp/install-clojure.sh \
    && rm /tmp/install-clojure.sh

RUN java -version && native-image --version && clojure -Sdescribe

WORKDIR /work
