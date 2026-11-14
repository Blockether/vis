# The Vis playground on AWS Lambda. One clean image for every function of the stack.
# There is no AWS base image and no Lambda adapter: the embedded Python of Vis is the
# Lambda custom runtime, and playground/runtime.py speaks the Lambda Runtime API.
# Build for linux/arm64.
#
# Each download has a pin: the base image by digest, each file by SHA-256 and each
# Python package by hash. To move to another release, change the pins together.
FROM public.ecr.aws/amazonlinux/amazonlinux:2023@sha256:5c6ed3b38335b2aa424a045f1e564c8c508409ead0ff3f8bf5e62c31b94082cb
ARG VIS_RELEASE=v0.5.17
ARG VIS_AGENT_SHA256=f5f1e53945c9e23d24af75738b9e81126310547005a3fe102d6b570f1cdf9add
ARG VIS_WEB_SHA256=275c6f9b00990595f0a6e39d16f32afd007bea17f94e0ca6c23f3952150a414d
ARG CLOJURE_VERSION=1.12.5.1654
ARG CLOJURE_SHA256=28f81b0833c0a072f4370ae0eb1e4c5a4f9f4a34035cd7607ea9f253a8b06da1
# git serves the repository functions and fetches the extensions. clang builds native
# Python packages. The clj extension runs the Clojure CLI on a JDK. Python 3.13 with
# pytest and ruff is the interpreter of the project, because a worker has no internet.
COPY requirements/ /opt/requirements/
RUN dnf install -y git tar gzip findutils clang openssh-clients java-21-amazon-corretto-headless python3.13 python3.13-pip && dnf clean all \
 && python3.13 -m pip install --no-cache-dir --require-hashes -r /opt/requirements/project.txt && ln -s /usr/bin/python3.13 /usr/local/bin/python3 \
 && curl -fsSL -o /tmp/clojure.sh https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/linux-install.sh \
 && echo "${CLOJURE_SHA256}  /tmp/clojure.sh" | sha256sum -c - && bash /tmp/clojure.sh && rm /tmp/clojure.sh \
 && clojure --version
RUN base=https://github.com/Blockether/vis/releases/download/${VIS_RELEASE} \
 && curl -fsSL -o /tmp/agent.tar.gz "$base/vis-agent-linux-arm64.tar.gz" && curl -fsSL -o /tmp/web.tar.gz "$base/vis-web.tar.gz" \
 && printf '%s  %s\n' "${VIS_AGENT_SHA256}" /tmp/agent.tar.gz "${VIS_WEB_SHA256}" /tmp/web.tar.gz | sha256sum -c - \
 && mkdir -p /opt/vis && tar xzf /tmp/agent.tar.gz -C /opt/vis && tar xzf /tmp/web.tar.gz -C /opt && rm /tmp/agent.tar.gz /tmp/web.tar.gz \
 && /opt/vis/vis-agent --version
# Lambda can write only /tmp and runs as a user without root. Install the extensions,
# the Vis SDK and their bytecode in the image, and let every user read them.
ENV VIS_EXTENSIONS_DIR=/opt/vis-extensions VIS_PYTHON_PYCACHE_PREFIX=/opt/vis-pycache \
    VIS_PYTHON=/opt/vis/vis-agent-python/python/bin/python3 VIS_WEB_DIR=/opt/vis-web
COPY extensions.yml /opt/vis-build/.vis/config.yml
# The Vis SDK is the `vis-agent` package of the same version as the engine.
RUN HOME=/opt/vis-build /opt/vis/vis-agent extension sync --global --trust \
 && version=$(cut -d' ' -f1 /opt/vis/vis-agent-native.build) \
 && grep -q "^vis-agent==$version " /opt/requirements/site.txt \
 && $VIS_PYTHON -m pip install --no-cache-dir --require-hashes --target /opt/playground/site -r /opt/requirements/site.txt \
 && { $VIS_PYTHON -I -X pycache_prefix="$VIS_PYTHON_PYCACHE_PREFIX" \
    -m compileall -q -j 0 -x '/python3\.[0-9]+/(test|idlelib|tkinter|turtledemo)/' /opt/vis/vis-agent-python/python/lib "$VIS_EXTENSIONS_DIR" /opt/playground/site >/dev/null || true; } \
 && ln -s /tmp/pycache "$VIS_PYTHON_PYCACHE_PREFIX/tmp" \
 && chmod -R a+rX "$VIS_EXTENSIONS_DIR" "$VIS_PYTHON_PYCACHE_PREFIX" /opt/playground \
 && rm -rf /opt/vis-build
ENV PYTHONPATH=/opt/playground:/opt/playground/site \
    PYTHONUNBUFFERED=1 PYTHONDONTWRITEBYTECODE=1
RUN $VIS_PYTHON -c "import boto3; from blockether.vis.engine import Agent, GatewayClient; print('sdk ok')"
# A worker has no internet, so the image holds each Maven artifact that the clj
# extension and the project need. The user configuration names that repository.
ENV MAVEN_LOCAL_REPO=/opt/m2 CLJ_CONFIG=/opt/clojure
COPY repo-seed/deps.edn /opt/vis-build/project/deps.edn
RUN mkdir -p /opt/clojure /opt/vis-build/empty && echo '{:mvn/local-repo "/opt/m2"}' > /opt/clojure/deps.edn \
 && library=$(sed -n 's/^LIBRARY_VERSION = "\(.*\)"/\1/p' "$(find "$VIS_EXTENSIONS_DIR" -path '*vis_lang_clojure/bridge.py' | head -n 1)") \
 && (cd /opt/vis-build/empty && clojure -Sdeps "{:deps {com.blockether/vis-lang-clojure {:mvn/version \"$library\"}}}" -P) \
 && (cd /opt/vis-build/project && clojure -P && clojure -A:test -P) \
 && chmod -R a+rX /opt/m2 /opt/clojure && rm -rf /opt/vis-build
COPY vendor.sh /opt/vendor.sh
RUN sh /opt/vendor.sh /opt/playground/vendor && rm /opt/vendor.sh
COPY known_hosts /opt/playground/known_hosts
COPY workflows/ /opt/playground/workflows/
COPY extension/ /opt/vis-extensions/
COPY web/ /opt/playground/web/
COPY playground/ /opt/playground/playground/
ENTRYPOINT ["/opt/vis/vis-agent-python/python/bin/python3", "-m", "playground"]
