# The Vis gateway on AWS Lambda: native Vis, the web app, the extensions of
# extensions.yml, the Lambda Web Adapter and the relay in vis_lambda.py.
# Build for linux/arm64.
FROM public.ecr.aws/lambda/python:3.13-arm64
# VIS_RELEASE is a release tag such as v0.2.55, or latest for the newest release.
# deploy.sh passes the newest tag, so a new release never reuses an old build layer.
ARG VIS_RELEASE=latest
ARG CLOJURE_VERSION=1.12.5.1654
# The Parakeet speech-to-text model, from the Vis asset pack. Vis lists the URL and the
# checksum in resources/vis-models/manifest.edn.
ARG PARAKEET_URL=https://github.com/Blockether/vis/releases/download/assets-pack/parakeet-asr-onnx.tar.bz2
ARG PARAKEET_SHA256=5793d0fd397c5778d2cf2126994d58e9d56b1be7c04d13c7a15bb1b4eafb16bf
COPY --from=public.ecr.aws/awsguru/aws-lambda-adapter:1.1.0 /lambda-adapter /opt/extensions/lambda-adapter
# git fetches the extensions, and clang builds their native Python packages.
# The clj extension runs the Clojure CLI on a JDK.
RUN dnf install -y git tar gzip bzip2 findutils clang java-21-amazon-corretto-headless && dnf clean all \
 && curl -fsSL https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/linux-install.sh | bash \
 && clojure --version
RUN base=https://github.com/Blockether/vis/releases/download/${VIS_RELEASE} \
 && if [ "${VIS_RELEASE}" = latest ]; then base=https://github.com/Blockether/vis/releases/latest/download; fi \
 && mkdir -p /opt/vis \
 && curl -fsSL "$base/vis-agent-linux-arm64.tar.gz" | tar xz -C /opt/vis \
 && curl -fsSL "$base/vis-web.tar.gz" | tar xz -C /opt \
 && /opt/vis/vis-agent --version
# Vis finds the model in ~/.vis/models: vis_lambda.py links the folders of /opt/vis-models there.
RUN mkdir -p /opt/vis-models \
 && curl -fsSL -o /tmp/parakeet.tar.bz2 "$PARAKEET_URL" \
 && echo "$PARAKEET_SHA256  /tmp/parakeet.tar.bz2" | sha256sum -c - \
 && tar xjf /tmp/parakeet.tar.bz2 -C /opt/vis-models \
 && rm /tmp/parakeet.tar.bz2 \
 && chmod -R a+rX /opt/vis-models \
 && ls /opt/vis-models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8/encoder.int8.onnx
# Lambda can write only /tmp and runs as a user without root. Install the extensions and
# their Python environments in the image, and let every user read them.
# Python reads its bytecode from VIS_PYTHON_PYCACHE_PREFIX. Compile the standard library
# and the extensions there, so a cold start imports them without a compile. Code
# under /tmp gets its bytecode in /tmp/pycache through the tmp link.
ENV VIS_EXTENSIONS_DIR=/opt/vis-extensions VIS_PYTHON_PYCACHE_PREFIX=/opt/vis-pycache
COPY extensions.yml /opt/vis-build/.vis/config.yml
RUN HOME=/opt/vis-build /opt/vis/vis-agent extension sync --global --trust \
 && { /opt/vis/vis-agent-python/python/bin/python3 -I -X pycache_prefix="$VIS_PYTHON_PYCACHE_PREFIX" \
    -m compileall -q -j 0 -x '/python3\.[0-9]+/(test|idlelib|tkinter|turtledemo)/' /opt/vis/vis-agent-python/python/lib "$VIS_EXTENSIONS_DIR" >/dev/null || true; } \
 && ln -s /tmp/pycache "$VIS_PYTHON_PYCACHE_PREFIX/tmp" \
 && chmod -R a+rX "$VIS_EXTENSIONS_DIR" "$VIS_PYTHON_PYCACHE_PREFIX" \
 && rm -rf /opt/vis-build
COPY vis_lambda.py login.html /opt/
ENV AWS_LWA_INVOKE_MODE=response_stream AWS_LWA_PORT=8080 AWS_LWA_READINESS_CHECK_PATH=/ \
    PYTHONUNBUFFERED=1 UV_CACHE_DIR=/tmp/uv-cache
ENTRYPOINT ["python3", "/opt/vis_lambda.py"]
