# The Vis gateway on AWS Lambda: native Vis, the web app, the extensions of
# extensions.yml, the Lambda Web Adapter and the relay in vis_lambda.py.
# Build for linux/arm64.
FROM public.ecr.aws/lambda/python:3.13-arm64
# VIS_RELEASE is a release tag such as v0.2.55, or latest for the newest release.
# deploy.sh passes the newest tag, so a new release never reuses an old build layer.
ARG VIS_RELEASE=latest
ARG CLOJURE_VERSION=1.12.5.1654
COPY --from=public.ecr.aws/awsguru/aws-lambda-adapter:1.1.0 /lambda-adapter /opt/extensions/lambda-adapter
# git fetches the extensions, and clang builds their native Python packages.
# The clj extension runs the Clojure CLI on a JDK.
RUN dnf install -y git tar gzip findutils clang java-21-amazon-corretto-headless && dnf clean all \
 && curl -fsSL https://github.com/clojure/brew-install/releases/download/${CLOJURE_VERSION}/linux-install.sh | bash \
 && clojure --version
RUN base=https://github.com/Blockether/vis/releases/download/${VIS_RELEASE} \
 && if [ "${VIS_RELEASE}" = latest ]; then base=https://github.com/Blockether/vis/releases/latest/download; fi \
 && mkdir -p /opt/vis \
 && curl -fsSL "$base/vis-agent-linux-arm64.tar.gz" | tar xz -C /opt/vis \
 && curl -fsSL "$base/vis-web.tar.gz" | tar xz -C /opt \
 && /opt/vis/vis-agent --version
# Lambda can write only /tmp and runs as a user without root. Install the extensions and
# their Python environments in the image, and let every user read them.
ENV VIS_EXTENSIONS_DIR=/opt/vis-extensions
COPY extensions.yml /opt/vis-build/.vis/config.yml
RUN HOME=/opt/vis-build /opt/vis/vis-agent extension sync --global --trust \
 && chmod -R a+rX "$VIS_EXTENSIONS_DIR" \
 && rm -rf /opt/vis-build
COPY vis_lambda.py login.html /opt/
ENV AWS_LWA_INVOKE_MODE=response_stream AWS_LWA_PORT=8080 AWS_LWA_READINESS_CHECK_PATH=/ \
    PYTHONUNBUFFERED=1 UV_CACHE_DIR=/tmp/uv-cache
ENTRYPOINT ["python3", "/opt/vis_lambda.py"]
