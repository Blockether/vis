# The Vis playground on AWS Lambda. One clean image for every function of the stack.
# There is no AWS Lambda base image and no Lambda adapter: the embedded Python of Vis is
# the Lambda custom runtime, and playground/runtime.py speaks the Lambda Runtime API.
# Build for linux/arm64.
#
# Each download has a hash. The base image has a digest. Each file that curl downloads
# has a SHA-256, each Python package has a hash and each extension has a Git commit.
# The digest also sets the Amazon Linux release, and dnf reads only the repository of
# that release. requirements/rpm.lock has the header SHA-256 of each installed RPM, and
# the build stops when one differs. After a new base digest, write the lock again:
#   rpm -qa --qf '%{NEVRA} %{SHA256HEADER}\n' | LC_ALL=C sort
# To move to another Vis version, change VIS_RELEASE, its two checksums and
# requirements/site.* together.
FROM public.ecr.aws/amazonlinux/amazonlinux:2023@sha256:5c6ed3b38335b2aa424a045f1e564c8c508409ead0ff3f8bf5e62c31b94082cb
ARG VIS_RELEASE=beta-0e5986a54b7c4beca9888cb6b7453d6862b03dff
ARG VIS_AGENT_SHA256=b8bbcc86951d8cff7dbdca44462ad89124492d485c9f6b9f5a8c75a5db930372
ARG VIS_WEB_SHA256=61ec56af4dc490d875a6356ffbf278f0d3adf6c1a51e501fcfaeb64cc86f3ae6
# git serves the repository functions and fetches the extensions. clang builds native
# Python packages. Python 3.13 is the interpreter of the project. The image holds its
# packages (music21, pytest and ruff), because a worker and the app have no internet.
COPY requirements/ /opt/requirements/
RUN dnf install -y git tar gzip findutils clang openssh-clients python3.13 python3.13-pip && dnf clean all \
 && rpm -qa --qf '%{NEVRA} %{SHA256HEADER}\n' | LC_ALL=C sort > /tmp/rpm.now \
 && ! grep -vxFf /opt/requirements/rpm.lock /tmp/rpm.now && ! grep -vxFf /tmp/rpm.now /opt/requirements/rpm.lock && rm /tmp/rpm.now \
 && python3.13 -m pip install --no-cache-dir --require-hashes -r /opt/requirements/project.txt && ln -s /usr/bin/python3.13 /usr/local/bin/python3 \
 && python3.13 -c "import music21; print('music21', music21.__version__)"
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
COPY vendor.sh /opt/vendor.sh
RUN sh /opt/vendor.sh /opt/playground/vendor && rm /opt/vendor.sh
COPY known_hosts /opt/playground/known_hosts
# Only `python -m playground.local` reads this copy: it is the first commit of its
# repository. A stack gets its project from GitHub.
COPY repo-seed/ /opt/playground/repo-seed/
COPY workflows/ /opt/playground/workflows/
COPY extension/ /opt/vis-extensions/
COPY web/ /opt/playground/web/
COPY playground/ /opt/playground/playground/
ENTRYPOINT ["/opt/vis/vis-agent-python/python/bin/python3", "-m", "playground"]
