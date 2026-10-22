# The Vis gateway on AWS Lambda: native Vis, the web app, the Lambda Web Adapter
# and the relay in vis_lambda.py. Build for linux/arm64.
FROM public.ecr.aws/lambda/python:3.13-arm64
# VIS_RELEASE is a release tag: v0.2.53, or a beta such as beta-<commit>.
ARG VIS_RELEASE=beta-317ff6130bb5a97928fab5a74225d812ace95bff
COPY --from=public.ecr.aws/awsguru/aws-lambda-adapter:1.1.0 /lambda-adapter /opt/extensions/lambda-adapter
RUN dnf install -y git tar gzip findutils && dnf clean all \
 && mkdir -p /opt/vis \
 && curl -fsSL https://github.com/Blockether/vis/releases/download/${VIS_RELEASE}/vis-agent-linux-arm64.tar.gz | tar xz -C /opt/vis \
 && curl -fsSL https://github.com/Blockether/vis/releases/download/${VIS_RELEASE}/vis-web.tar.gz | tar xz -C /opt
COPY vis_lambda.py login.html /opt/
ENV AWS_LWA_INVOKE_MODE=response_stream AWS_LWA_PORT=8080 AWS_LWA_READINESS_CHECK_PATH=/ \
    PYTHONUNBUFFERED=1
ENTRYPOINT ["python3", "/opt/vis_lambda.py"]
