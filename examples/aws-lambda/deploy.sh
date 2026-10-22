#!/usr/bin/env bash
# Build the image, push it to ECR and deploy the vis-lambda stack.
# Usage: AWS_PROFILE=vis-deployer [VIS_RELEASE=<release tag>] [VIS_WEB_DIR=<web build>] ./deploy.sh
# Without VIS_RELEASE, the script uses the newest Vis release.
# VIS_WEB_DIR replaces the release web app with a local `npm run build:web` output.
set -euo pipefail
cd "$(dirname "$0")"
region="${AWS_REGION:-$(aws configure get region)}"
cli="${CONTAINER_CLI:-podman}"
account="$(aws sts get-caller-identity --query Account --output text)"
registry="$account.dkr.ecr.$region.amazonaws.com"
image="$registry/vis-lambda:$(date +%Y%m%d%H%M%S)"
release="${VIS_RELEASE:-$(curl -fsSL https://api.github.com/repos/Blockether/vis/releases/latest |
  python3 -c 'import json, sys; print(json.load(sys.stdin)["tag_name"])')}"
echo "deploy.sh: Vis $release"

aws ecr describe-repositories --repository-names vis-lambda >/dev/null 2>&1 ||
  aws ecr create-repository --repository-name vis-lambda >/dev/null
aws ecr get-login-password | "$cli" login --username AWS --password-stdin "$registry"
"$cli" build --platform linux/arm64 --build-arg VIS_RELEASE="$release" -t "$image" .
if [[ -n "${VIS_WEB_DIR:-}" ]]; then
  [[ -f "$VIS_WEB_DIR/index.html" ]] || { echo "deploy.sh: no index.html in $VIS_WEB_DIR" >&2; exit 1; }
  printf 'FROM %s\nRUN rm -rf /opt/vis-web\nCOPY . /opt/vis-web/\n' "$image" |
    "$cli" build --platform linux/arm64 -t "$image" -f - "$VIS_WEB_DIR"
fi
if [[ "$cli" == podman ]]; then podman push --format v2s2 "$image"; else docker push "$image"; fi

aws ssm get-parameter --name /vis/token >/dev/null 2>&1 ||
  aws ssm put-parameter --name /vis/token --type SecureString \
    --value "$(python3 -c 'import uuid; print(uuid.uuid4())')" >/dev/null

aws cloudformation deploy --stack-name vis-lambda --template-file template.yaml \
  --capabilities CAPABILITY_NAMED_IAM --parameter-overrides "ImageUri=$image"
bucket="$(aws cloudformation describe-stacks --stack-name vis-lambda \
  --query "Stacks[0].Outputs[?OutputKey=='Bucket'].OutputValue" --output text)"
aws s3 sync seed "s3://$bucket/seed/.vis/"

web="$(mktemp -d)"
if [[ -n "${VIS_WEB_DIR:-}" ]]; then
  mkdir "$web/vis-web" && cp -R "$VIS_WEB_DIR/." "$web/vis-web/"
else
  curl -fsSL "https://github.com/Blockether/vis/releases/download/$release/vis-web.tar.gz" | tar xz -C "$web"
fi
aws s3 sync "$web/vis-web/assets" "s3://$bucket/web/assets/" --cache-control "public, max-age=31536000, immutable"
rm -rf "$web"
aws cloudformation describe-stacks --stack-name vis-lambda \
  --query "Stacks[0].Outputs[?OutputKey=='WebUrl'].OutputValue" --output text
