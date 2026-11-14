#!/usr/bin/env bash
# Deploy the Vis playground: the image, the secrets, the stack and the GitHub hooks.
# The script is safe to run again. It keeps each secret that exists: to change one,
# write its parameter with `aws ssm put-parameter --overwrite`.
#
#   REPOSITORY=owner/vis-playground PLAYGROUND_LLM_URL=https://host/v1 PLAYGROUND_LLM_KEY=... ./deploy.sh
set -euo pipefail
cd "$(dirname "$0")"

STACK=${STACK:-vis-playground}
REGION=${AWS_REGION:-eu-central-1}
REPOSITORY=${REPOSITORY:?set REPOSITORY to owner/name}
ENGINE=${ENGINE:-docker}
MODELS=${PLAYGROUND_MODELS:-}
export AWS_REGION=$REGION AWS_DEFAULT_REGION=$REGION AWS_PAGER=

account=$(aws sts get-caller-identity --query Account --output text)
registry=$account.dkr.ecr.$REGION.amazonaws.com

exists() { aws ssm get-parameter --name "/$STACK/$1" >/dev/null 2>&1; }
# The CLI reads the value from standard input, so it does not show in the process list.
store() { aws ssm put-parameter --name "/$STACK/$1" --type SecureString --value file:///dev/stdin >/dev/null; }

echo "1/6 repository $REPOSITORY"
if ! gh repo view "$REPOSITORY" >/dev/null 2>&1; then
  gh repo create "$REPOSITORY" --private --description "The repository of the Vis playground"
  seed=$(mktemp -d)
  cp -R repo-seed/. "$seed"
  git -C "$seed" init --quiet --initial-branch main
  git -C "$seed" add --all
  git -C "$seed" -c user.name="Vis Playground" -c user.email=playground@blockether.com commit --quiet --message "chore: start the playground repository"
  git -C "$seed" push --quiet "https://github.com/$REPOSITORY.git" main
  rm -rf "$seed"
fi
# A session can push to this repository. GitHub must not run code from it, and the
# main branch must only grow. A ruleset needs a public repository or a paid plan.
gh api --method PUT "repos/$REPOSITORY/actions/permissions" -F enabled=false >/dev/null
if ! gh api "repos/$REPOSITORY/rulesets" --jq '.[].name' 2>/dev/null | grep -qxF "main is append-only"; then
  jq -n '{name: "main is append-only", target: "branch", enforcement: "active",
          conditions: {ref_name: {include: ["~DEFAULT_BRANCH"], exclude: []}},
          rules: [{type: "deletion"}, {type: "non_fast_forward"}]}' |
    gh api "repos/$REPOSITORY/rulesets" --method POST --input - >/dev/null 2>&1 ||
    echo "  no ruleset: this GitHub plan does not permit one for this repository"
fi

echo "2/6 image"
aws ecr describe-repositories --repository-names "$STACK" >/dev/null 2>&1 ||
  aws ecr create-repository --repository-name "$STACK" --image-scanning-configuration scanOnPush=true >/dev/null
# Each run pushes one image. ECR keeps the 5 newest images and removes the others.
aws ecr put-lifecycle-policy --repository-name "$STACK" --lifecycle-policy-text '{"rules":[{"rulePriority":1,
  "description":"Keep the 5 newest images","selection":{"tagStatus":"any","countType":"imageCountMoreThan","countNumber":5},
  "action":{"type":"expire"}}]}' >/dev/null
aws ecr get-login-password | "$ENGINE" login --username AWS --password-stdin "$registry" >/dev/null
tag=$(date -u +%Y%m%d%H%M%S)
"$ENGINE" build --platform linux/arm64 --tag "$registry/$STACK:$tag" .
# Lambda needs the Docker image format, not the OCI format with attestations.
if [ "$ENGINE" = podman ]; then
  podman push --format docker "$registry/$STACK:$tag"
else
  docker push "$registry/$STACK:$tag"
fi
digest=$(aws ecr describe-images --repository-name "$STACK" --image-ids imageTag="$tag" --query 'imageDetails[0].imageDigest' --output text)

echo "3/6 secrets"
exists password || openssl rand -base64 24 | tr -d '\n' | store password
exists webhook-secret || openssl rand -hex 32 | tr -d '\n' | store webhook-secret
exists llm-url || printf %s "${PLAYGROUND_LLM_URL:?set PLAYGROUND_LLM_URL}" | store llm-url
exists llm-key || printf %s "${PLAYGROUND_LLM_KEY:?set PLAYGROUND_LLM_KEY}" | store llm-key
if ! exists deploy-key; then
  # The key opens one repository. GitHub gets the public half.
  keys=$(mktemp -d)
  # The private half does not stay on this computer, also when a step fails.
  trap 'rm -rf "$keys"' EXIT
  ssh-keygen -q -t ed25519 -N "" -C "$STACK" -f "$keys/key"
  gh repo deploy-key add "$keys/key.pub" --repo "$REPOSITORY" --title "$STACK" --allow-write
  store deploy-key <"$keys/key"
  rm -rf "$keys"
  trap - EXIT
fi

echo "4/6 stack $STACK"
# The functions in the network can open a connection to S3, and not to the internet.
prefixes=$(aws ec2 describe-managed-prefix-lists --filters "Name=prefix-list-name,Values=com.amazonaws.$REGION.s3" --query 'PrefixLists[0].PrefixListId' --output text)
aws cloudformation deploy --stack-name "$STACK" --template-file template.yaml --capabilities CAPABILITY_IAM \
  --no-fail-on-empty-changeset \
  --parameter-overrides "ImageUri=$registry/$STACK@$digest" "Repository=$REPOSITORY" "Models=$MODELS" "S3PrefixList=$prefixes"
url=$(aws cloudformation describe-stacks --stack-name "$STACK" --query "Stacks[0].Outputs[?OutputKey=='Url'].OutputValue" --output text)

echo "5/6 webhook"
hook=${url}hooks/github
if ! gh api "repos/$REPOSITORY/hooks" --jq '.[].config.url' | grep -qxF "$hook"; then
  aws ssm get-parameter --name "/$STACK/webhook-secret" --with-decryption --query Parameter.Value --output text |
    jq -Rn --arg url "$hook" '{name: "web", active: true, events: ["push"], config: {url: $url, content_type: "json", secret: input}}' |
    gh api "repos/$REPOSITORY/hooks" --method POST --input - >/dev/null
fi

echo "6/6 first release"
aws lambda invoke --function-name "$STACK-repo" --cli-binary-format raw-in-base64-out \
  --payload '{"action":"fetch","force":true}' /dev/stdout
echo
echo "Playground: $url"
echo "Password:   aws ssm get-parameter --name /$STACK/password --with-decryption --query Parameter.Value --output text"
