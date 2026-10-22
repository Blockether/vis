# Run Vis on AWS Lambda

## When to use

- You want Vis in the cloud without a server that runs all the time: see [How it works](#how-it-works).
- You want to use Vis from a phone or another computer through a browser: see [Connect](#connect).
- You want to try this example in your own AWS account: see [Deploy](#deploy).
- You want to remove everything again: see [Remove the stack](#remove-the-stack).

For a gateway on your own computer or server, read the *Running a gateway* page of the manual.

## How it works

This example runs the native Vis gateway and the Vis web app on AWS Lambda. S3 keeps all state.
A worker gives its work to a new worker before the 15-minute Lambda limit.

- CloudFront serves the web app files (`/assets/*`) from `s3://<bucket>/web/`, without Lambda.
- The `vis-front` function answers `/`, `/login` and the API behind CloudFront. It checks the password.
  It sends each API call to the live worker through SQS and reads the answer from S3.
- The `vis-worker` function holds `lease.json` in S3 and runs `vis-agent-native gateway`.
  It restores `state/home.tar.gz`, and it saves the state every 60 s and before it stops.
- At 810 s the worker saves the state, gives the lease away and starts a successor.
  The successor continues each turn that was running.
- A provider that rotates its refresh token writes `~/.vis/*auth*.json`. The worker then saves the state at once.
- After 600 s without requests, the worker saves the state and stops.
- The EventBridge schedule `vis-tick` runs each minute. It starts a worker when work is due.
  Work is due when a turn was running or an automation runs within 120 s.

| File | Purpose |
|---|---|
| `template.yaml` | The CloudFormation stack: functions, roles, bucket, queue, schedule and CloudFront |
| `Dockerfile` | The Lambda image: native Vis, the web app, the Lambda Web Adapter and the relay |
| `vis_lambda.py` | The front relay and the worker lease, handoff and state sync |
| `login.html` | The password page |
| `deploy.sh` | Builds and pushes the image, deploys the stack and uploads the web app |
| `deployer-policy.json` | The smallest IAM policy for the role that runs `deploy.sh` |
| `seed/config.edn` | The first `~/.vis/config.edn` of the gateway |

## Deploy

### Costs and permissions

The stack creates billable resources: Lambda, S3, SQS, CloudFront, ECR and CloudWatch Logs.
An idle stack costs little, because no worker runs without requests or due work.
Each open event stream keeps one front Lambda busy, so a browser tab that stays open costs Lambda time.

You need an AWS account where you can create IAM roles.

### Prerequisites

- The AWS CLI version 2.
- `podman`, or `docker` with `CONTAINER_CLI=docker`.
- Python 3 and `curl`.

### Create the deployer role

1. In `deployer-policy.json`, replace each `ACCOUNT_ID` with your 12-digit account ID.
2. Create a role `vis-deployer` that your administrator user can assume, and attach the policy as an inline policy.
3. Add a profile for the role to `~/.aws/config`:

   ```ini
   [profile vis-deployer]
   role_arn = arn:aws:iam::ACCOUNT_ID:role/vis-deployer
   source_profile = default
   region = eu-central-1
   ```

### Run the deployment

```bash
cd examples/aws-lambda
AWS_PROFILE=vis-deployer ./deploy.sh
AWS_PROFILE=vis-deployer VIS_RELEASE=v0.2.54 ./deploy.sh   # another Vis release
AWS_PROFILE=vis-deployer VIS_WEB_DIR=../../apps/vis-companion/dist-web ./deploy.sh   # a local web build
```

The script creates the password parameter `/vis/token` (a UUID in SSM) once.
Then it deploys the stack and prints the URL of the web app.
The first deployment takes about 10 minutes, because CloudFront needs that time.

## Connect

1. Read the password:

   ```bash
   aws ssm get-parameter --name /vis/token --with-decryption --query Parameter.Value --output text
   ```

2. Open the printed `WebUrl` (`https://<id>.cloudfront.net`) in a browser and type the password.

The web app then connects to this gateway. The first request after a pause waits about 25 s for a worker.
The browser keeps an HttpOnly cookie for one year. The cookie is an HMAC of the password, not the password.
A new password signs out every browser.

API clients send `Authorization: Bearer <password>`. Webhooks (`/v1/hooks/`) use their own secrets.

## Configuration

`seed/` is the first `~/.vis`. Edit `seed/config.edn`, then deploy again.
After the first start, the live state in S3 wins. Settings changes in the web app persist.

Function environment values:

| Name | Default | Meaning |
|---|---|---|
| `VIS_LIFETIME_S` | 900 | Lambda timeout of the worker |
| `VIS_HANDOFF_MARGIN_S` | 90 | Time before the timeout for the handoff |
| `VIS_IDLE_S` | 600 | Idle time before the worker stops; keep it below the lifetime |
| `VIS_STREAM_S` | 600 | Length of one relayed event stream; the client then reconnects |

### Provider sign-in

Sign in to each provider in the web app on Lambda: open Settings, then Providers, then Sign in.
OpenAI Codex uses a device code, so you can confirm the sign-in on any device.

Do not copy a local `~/.vis/*-auth.json` into `seed/`. Codex and Anthropic rotate the refresh token.
Two installations that share one token sign each other out at the next refresh.

## Limits

- The account concurrency limit applies to all Lambda functions. A new account has a limit of 10.
  With 10, the web app gets some 429 answers on load and recovers on its next poll.
  Request 1000 in Service Quotas (`L-B99A9384`).
- A cold start of the worker takes about 25 s. The first request waits for it.
- Lambda has no `/dev/ptmx`, so shell commands run without a terminal.

## Remove the stack

Run these commands to delete the gateway and all its sessions. You cannot undo this.

```bash
export AWS_PROFILE=vis-deployer
bucket="$(aws cloudformation describe-stacks --stack-name vis-lambda \
  --query "Stacks[0].Outputs[?OutputKey=='Bucket'].OutputValue" --output text)"
aws cloudformation delete-stack --stack-name vis-lambda
aws cloudformation wait stack-delete-complete --stack-name vis-lambda
aws ecr delete-repository --repository-name vis-lambda --force
aws ssm delete-parameter --name /vis/token
```

The stack keeps the state bucket on purpose. To delete it, do these steps:

1. In the S3 console, select the bucket and choose **Empty**. This deletes every object version.
2. Run `aws s3api delete-bucket --bucket "$bucket"`.

Then delete the `vis-deployer` role with your administrator user.
