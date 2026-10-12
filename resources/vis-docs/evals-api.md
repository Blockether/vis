# Evaluations API

Summarize, score, diagnose and compare [evaluation](evals.md) rounds from your own program.

<div data-variant="python">

The `blockether.vis.evals` package reads Harbor job folders on your computer. Each
`GatewayClient` method sends one evaluation request to a gateway and returns the parsed JSON answer.

</div>

<div data-variant="http">

The requests and answers are JSON. The gateway runs the same evaluation code as the Python SDK.

</div>

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A script must score each new benchmark round.** [Summarize a round](#summarize-a-round),
  then [score it](#score-a-round).
- **Your dashboard must show why tasks fail.** [Find the failure causes](#find-the-failure-causes).
- **A reviewer labels failed attempts, and you must check the automatic causes.** [Check the
  labels](#check-the-labels).
- **Your release check must block a change that makes the agent worse.** [Compare two
  runs](#compare-two-runs).
- **A report must show the public position of a round.** [Read the leaderboard
  position](#read-the-leaderboard-position).

To learn what the scores, causes and verdicts mean, read [Evaluations](evals.md). To send the same
requests from another language, select **HTTP** at the top of the page.

## Before you start

<div data-variant="python">

[Install the SDK](python-sdk.md#install-the-sdk). Then set `VIS_GATEWAY_URL` and
`VIS_GATEWAY_TOKEN` as in [Connect to a gateway and run a
task](python-sdk.md#connect-to-a-gateway-and-run-a-task).

```python
import os

from blockether.vis.engine import GatewayClient

with GatewayClient(os.environ["VIS_GATEWAY_URL"], token=os.environ["VIS_GATEWAY_TOKEN"]) as client:
    report = client.post_evals_report(body={"summary": summary})
    print(report["pass_rate_percent"])
```

The next examples are calls on this `client` inside the `with` block. The functions of
`blockether.vis.evals` need no gateway.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. Each request sends a JSON file with `--data @file.json`.

</div>

You also need the job folders of a finished Harbor round. Each trial folder must have its
`result.json`. The trajectory and the verifier report make the summary more complete.

## Operations

<div data-variant="python">

`GatewayClient` has one method for each evaluation route. You give the body as a Python `dict`.
The package functions give the same results as typed objects.

| Task | Method | Package function |
|---|---|---|
| Summarize a round | None | `round_summary(jobs_dir, dataset_dir, model=...)` |
| Score a round | `post_evals_report(body=...)` | `bench_report(summary)` |
| Find the failure causes | `post_evals_diagnose(body=...)` | `diagnose(summary, labels, limits)` |
| Check the cause labels | `post_evals_labels(body=...)` | `cause_check(summary, labels, limits)` |
| Check other labels | `post_evals_agreement(body=...)` | `agreement(items, ...)` |
| Compare two runs | `post_evals_compare(body=...)` | `compare_cases(baseline, candidate)` |
| Read the leaderboard position | `post_evals_leaderboard(body=...)` | `read_leaderboard(...)` |

</div>

<div data-variant="http">

Each route takes a JSON object and gives a JSON object.

| Task | Method and path |
|---|---|
| Score a round | `POST /v1/evals/report` |
| Find the failure causes | `POST /v1/evals/diagnose` |
| Check the cause labels | `POST /v1/evals/labels` |
| Check other labels | `POST /v1/evals/agreement` |
| Compare two runs | `POST /v1/evals/compare` |
| Read the leaderboard position | `POST /v1/evals/leaderboard` |

</div>

## Summarize a round

<div data-variant="python">

```python
import json

from blockether.vis.evals import round_summary

summary = round_summary("rounds/main/jobs", "datasets/terminal-bench", model="zai/glm-5.3")
print(summary["scored_tasks"], summary["solved_tasks"], summary["task_outcomes"])
with open("summary.json", "w") as stream:
    json.dump(summary, stream)
```

`jobs_dir` holds one folder for each Harbor job. `dataset_dir` holds one folder with a `task.toml`
for each task. It is optional, but without it the total counts only the attempted tasks. `model`
is the model name or the `provider/model` route in the trajectories. An attempt of another model
does not count.

</div>

<div data-variant="http">

No route reads job folders, because the gateway can run on another computer. Build the summary
with the Python SDK, then send it:

```bash
python3 - <<'EOF'
import json
from blockether.vis.evals import round_summary

summary = round_summary("rounds/main/jobs", "datasets/terminal-bench", model="zai/glm-5.3")
with open("summary.json", "w") as stream:
    json.dump(summary, stream)
EOF
jq '{summary: .}' summary.json > report.json
```

</div>

The summary has the counts, `task_outcomes`, `usage_totals`, `repeated_final_errors` and one row
in `trials` for each attempt.

## Score a round

<div data-variant="python">

```python
report = client.post_evals_report(body={"summary": summary, "round_name": "main"})
print(report["pass_rate_percent"], report["pass_rate_ci95"])
print(report["strict_pass_rate_percent"], report["detectable_change_points"])
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/report" \
  -H 'content-type: application/json' --data @report.json
```

</div>

The answer has the [pass rates](evals.md#scores-and-intervals) with their 95% intervals, the
`outcomes` and the usage. `round_name` is optional. It only names the round in the answer.

## Find the failure causes

<div data-variant="python">

```python
from blockether.vis.evals import agent_limits

limits = agent_limits("datasets/terminal-bench")
diagnosis = client.post_evals_diagnose(body={"summary": summary, "limits": limits})
for finding in diagnosis["findings"]:
    print(finding["kind"], finding["task_count"], finding["title"])
```

</div>

<div data-variant="http">

```bash
jq '{summary: ., limits: {"fix-git": 900}}' summary.json > diagnose.json
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/diagnose" \
  -H 'content-type: application/json' --data @diagnose.json
```

</div>

Each finding has a `kind`, a `title`, a `task_count`, a `share` of the scored tasks, a `detail` and
the `tasks`. `limits` maps each task to its agent time limit in seconds. Without limits, no attempt
counts as an [early stop](evals.md#failure-causes). `labels` is optional. With labels, the answer
also has a `label_check`.

## Check the labels

<div data-variant="python">

```python
from blockether.vis.evals import load_labels, save_label

save_label("rounds/main/labels.json", summary, "fix-git", "early_stops", "Stopped after the first test.")
labels = load_labels("rounds/main/labels.json")
check = client.post_evals_labels(body={"summary": summary, "labels": labels, "limits": limits})
print(check["labelled"], check["kappa"], check["advice"])
```

`save_label` saves one human cause in a labels file. It maps each trial name to its `cause` and
`note`. A new label of the same attempt replaces the old one.

</div>

<div data-variant="http">

```bash
jq -n --slurpfile summary summary.json \
  '{summary: $summary[0], labels: {"fix-git__a1b2": {cause: "early_stops"}}}' > labels.json
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/labels" \
  -H 'content-type: application/json' --data @labels.json
```

`labels` maps each trial name to an object with its `cause`.

</div>

The answer has the `labelled` count, the `agreement_percent`, the `kappa`, the `disagreements` and
the `advice`. The advice uses the [kappa limits](evals.md#check-the-causes-with-labels).

To check other automatic results, for example the stages of a judge, send your own items:

<div data-variant="python">

```python
check = client.post_evals_agreement(
    body={
        "items": [{"name": "fix-git__a1b2", "automatic": "write", "human": "verify"}],
        "source": "judge",
        "noun": "judge stages",
    }
)
print(check["kappa"], check["advice"])
```

</div>

<div data-variant="http">

```bash
cat > agreement.json <<'EOF'
{"items": [{"name": "fix-git__a1b2", "automatic": "write", "human": "verify"}],
 "source": "judge", "noun": "judge stages"}
EOF
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/agreement" \
  -H 'content-type: application/json' --data @agreement.json
```

</div>

`source`, `noun`, `labeller` and `fix` are optional. They change only the words of the advice.

## Compare two runs

<div data-variant="python">

```python
from blockether.vis.evals import case_results, read_trials

def cases(directory):
    return [
        {"task": task, "agent": agent, "model": model, "passed": passed, "runs": runs}
        for (task, agent, model), (passed, runs) in case_results(read_trials(directory)).items()
    ]

comparison = client.post_evals_compare(
    body={"baseline": cases("rounds/before/jobs"), "candidate": cases("rounds/after/jobs")}
)
print(comparison["verdict"], comparison["difference_points"], comparison["difference_ci95"])
```

</div>

<div data-variant="http">

```bash
cat > compare.json <<'EOF'
{"baseline": [{"task": "fix-git", "agent": "vis", "model": "zai/glm-5.3", "passed": 0, "runs": 1}],
 "candidate": [{"task": "fix-git", "agent": "vis", "model": "zai/glm-5.3", "passed": 1, "runs": 1}]}
EOF
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/compare" \
  -H 'content-type: application/json' --data @compare.json
```

</div>

Each case has a `task`, an `agent`, a `model`, the `passed` runs and all `runs`. The answer has the
`verdict`, the rates, the difference with its interval, the `routes` tests, the `worse` and
`better` cases and the `caveats`. `baseline_name` and `candidate_name` are optional names for the
error messages.

## Read the leaderboard position

<div data-variant="python">

```python
board = client.post_evals_leaderboard(body={"summary": summary, "round_name": "main"})
print(board["title"], len(board["rows"]))
print(board["vis"]["rank_by_pass_rate"], board["vis"]["rank_range"], board["vis"]["caveats"])
```

</div>

<div data-variant="http">

```bash
jq '{summary: ., round_name: "main"}' summary.json > leaderboard.json
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/leaderboard" \
  -H 'content-type: application/json' --data @leaderboard.json
```

</div>

This is the only evaluation request that uses the network: the gateway reads the board from Harbor
Hub. `package` and `name` select the board. The default is `terminal-bench/terminal-bench` and
`4-0-0`. Without a `summary`, the answer has only the board `rows`, and `vis` has no value.

## Handle errors

<div data-variant="python">

```python
from blockether.vis.engine import GatewayError

try:
    client.post_evals_report(body={})
except GatewayError as error:
    print(error.status, error.code)  # 400 invalid-evals-request
```

A failed request raises `GatewayError` with the HTTP `status` and the error `code`.

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/report" \
  -H 'content-type: application/json' --data '{}'
```

</div>

| Status | Code | Cause |
|---|---|---|
| 400 | `invalid-evals-request` | The body is not a JSON object, or a field has the wrong type. The message names the field. |
| 500 | `evals-failed` | The evaluation code stopped with an error. Report it with the request body. |

## See also

- [Evaluations](evals.md) — what the scores, causes, labels and verdicts mean.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [HTTP API](http-api.md) — authentication, the OpenAPI document and gateway errors.
