# Evaluations API

Write and run evaluation tasks, then summarize, score, diagnose and compare the rounds from your
own program. Read [Evaluations](evals.md) first for the steps by hand and the meaning of the
numbers.

<div data-variant="python">

The `blockether.vis.evals` package does all the work on your computer. It writes task folders,
runs them with Harbor and reads the Harbor job folders. It needs no gateway.

</div>

<div data-variant="http">

The gateway routes score, diagnose and compare a round summary that you send. They run the same
Python code as the SDK, on the gateway. They save nothing: each answer comes from the request
body only.

</div>

The examples use the [Python SDK](python-sdk.md). To see the same steps as [HTTP API](http-api.md)
requests, select **HTTP** at the top of the page.

## When to use

- **A script must create benchmark tasks.** [Write a task](#write-a-task).
- **A script must run the tasks and score each new round.** [Run the tasks](#run-the-tasks),
  [summarize the round](#summarize-a-round), then [score it](#score-a-round).
- **A script must save all numbers of a round in one file.** [Report a round](#report-a-round).
- **Your dashboard must show why tasks fail.** [Find the failure causes](#find-the-failure-causes).
- **A reviewer labels failed attempts, and you must check the automatic causes.** [Check the
  labels](#check-the-labels).
- **Your release check must block a change that makes the agent worse.** [Compare two
  runs](#compare-two-runs).
- **A report must show the public position of a round.** [Read the leaderboard
  position](#read-the-leaderboard-position).

## Before you start

<div data-variant="python">

Install the SDK with Python 3.12 or later. It installs Harbor 0.24.0 with it. To run tasks, also
configure a provider, as in [Evaluations](evals.md#step-1-install-the-tools). The SDK downloads Vis
when it is not installed.

```bash
python -m pip install vis-agent
```

The examples run as one script in an empty folder. Each example uses the names of the examples
before it.

</div>

<div data-variant="http">

Define the `vis_api` function from [Authenticate requests](http-api.md#authenticate-requests). The
examples use it. Each request sends a JSON file with `--data @file.json`.

The routes need a round summary, and only the Python SDK makes one. The summary comes from the
Harbor job folders, which are on the computer that ran Harbor. So you need the Python SDK there,
also when another program sends the requests. Keep the summary and the answers yourself. The
gateway does not store them.

</div>

## Operations

<div data-variant="python">

Each function of `blockether.vis.evals` gives a typed result. The gateway method sends the same
operation to a gateway and gives the answer as a `dict`. Use it only when the gateway must do the
work, for example from a computer without the job folders.

| Task | Function | Gateway method |
|---|---|---|
| Write a task | `write_task(dataset, name, instruction, checks)` | None |
| Run the tasks | `run_tasks(path, model)` | None |
| Report a round | `round_report(jobs_dir, dataset_dir)` | None |
| List the tasks of a dataset | `read_tasks(dataset)` | None |
| Summarize a round | `round_summary(jobs_dir, dataset_dir, model=...)` | None |
| Score a round | `bench_report(summary)` | `post_evals_report(body=...)` |
| Find the failure causes | `diagnose(summary, labels, limits)` | `post_evals_diagnose(body=...)` |
| Check the cause labels | `cause_check(summary, labels, limits)` | `post_evals_labels(body=...)` |
| Check other labels | `agreement(items, ...)` | `post_evals_agreement(body=...)` |
| Compare two runs | `compare_cases(baseline, candidate)` | `post_evals_compare(body=...)` |
| Read the leaderboard position | `read_leaderboard(package, name, summary)` | `post_evals_leaderboard(body=...)` |

</div>

<div data-variant="http">

Each route takes a JSON object and gives a JSON object. No route writes or runs tasks or reads
job folders. Do these steps with the Python SDK or the `harbor` command.

| Task | Method and path |
|---|---|
| Score a round | `POST /v1/evals/report` |
| Find the failure causes | `POST /v1/evals/diagnose` |
| Check the cause labels | `POST /v1/evals/labels` |
| Check other labels | `POST /v1/evals/agreement` |
| Compare two runs | `POST /v1/evals/compare` |
| Read the leaderboard position | `POST /v1/evals/leaderboard` |

</div>

## Write a task

<div data-variant="python">

```python
from blockether.vis.evals import write_task

task = write_task(
    "tasks",
    "add-function",
    "In m.py add a new function c that returns 3, placed after b. Keep a and b unchanged.",
    {"want": {"m.py": ["def c(", "return 3", "def a(", "def b("]}},
    files={"m.py": "def a():\n    return 1\n\n\ndef b():\n    return 2\n"},
    agent_timeout_sec=300,
)
print(task.name, task.path, task.agent_timeout_sec)
```

The call writes the folder `tasks/add-function` with the files of [a
task](evals.md#step-2-write-a-task). `files` maps each seed file path to its text. `checks` is the
content of `tests/checks.json`. It raises `FileExistsError` when the task folder exists. Give a list
of strings as the instruction to write a [task with several
turns](evals.md#write-a-task-with-several-turns): one string for each user message.

`artifacts` lists the workspace files that Harbor copies to the trial folder, for example the
record of a [mock extension](evals.md#replace-an-extension-with-a-mock). `filesystem_roots` maps
each filesystem root id to a workspace folder, for example `{"library": "library"}`. The trial
registers each root in `vis.yml`, and the agent finds it with `ls`. The `e2e/py-project-paths`
task uses it.

</div>

<div data-variant="http">

No route writes a task, because Harbor reads the task folder on the computer that runs it. Write
the files of [a task](evals.md#step-2-write-a-task) yourself, or run the Python example on that
computer:

```bash
python3 - <<'EOF'
from blockether.vis.evals import write_task

write_task("tasks", "add-function", "In m.py add a new function c that returns 3.",
           {"want": {"m.py": ["def c(", "return 3"]}},
           files={"m.py": "def a():\n    return 1\n"})
EOF
```

</div>

## Run the tasks

<div data-variant="python">

```python
from blockether.vis.evals import run_tasks

job = run_tasks("tasks", "zai-coding-plan/glm-5.3-flash", jobs_dir="jobs", job_name="first")
print(job)  # jobs/first
```

`path` is one task folder or a folder of tasks. The model is a `provider/model` route. `attempts`
sets the trials for each task, and `concurrency` sets the trials that run at the same time. Each
trial makes paid model calls. The call waits until all trials end.

`agent_options` gives the options of the Vis agent. `{"source": "/path/to/vis"}` tests a Vis
checkout, and `{"native_bin": "/path/to/vis"}` tests a raw native image. The call sets
`HARBOR_TELEMETRY=off`, so Harbor sends no usage data.

</div>

<div data-variant="http">

No route runs tasks. Run them with Harbor on the computer that has the task folders:

```bash
HARBOR_TELEMETRY=off harbor run --path tasks \
  --agent blockether.vis.evals.agent:VisAgent \
  --env blockether.vis.evals.environment:HostEnvironment \
  --model zai-coding-plan/glm-5.3-flash --jobs-dir jobs --job-name first
```

</div>

Harbor writes the job folder `jobs/first`. [Read the results](evals.md#step-5-read-the-results)
describes its files. After the run, `run_tasks` also writes the [round report](#report-a-round)
to `jobs/first/vis-report.json`. Give `report=False` to skip it.

## Report a round

<div data-variant="python">

```python
from pathlib import Path

from blockether.vis.evals import REPORT_FILE, render_report, round_report

full = round_report("jobs/first", "tasks")
print(render_report(full))
for row in full.repeats:
    print(row.k, row.pass_at_k_percent, row.pass_all_k_percent)
print(Path("jobs/first", REPORT_FILE).exists())  # True, run_tasks wrote it
```

`round_report` reads the job folder once and gives a `RoundReport`:

| Field | Content |
|---|---|
| `score` | The [`bench_report`](#score-a-round) of the round. |
| `repeats` | One row for each k: `k`, `tasks`, `pass_at_k_percent` and `pass_all_k_percent`. |
| `diagnosis` | The [`diagnose`](#find-the-failure-causes) result, with the `label_check` of `labels.json`. |
| `judge` | The stage agreement of a judge, or `None` without `judge_stages.json`. |

`round_report` raises `FileNotFoundError` when the folder holds no trial. `labels`, `judge_dir`
and `stage_labels` replace the default paths of the [label and judge
files](evals.md#check-the-causes-with-labels). `write_report(full, path)` writes the JSON.

</div>

<div data-variant="http">

No route reads job folders. Run the report command on the computer that has the job folder:

```bash
python -m blockether.vis.evals.report jobs/first --tasks tasks --json
```

</div>

The repeat rows use the unbiased estimators of
[Repeated attempts](evals.md#repeated-attempts). To compute them for your own counts, use
`pass_at_k(runs, passed, k)` and `pass_all_k(runs, passed, k)`.

## Summarize a round

<div data-variant="python">

```python
import json

from blockether.vis.evals import round_summary

summary = round_summary(job, "tasks", model="zai-coding-plan/glm-5.3-flash")
print(summary["scored_tasks"], summary["solved_tasks"], summary["task_outcomes"])
with open("summary.json", "w") as stream:
    json.dump(summary, stream)
```

`jobs_dir` holds one Harbor job folder or one folder for each Harbor job. `dataset_dir` holds one
folder with a `task.toml` for each task. It is optional, but without it the total counts only the
attempted tasks. `model` is the model name or the `provider/model` route in the trajectories. An
attempt of another model does not count.

</div>

<div data-variant="http">

No route reads job folders, because the gateway can run on another computer. Build the summary
with the Python SDK, then send it:

```bash
python3 - <<'EOF'
import json
from blockether.vis.evals import round_summary

summary = round_summary("jobs/first", "tasks", model="zai-coding-plan/glm-5.3-flash")
with open("summary.json", "w") as stream:
    json.dump(summary, stream)
EOF
jq '{summary: .}' summary.json > report.json
```

</div>

The summary is plain JSON. It has the counts, `task_outcomes`, `agent_limits`, `usage_totals`,
`repeated_final_errors` and one row in `trials` for each attempt. It has no prompts, messages or
logs, so you can share it.

## Score a round

<div data-variant="python">

```python
from blockether.vis.evals import bench_report

report = bench_report(summary, "first")
print(report.pass_rate_percent, report.pass_rate_ci95)
print(report.strict_pass_rate_percent, report.detectable_change_points)
```

</div>

<div data-variant="http">

```bash
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/report" \
  -H 'content-type: application/json' --data @report.json
```

</div>

The answer has the [pass rates](evals.md#scores-and-intervals) with their 95% intervals, the
`outcomes` and the usage. The round name is optional. It only names the round in the answer.

## Find the failure causes

<div data-variant="python">

```python
from blockether.vis.evals import agent_limits, diagnose

limits = agent_limits("tasks")
diagnosis = diagnose(summary, None, limits)
for finding in diagnosis.findings:
    print(finding.kind, finding.task_count, finding.title)
```

</div>

<div data-variant="http">

```bash
jq '{summary: ., limits: {"add-function": 300}}' summary.json > diagnose.json
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/diagnose" \
  -H 'content-type: application/json' --data @diagnose.json
```

</div>

Each finding has a `kind`, a `title`, a `task_count`, a `share` of the scored tasks, a `detail` and
the `tasks`. The limits map each task to its agent time limit in seconds. Without limits, the
answer uses the `agent_limits` of the summary. Without both, no attempt counts as an
[early stop](evals.md#failure-causes). Labels are optional. With labels, the answer also has a
`label_check`.

## Check the labels

<div data-variant="python">

```python
from blockether.vis.evals import cause_check, load_labels, save_label

save_label("labels.json", summary, "add-function", "early_stops", "Stopped before the tests.")
labels = load_labels("labels.json")
check = cause_check(summary, labels, limits)
print(check.labelled, check.kappa, check.advice)
```

`save_label` saves the cause that a person gave to one failed attempt. The labels file maps each
trial name to its `cause` and `note`. A new label of the same attempt replaces the old one. A
solved attempt raises `ValueError`.

</div>

<div data-variant="http">

```bash
jq -n --slurpfile summary summary.json \
  '{summary: $summary[0], labels: {"add-function__a1b2": {cause: "early_stops"}}}' > labels.json
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/labels" \
  -H 'content-type: application/json' --data @labels.json
```

`labels` maps each trial name to an object with its `cause`.

</div>

The answer has the `labelled` count, the `agreement_percent`, the `kappa`, the Krippendorff
`alpha`, the `disagreements` and the `advice`. The advice uses the [kappa
limits](evals.md#check-the-causes-with-labels).

To check other automatic results, for example the stages of a judge, send your own items:

<div data-variant="python">

```python
from blockether.vis.evals import agreement

check = agreement(
    [("add-function__a1b2", "write", "verify")],
    source="judge",
    noun="judge stages",
    labeller="a person",
    fix="Correct the judge rubric first.",
)
print(check.kappa, check.advice)
```

Each item is a trial name, the automatic result and the human result.

</div>

<div data-variant="http">

```bash
cat > agreement.json <<'EOF'
{"items": [{"name": "add-function__a1b2", "automatic": "write", "human": "verify"}],
 "source": "judge", "noun": "judge stages"}
EOF
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/agreement" \
  -H 'content-type: application/json' --data @agreement.json
```

</div>

`source`, `noun`, `labeller` and `fix` are optional. They change only the words of the advice.

To check repeated samples of one judge, use `sample_alpha(samples)`. It maps each trial name to
the results of its samples and gives Krippendorff's alpha. `alpha_advice(alpha)` gives the advice
for the [alpha limits](evals.md#check-a-judge). For other data with missing values, use
`krippendorff_alpha(units)`. Each unit is a list of values, with `None` for a missing value.

## Compare two runs

<div data-variant="python">

Run the same tasks again after a change, then compare the two jobs:

```python
from blockether.vis.evals import case_results, compare_cases, read_trials

second = run_tasks(
    "tasks",
    "zai-coding-plan/glm-5.3-flash",
    job_name="second",
    agent_options={"source": "/path/to/vis"},
)
comparison = compare_cases(
    case_results(read_trials(job)),
    case_results(read_trials(second)),
    baseline_name="first",
    candidate_name="second",
    margin_points=5,
)
print(comparison.verdict, comparison.difference_points, comparison.difference_ci95)
```

`case_results` maps each case to its passed runs and all runs.

</div>

<div data-variant="http">

```bash
cat > compare.json <<'EOF'
{"baseline": [{"task": "add-function", "agent": "vis", "model": "zai-coding-plan/glm-5.3-flash", "passed": 0, "runs": 1}],
 "candidate": [{"task": "add-function", "agent": "vis", "model": "zai-coding-plan/glm-5.3-flash", "passed": 1, "runs": 1}],
 "margin_points": 5}
EOF
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/compare" \
  -H 'content-type: application/json' --data @compare.json
```

Each case has a `task`, an `agent`, a `model`, the `passed` runs and all `runs`.

</div>

A case is one task with one agent and one model. The answer has the `verdict`, the rates, the
difference with its interval, the `routes` tests, the `worse` and `better` cases and the
`caveats`. The baseline and candidate names are optional names for the error messages.

`margin_points` is optional. It is the smallest change in points that matters to you, from 0 to
100. Without it, a result without a clear change is `no measurable difference`. With it, the
verdict is `equivalent`, `not worse`, `not better` or `inconclusive`. The answer repeats the
margin in `margin_points`. Read [Decide on a change](evals.md#decide-on-a-change) to choose an
action from the verdict.

## Read the leaderboard position

<div data-variant="python">

```python
from blockether.vis.evals import read_leaderboard

board = read_leaderboard("terminal-bench/terminal-bench", "4-0-0", summary, "first")
print(board.title, len(board.rows))
print(board.vis.rank_by_pass_rate, board.vis.rank_range, board.vis.caveats)
```

</div>

<div data-variant="http">

```bash
jq '{package: "terminal-bench/terminal-bench", name: "4-0-0", summary: ., round_name: "first"}' \
  summary.json > leaderboard.json
vis_api -X POST "$VIS_GATEWAY_URL/v1/evals/leaderboard" \
  -H 'content-type: application/json' --data @leaderboard.json
```

</div>

This is the only evaluation operation that uses the network: it reads the board from Harbor Hub.
The package and the name select the board. Without a summary, the answer has only the board
`rows`, and `vis` has no value.

## Handle errors

<div data-variant="python">

```python
try:
    write_task("tasks", "Add Function", "Add c.", {"want": {"m.py": ["def c("]}})
except ValueError as error:
    print(error)  # task name must be lowercase words joined by hyphens: 'Add Function'
```

A function raises `ValueError` for an invalid value and `FileNotFoundError` for a missing folder.
A gateway method raises `GatewayError` with the HTTP `status` and the error `code` of the table.

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

- [Evaluations](evals.md) — the steps by hand and the meaning of the scores, causes and verdicts.
- [Python SDK](python-sdk.md) — install the SDK, connect a client and handle gateway errors.
- [HTTP API](http-api.md) — authentication, the OpenAPI document and gateway errors.
- [Harbor documentation](https://docs.harborframework.com/) — tasks, jobs and the `harbor`
  command.
