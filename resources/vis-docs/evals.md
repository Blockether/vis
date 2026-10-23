# Evaluations

An evaluation measures how well an agent solves a set of tasks. You write the tasks, run them with
[Harbor](https://docs.harborframework.com/), then score the results, find why tasks failed and
compare two runs. This page shows each step from an empty folder to a score.

## When to use

- **You want to know if Vis can do a type of work, for example renames.** [Write a
  task](#step-2-write-a-task), then [run it](#step-4-run-the-tasks).
- **You changed a prompt, a tool or a model and want proof that it helps.** Run the same tasks
  before and after the change, then [compare the two runs](#compare-two-runs).
- **You ran a benchmark, for example Terminal-Bench, and want the score with its uncertainty.**
  Read [Scores and intervals](#scores-and-intervals).
- **You want all numbers of a run at once.** [Report the round](#report-the-round) with one
  command.
- **You want to know why the agent failed tasks.** Read [Failure causes](#failure-causes).
- **You are not sure that your tasks measure the agent and not the task text.** Read [Write a
  fair task](#write-a-fair-task).
- **You want to know if the failure causes or a judge are reliable.** Read [Check the causes
  with labels](#check-the-causes-with-labels).
- **The agent must call a function of your extension, but the real function must not run.**
  [Replace the extension with a mock](#replace-an-extension-with-a-mock).
- **You want to know how Vis and Harbor share the work.** Read [Vis and
  Harbor](#vis-and-harbor).
- **Your own program must do these steps.** Read [Evaluations API](evals-api.md).

To watch a single session while it runs, use [Sessions](sessions.md). To run a prompt on a
schedule, use [Automations](automations.md).

## Vis and Harbor

Harbor is an open framework for agent evaluations. Vis uses it as its only task runner. Each
tool does a different part of the work:

| Part | Harbor | Vis |
|---|---|---|
| Task format | Defines the task folder: `task.toml`, `instruction.md`, `environment/` and `tests/`. | Adds `tests/checks.json`, which its grader reads. |
| Run | `harbor run` starts a job and runs one trial for each task and attempt. | Gives the `VisAgent` agent and the `HostEnvironment` environment. |
| Verifier | Runs `tests/test.sh` after the agent and reads the reward. | Gives the grader `python -m blockether.vis.evals.grade`. |
| Results | Writes a job folder with one `result.json` for each trial. | Writes the agent trajectory in the Harbor ATIF format. |
| Analysis | `harbor view` shows the trials in a browser. | Gives scores with intervals, failure causes, label checks and paired comparisons. |

The Vis setup is different from the Harbor defaults in three ways:

- **No containers.** Harbor usually runs each trial in Docker. `HostEnvironment` runs each trial
  in a new git repository on your computer. The agent can read files outside that repository,
  so run only tasks that you trust.
- **A Vis grader.** A standard Harbor verifier runs tests, for example `pytest`, and writes a
  reward. The Vis grader also reads the agent trajectory. It can check the answer, the tools
  that the agent used and its errors, not only the files.
- **Scores for every agent.** The analysis reads only standard Harbor files. It scores a job of
  any Harbor agent, for example Claude Code or Codex, in the same way.

For the Harbor concepts, read the Harbor [core concepts](https://docs.harborframework.com/core-concepts)
and the [task overview](https://docs.harborframework.com/tasks/overview).

## Step 1: Install the tools

You need Python 3.12 or later and a configured provider and model. Read
[Configuration](configuration.md) for the provider.

Install the SDK. It installs Harbor 0.24.0 with it:

```bash
python3 -m venv .venv
. .venv/bin/activate
python -m pip install vis-agent
harbor --version
```

The last command shows `0.24.0`. You do not have to install Vis. When `vis-agent` is missing or another release,
the first run downloads the Vis release of the SDK once. Each trial makes paid model calls. A task
with one attempt costs one agent run.

## Step 2: Write a task

A task is one folder. The folder name is the task name. This example asks the agent to add one
function to a Python file:

```text
tasks/
  add-function/
    task.toml
    instruction.md
    environment/workspace/m.py
    tests/checks.json
    tests/test.sh
```

`task.toml` sets the time limits in seconds:

```toml
[metadata]
lang = "python"

[agent]
timeout_sec = 300.0

[verifier]
timeout_sec = 300.0
```

`instruction.md` is the prompt that the agent gets:

```markdown
In m.py add a new function c that returns 3, placed after b. Keep a and b unchanged.
```

`environment/workspace/` holds the seed files. Each trial starts with a copy of them in a new git
repository. Here, `m.py` defines the functions `a` and `b`.

`tests/checks.json` tells the grader what a correct result is:

```json
{
  "want": {"m.py": ["def c(", "return 3", "def a(", "def b("]},
  "wantnot": {}
}
```

`tests/test.sh` runs the grader. Use it without changes:

```bash
#!/usr/bin/env bash
# Grade the trial. The Vis host environment sets the VIS_HARBOR_* paths.
set -euo pipefail
exec "$VIS_HARBOR_PYTHON" -m blockether.vis.evals.grade "$VIS_HARBOR_TESTS/checks.json"
```

Make the script executable with `chmod +x tasks/add-function/tests/test.sh`. To write the same
folder from Python, use [`write_task`](evals-api.md#write-a-task).

### Choose the checks

A trial passes only when all its checks pass. These are the checks that most tasks need:

| Key | What it checks |
|---|---|
| `want` | Each named file holds each text. |
| `wantnot` | No named file holds any of its texts. |
| `want_answer` | The final answer of the agent holds each text. |
| `want_json_files` | Each named file is JSON Lines with exactly these rows, in this order. |
| `want_tools` | The agent used each named tool. |
| `forbid_tools` | The agent did not use any named tool. |
| `max_patch_calls_per_file` | The agent changed each file with at most this number of successful `patch` calls. |
| `max_total_output_chars` | The tool output of the run stayed below this size. |

The grader always checks that the agent finished without an error. The `e2e/` folder of the Vis
repository has 42 tasks that use these checks. Read them for more examples.

To test a project with its own test suite, write a standard Harbor verifier instead. Read the
Harbor guide [Create a task](https://docs.harborframework.com/tutorials/create-a-task).

### Write a fair task

A task that tells the agent the answer measures the instruction, not the agent. Use these rules
for each task. They follow the Anthropic guides in [References](#references).

- **Describe the goal, not the solution.** Give the result and the limits that a person needs.
  Do not copy the texts of `want` into the instruction, unless the exact format is a requirement.
- **Make the task unambiguous.** Two experts who read the instruction must agree on pass or fail.
  Each check must follow from the instruction. A check on a file that the instruction does not
  name fails good agents.
- **Prove that the task can be solved.** Write a reference solution and make sure that it passes
  all checks. A task that nobody can solve gives 0% for every agent.
- **Test both directions.** Add tasks where the correct action is to change nothing, for example
  a file that must stay the same. Check it with `wantnot`. A set of only "change it" tasks
  rewards an agent that always changes files.
- **Grade the result, not the path.** Check the files and the answer. Use `want_tools` and
  `forbid_tools` only when the tool is a requirement, as in the [mock
  example](#replace-an-extension-with-a-mock).
- **Keep the solution out of the trial.** The seed files must not hold the solution, a solved
  copy or a hint in a comment. `HostEnvironment` lets the agent read files outside its
  repository, also the `tests/` folder of the task. Look for such reads in the trajectories.
- **Give a judge a rubric and a way out.** When a model grades the result, give it a detailed
  rubric. Ask for the reason before the score, and let it answer that it does not know. Then
  [check the judge with labels](#check-the-causes-with-labels).
- **Replace tasks that every run solves.** A task that always passes cannot show a change.

After each change of the tasks, read some passed and some failed trajectories. Some broken tasks
show only there, for example a check that passes for the wrong reason.

### Replace an extension with a mock

A task can test an agent that uses a function of your own extension, for example
`mylib.send_some_huge_payload`. In an evaluation, you usually do not want the real function to
send data. Put a mock extension in the seed files instead. The mock returns a made-up answer and
writes one line for each call.

Vis loads the extensions of the project from `.vis/extensions/` in the workspace. A project
extension replaces a global extension with the same `name`, so the agent calls the mock and the
real function does not run. The mock replaces the whole extension, so copy each function that
the agent needs. Keep the names, the parameters and the docstrings the same as in the real
extension. The agent then sees the same contract.

This mock writes the target, the size and the SHA-256 hash of each payload, not the payload:

```python
"""Mock of mylib for evaluations: it records each call and sends nothing."""

import hashlib
import json
from pathlib import Path

import blockether.vis.extension as sdk

# The workspace root is two folders above .vis/extensions/.
LEDGER = Path(__file__).resolve().parents[2] / "payload-calls.jsonl"


class Payloads:
    @sdk.method(
        tag="mutation", activity=sdk.Activity(label="Send payload", show_start=False)
    )
    def send_some_huge_payload(self, target: str, payload: str) -> dict:
        """Send one payload to a named target. Returns the status and the job id."""
        calls = LEDGER.read_text().count("\n") if LEDGER.exists() else 0
        row = {
            "target": target,
            "size": len(payload),
            "sha256": hashlib.sha256(payload.encode()).hexdigest(),
        }
        with LEDGER.open("a") as stream:
            stream.write(json.dumps(row, sort_keys=True) + "\n")
        return {"status": "accepted", "id": f"job-{calls + 1}"}


sdk.register_extension(
    sdk.Extension(
        name="mylib",
        alias="mylib",
        description="Send payloads to the report service.",
        symbols=[sdk.Symbol(Payloads(), name="mylib")],
    )
)
```

Save it as `environment/workspace/.vis/extensions/mylib.py`. Then check the calls in
`tests/checks.json`:

```json
{
  "want_tools": ["mylib.send_some_huge_payload"],
  "want": {
    "payload-calls.jsonl": ["\"target\": \"north\"", "\"target\": \"south\""]
  },
  "want_answer": ["job-1", "job-2"]
}
```

- `want_tools` passes when the agent called the function at least once without an error.
- `want` checks each call in the record. Add one text for each call that you expect.
- `want_answer` shows that the agent used the answers of the mock.

The record must also stay after the trial, because the workspace of a trial is temporary. Name
it in `artifacts` at the start of `task.toml`, before the first table:

```toml
artifacts = ["payload-calls.jsonl"]

[metadata]
```

After the check, Harbor copies the file to `artifacts/payload-calls.jsonl` in the trial folder.
`artifacts/manifest.json` gives the status of each file: `ok`, or `failed` when the file is
missing. A missing file does not change the reward. `write_task` writes this line when you give
[`artifacts`](evals-api.md#write-a-task).

## Step 3: Check the tasks

A bad task gives wrong scores. For example, its instruction does not name a file that its checks
need. Review each task with the Harbor check rubric:

```bash
python -m blockether.vis.evals.check tasks --provider zai-coding-plan --model glm-5.3-flash
```

The Vis agent reviews each task. The command shows a table of the checks and writes
`check_report.json` in a job folder under `jobs/vis-check/`. Each check makes paid model calls. Correct each task that fails a check before you run it.

## Step 4: Run the tasks

Run all tasks of the folder with the Vis agent:

```bash
export HARBOR_TELEMETRY=off
harbor run --path tasks \
  --agent blockether.vis.evals.agent:VisAgent \
  --env blockether.vis.evals.environment:HostEnvironment \
  --model zai-coding-plan/glm-5.3-flash \
  --jobs-dir jobs --job-name first --n-attempts 1
```

`HARBOR_TELEMETRY=off` stops Harbor from sending usage data. `--n-attempts` sets the trials for
each task. [pass@k and pass^k](#repeated-attempts) need at least k attempts for each task. More
attempts give a smaller interval but cost more. To test a Vis checkout, add `--ak
source=PATH`. To test a raw native image, add `--ak native_bin=PATH`.

For all options of `harbor run`, read the Harbor guide [Run a
job](https://docs.harborframework.com/jobs/run-a-job). When you start the run from Python with
[`run_tasks`](evals-api.md#run-the-tasks), Vis also writes the [round
report](#report-the-round) after the run.

## Step 5: Read the results

Harbor writes one job folder. It holds one folder for each trial:

```text
jobs/first/
  result.json
  add-function__AbC123/
    result.json
    agent/trajectory.json
    verifier/reward.json
    verifier/grade.json
```

| File | Content |
|---|---|
| `result.json` | The reward, the times, the tokens and the Harbor exception of the trial. |
| `agent/trajectory.json` | Each step of the agent in ATIF format: the code, the output and the errors. |
| `verifier/reward.json` | `reward` is 1.0 for a pass and 0.0 for a failure. The other keys are the rewards of each check. |
| `verifier/grade.json` | The evidence of the grader, for example the missing texts. |

To see the trials in a browser, run `harbor view jobs`. Read the Harbor guide [View job
results](https://docs.harborframework.com/results/view-job-results).

### Report the round

One command gives all numbers of the round:

```bash
python -m blockether.vis.evals.report jobs/first
```

The command prints these parts:

- The three [pass rates](#scores-and-intervals) with 95% intervals.
- pass@k and pass^k for each k, when each task has more than one attempt.
- The outcomes and the [failure causes](#failure-causes), with the near misses.
- Cohen's kappa and Krippendorff's alpha, when [labels or judge
  samples](#check-the-causes-with-labels) exist.
- The tokens, the time and the estimated cost.

It also writes the same data as JSON to `jobs/first/vis-report.json`. This is the start of the
report of a round with 16 tasks and two attempts for each task:

```text
Round full2 - dataset unknown, model claude-opus-5-5

Score
  Pass rate          12/16 tasks  75.0%  95% CI 50.5-89.8%
  Strict pass rate   12/16 tasks  75.0%  95% CI 50.5-89.8%
  Attempt pass rate  24/32 attempts  75.0%  95% CI 50.5-89.8%
  Detectable change  47.8 points

Repeats (2 attempts per task)
  k  tasks  pass@k  pass^k
  1  16     75.0%   75.0%
  2  16     75.0%   75.0%
```

Each task passed both attempts or failed both, so pass@2 equals pass^2. For the same reason, the
attempt interval is as wide as the task interval. A second round of 16 tasks can detect only a
change of 48 points or more.

| Option | Effect |
|---|---|
| `--tasks DIR` | Reads the dataset folder. The strict pass rate then also counts the tasks without a trial. |
| `--model M` | Counts only the attempts of this model. |
| `--round NAME` | Sets the round name. The default is the folder name. |
| `--labels FILE` | Reads the cause labels. The default is `labels.json` in the job folder. |
| `--judge DIR` | Reads the samples of a judge job. The default is the job folder. |
| `--stage-labels FILE` | Reads the stage labels. The default is `stage-labels.json` in the judge folder. |
| `--json` | Prints the JSON, not the text. |
| `--no-write` | Does not write `vis-report.json`. |

The command ends with exit status 1 when the folder holds no Harbor trials.

## Step 6: Score and compare

The report of Step 5 is enough for one round. The [Evaluations API](evals-api.md) gives each part
in Python, and also compares two runs:

1. [Summarize the round](evals-api.md#summarize-a-round) from the job folder.
2. [Score it](evals-api.md#score-a-round) with 95% intervals.
3. [Find the failure causes](evals-api.md#find-the-failure-causes).
4. [Compare it with another run](evals-api.md#compare-two-runs).

The next sections explain the numbers that these steps give.

## How evaluations work

A benchmark round is a set of Harbor jobs on one dataset, for example Terminal-Bench. Each job
holds trials. A trial is one attempt of the agent at one task. Vis reads these files from each
trial folder:

| File | What Vis reads |
|---|---|
| `result.json` | The reward, the times, the tokens, the cost and the Harbor exception type. |
| `verifier/ctrf.json` | The number of passed and total verifier tests. |
| `agent/trajectory.json` | The agent trajectory in ATIF format: the model, the steps, the tool calls and their error types. |

The round summary keeps names, counts, error types and times. It never copies a prompt, a message,
a log line or a credential. So you can share a summary without the trial folders.

An attempt is **scored** when Harbor verified it and the agent did real work:

- The trajectory names the model that you pinned for the round.
- The model gave output tokens. An attempt where the provider refused every call measures the
  runner, not the agent.
- An attempt that reached the agent time limit is scored, also without a trajectory.

Each task counts once, by its latest scored attempt. A task without a scored attempt is in
`unscored_model_tasks`.

## Scores and intervals

The report gives three pass rates:

| Rate | What it counts |
|---|---|
| Pass rate | Solved tasks out of scored tasks, by the latest attempt of each task. |
| Strict pass rate | Solved tasks out of all dataset tasks. An unscored, unattempted or GPU task counts as failed. |
| Attempt pass rate | Solved attempts out of all scored attempts. The attempts of one task form one cluster. |

Each rate has a 95% interval. A small round has a wide interval. The report also gives
`detectable_change_points`: a second round of the same size cannot detect a smaller change. The
attempts of one task are not independent, so the attempt interval uses clustered standard
errors. More attempts make each task result less noisy. They do not replace more tasks.

### Repeated attempts

When each task has more than one attempt, the report gives two rates for each k from 1 to the
number of attempts:

| Rate | The question that it answers |
|---|---|
| pass@k | Does the agent solve the task in at least one of k attempts? |
| pass^k | Does the agent solve the task in all k attempts? |

Vis estimates both from all attempts of a task, not only from the first k. A task with n
attempts and c solved attempts gives these values. The report shows the mean over the tasks.

```text
pass@k = 1 - C(n - c, k) / C(n, k)
pass^k = C(c, k) / C(n, k)
```

`C(n, k)` is the number of ways to choose k attempts from n. For example, a task that solved 2
of 4 attempts has pass@2 = 1 - 1/6 = 83% and pass^2 = 1/6 = 17%.

- At k = 1, both rates are c / n.
- pass@k goes up with k, and pass^k goes down. A large gap shows an agent that is not
  consistent.
- Use pass@k when one success is enough, for example when a person picks the best result.
- Use pass^k when each run must succeed, for example in an automation.

`unstable_tasks` lists the tasks with both solved and failed attempts. Read them first.

Cost values use metered API prices. They are not subscription charges.

## Failure causes

The diagnosis sorts the failed tasks by cause, with the largest cause first. Each attempt gets one
outcome, then one cause:

| Cause | When it applies |
|---|---|
| `dead_tools` | The attempt repeated one error until its end, for example a lost sandbox. |
| `agent_errors` | Harbor reported an exception, or the trajectory ended with an error. The outcome is `agent_error:<type>`. |
| `agent_timeouts` | The attempt reached the agent time limit. |
| `early_stops` | The agent finished by itself and failed the tests, after at most 25% of its time limit. |
| `unscored` | The attempt ended without a verifier result. |
| `other` | The agent used its time and failed the tests. |

The diagnosis also lists near misses. A near miss is a failed task that passed at least 80% of
its verifier tests. Rewards are all or nothing, so a near miss scores zero. Read its trajectory
first.

## Check the causes with labels

The causes come from rules, and a rule can be wrong. To check them, a person reads failed attempts
and gives each one a cause, a label. Save the labels with
[`save_label`](evals-api.md#check-the-labels), or write `labels.json` in the job folder:

```json
{
  "add-function__AbC123": {"cause": "early_stops"},
  "add-function__XyZ789": {"cause": "other"}
}
```

Vis then compares the labels with the automatic causes. It gives two measures that correct for
agreement by chance:

- **Cohen's kappa** compares two raters, here the rules and the person.
- **Krippendorff's alpha** also accepts more than two raters and missing values.

| Labels and kappa | What it means |
|---|---|
| Fewer than 30 labels | The kappa is not stable yet. Label more failed attempts. |
| Kappa below 0.6 | Do not trust the automatic causes. Correct the rules first. |
| Kappa from 0.6 to 0.8 | Use the causes with a review. Read the disagreements first. |
| Kappa 0.8 or more | You can use the causes without a review. |

Only a person can add a label. Label the attempts without reading the automatic cause first, or
the cause suggests the label.

### Check a judge

A judge is a model that reads trajectories. It sorts each failed trial into a stage:
`understand`, `write`, `verify` or `other`. Run it with three samples for each trial:

```bash
python -m blockether.vis.evals.judge jobs/first --provider P --model M --samples 3
```

The judge writes `judge_stages.json` in a new job folder under `jobs/vis-judge/`. Give that folder
to the report:

```bash
python -m blockether.vis.evals.report jobs/first --judge jobs/vis-judge/judge-20260102-000000
```

The report then gives two checks:

- **The sample agreement** is the share of trials where all samples give the same stage. Its
  alpha corrects it for chance. Correct the rubric of a judge that disagrees with itself before you
  compare it with people.
- **The stage labels** compare the judge with a person. Save them in `stage-labels.json` in the
  judge folder with `save_stage_label`, or write the file, for example
  `{"add-function__AbC123": {"stage": "write"}}`.

| Krippendorff's alpha | What it means |
|---|---|
| Below 0.667 | The samples disagree too much. Make the rubric clearer before you use them. |
| From 0.667 to 0.8 | Use the results only for tentative conclusions. Read the trials where the samples differ. |
| 0.8 or more | You can use the stages of the judge. |

## Compare two runs

A comparison pairs the cases that both runs scored. A case is one task with one agent and one
model. The comparison gives the difference in points with a 95% interval and a verdict:

- `better` or `worse` when the interval excludes zero.
- `no measurable difference` in all other cases.

It also gives an exact McNemar test for each route, and lists the cases that became better or
worse. When the two runs share no case but each run used one route, Vis pairs the cases by task.
It then adds a caveat, because the model and the harness changed together.

## Leaderboard position

Vis reads a public board from Harbor Hub, for example Terminal-Bench 4.0.0. It places your round
on the board by its pass rate and its strict pass rate, each with a range of possible ranks. The
position also lists caveats, for example when the board rows ran more trials for each task.

## References

- Anthropic, [Demystifying evals for AI
  agents](https://www.anthropic.com/engineering/demystifying-evals-for-ai-agents): task design,
  pass@k and pass^k, isolated trials and judge calibration.
- Anthropic, [Define success criteria and build
  evaluations](https://docs.claude.com/en/docs/test-and-evaluate/develop-tests): automatic
  grading and rubrics for model judges.
- Evan Miller, [Adding error bars to evals](https://www.anthropic.com/research/statistical-approach-to-model-evals)
  ([arXiv:2411.00640](https://arxiv.org/abs/2411.00640)): intervals, clustered errors, paired
  comparisons and power.
- Mark Chen and others, [Evaluating large language models trained on
  code](https://arxiv.org/abs/2107.03374), 2021: the unbiased pass@k estimator.
- Shunyu Yao and others, [τ-bench](https://arxiv.org/abs/2406.12045), 2024: the pass^k metric.
- Jacob Cohen, "A coefficient of agreement for nominal scales", *Educational and Psychological
  Measurement* 20, 1960: Cohen's kappa.
- Klaus Krippendorff, [Computing Krippendorff's
  alpha-reliability](https://repository.upenn.edu/asc_papers/43/), 2011: the alpha and its
  limits 0.667 and 0.8.

## See also

- [Evaluations API](evals-api.md) — write, run, score, diagnose and compare from Python or HTTP.
- [Python SDK](python-sdk.md) — install the SDK and read one run with its trajectory.
- [Sessions](sessions.md) — follow one task while it runs.
- [Harbor documentation](https://docs.harborframework.com/) — tasks, jobs, verifiers and the ATIF
  trajectory format.
