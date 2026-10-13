# Evaluations

An evaluation measures how well an agent solves benchmark tasks. You run the tasks with
[Harbor](https://harborframework.com), then Vis scores the results, finds where the agent loses
tasks and compares two runs. Vis reads only standard Harbor files, so it works for any agent that
Harbor runs.

## When to use

- **You want to run Vis on benchmark tasks.** Read [Run Vis in Harbor](#run-vis-in-harbor).
- **You want the score of a benchmark round, with its uncertainty.** Read [Scores and
  intervals](#scores-and-intervals).
- **You want to know why the agent failed tasks.** Read [Failure causes](#failure-causes).
- **You must decide if the automatic causes are correct.** [Check them with
  labels](#check-the-causes-with-labels).
- **You changed the agent and want to know if it is better.** [Compare two
  runs](#compare-two-runs).
- **You want to see where your agent stands on a public board.** Read [Leaderboard
  position](#leaderboard-position).
- **Your own program must do these steps.** Read [Evaluations API](evals-api.md).

To watch a single session while it runs, use [Sessions](sessions.md). To run a prompt on a
schedule, use [Automations](automations.md).

## Run Vis in Harbor

Vis is a native Harbor agent. Harbor runs any standard task folder with Vis, so you convert
nothing. You need Python 3.12 or later, an installed Vis and a configured provider. Each trial
makes paid model calls.

1. Install the SDK with its Harbor extra:

   ```bash
   pip install 'vis-agent[harbor]'
   ```

2. Run a dataset folder, for example the `e2e` folder of the Vis repository:

   ```bash
   harbor run --path e2e \
     --agent blockether.vis.evals.agent:VisAgent \
     --env blockether.vis.evals.environment:HostEnvironment \
     --model zai-coding-plan/glm-5.3-flash
   ```

The host environment runs each trial in a new git repository on your computer, without
containers. The agent runs the installed `vis-agent` CLI with its own database for each trial.
To test a Vis checkout, add `--ak source=PATH`. To test a raw native image, add
`--ak native_bin=PATH`.

Each Vis task is a standard Harbor task folder:

| Path | Content |
|---|---|
| `task.toml` | The agent and verifier time limits. |
| `instruction.md` | The prompt of the trial. |
| `environment/workspace/` | The seed files of the trial repository. |
| `environment/workspace_filesystem.json` | Optional. Maps a filesystem root id to a workspace folder. |
| `tests/checks.json` | The expected files, answer, code blocks and tools. |
| `tests/test.sh` | Runs the SDK grader `python -m blockether.vis.evals.grade`. |

The grader writes `reward.json` with the verdict and one reward for each check. It also writes
`grade.json` with the evidence. To review the tasks, run `python -m blockether.vis.evals.check`.
To find the stage where failed trials went wrong, run `python -m blockether.vis.evals.judge`.
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
`detectable_change_points`: a second round of the same size cannot detect a smaller change. When
each task has more than one attempt, the report also gives pass@k, the share of tasks that solved
at least one attempt.

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
and gives each one a cause, a label. Vis then compares the labels with the automatic causes. It
uses Cohen's kappa, which corrects for agreement by chance.

| Labels and kappa | What it means |
|---|---|
| Fewer than 30 labels | The kappa is not stable yet. Label more failed attempts. |
| Kappa below 0.6 | Do not trust the automatic causes. Correct the rules first. |
| Kappa from 0.6 to 0.8 | Use the causes with a review. Read the disagreements first. |
| Kappa 0.8 or more | You can use the causes without a review. |

Only a person can add a label. The same check works for an automatic judge of trajectories. The
judge sorts each failure into a stage: `understand`, `write`, `verify` or `other`.

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

## See also

- [Evaluations API](evals-api.md) — summarize, score, diagnose and compare rounds from Python or HTTP.
- [Python SDK](python-sdk.md) — install the SDK and read one run with its trajectory.
- [Sessions](sessions.md) — follow one task while it runs.
