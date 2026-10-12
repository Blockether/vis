# Evaluations on Harbor

Run every Vis evaluation through Harbor, keep trajectories on this machine and decide harness changes with paired statistics.

## Context

The previous plan, Automations, is complete and verified (phases 1 to 6, native test `2687ca926`).
Git history keeps that plan.

Vis has four evaluation runners, and each one writes its own result format:

- `e2e/run.py` runs 40 editing scenarios from `e2e/scenarios/<id>/scenario.json` on the host. It starts the source CLI (`clojure -M:vis`) with `--full-trace-json-stream`. Results go to `~/.vis/evals/runs/<run_id>/`.
- `~/curated-terminal-benchmark/terminal_bench/` runs the full Terminal-Bench queue for Vis on Harbor 0.23.0.
- `~/curated-terminal-benchmark/ctb/` runs 12 curated Terminal-Bench tasks for 7 agents and seals each round in `results/<round>.json`.
- `~/.vis/tmp/subagent-bench/` holds a one-time subagent benchmark.

`.vis/extensions/evals.py` gives the `evals.*` tools. It starts the first two runners. It also has the statistics: 95% intervals with tasks as clusters, paired comparisons, power and Cohen's kappa for failure labels.

Problems:

- You cannot compare an e2e result with a Terminal-Bench result.
- The Vis Harbor adapters write no ATIF `trajectory.json`. They copy only the token counts. `harbor view`, `harbor analyze` and trajectory criteria do not see the Vis steps.
- Two Vis adapters do the same work: `ctb/agents/vis.py` and `terminal_bench/vis_agent.py`.
- Harness components have no named switches. A component ablation is not possible.
- Harbor sends usage telemetry by default, with agents, models, rewards and durations. Nothing in our tools prevents an upload to Harbor Hub.
- Container runs need a Linux native build. The benchmark uses v0.2.37, which does not have the Council subagent API (`104c91899`). A dry run of `native-release.yml` uploads only build logs, not the binary.

Evidence: arXiv 2609.20804, "An Empirical Study of Harness Design for Coding Agents", keeps the agent loop fixed and changes one component at a time: planning, action space and context management.
It runs SWE-Bench Verified and Terminal-Bench 2.1 through Harbor and reports success rate and cost together.
It compares settings with two-sided exact McNemar tests on paired tasks and the Benjamini-Hochberg procedure (FDR 0.05).
LLM judges label failure stages and action types, and human labels validate the judges with Cohen's kappa.

Harbor facts (0.23.0 installed, 0.24.0 latest):

- `Job.create(JobConfig)` and `Job.run()` return a `JobResult`. Trial hooks run on start, environment start, agent start and end, verification start, end and cancel.
- `JobConfig` takes several agents, datasets, attempts, retries and metrics in one job.
- `--agent`, `--env` and `--plugin` accept a `module:Class` import path.
- A task is `task.toml`, an instruction, an environment and `tests/`. It can have steps, network rules for each phase, MCP servers, skills and a separate verifier.
- `harbor-rewardkit` 0.2.3 gives file, command, data, HTTP and trajectory criteria, and LLM or agent judges with `samples` and `guard`.
- ATIF v1.8 has steps, tool calls, observations, step metrics, `is_copied_context` and `subagent_trajectories`.
- `harbor job regrade` grades a job again without a new agent run. `harbor view`, `harbor analyze` and `harbor check` read jobs, trajectories and tasks.
- Telemetry turns off only with `HARBOR_TELEMETRY=off`. There is no configuration file for it.

Decisions:

- Harbor is the only runner. A Harbor job directory is the only raw result format.
- Vis writes ATIF itself. Every consumer (Harbor, e2e and other tools) gets the same trajectory.
- Trajectories and results stay on this machine. No tool of ours uploads them.
- A round is one Vis build, one profile set and one Harbor version. A sealed round never changes.
- A harness change is accepted only through a paired test on held-out tasks and a person's approval.

Rejected alternatives:

- A new runner in Vis: Harbor already owns tasks, containers, verifiers, retries, job results and trajectories.
- An ATIF converter in the benchmark repository: e2e also needs ATIF, and the engine owns the trace.
- Harbor Hub as result storage: it sends trajectories off this machine.
- A self-improvement loop that changes Vis without a gate: it fits the benchmark, not the work.

## 1. Private trajectories

Rationale: trajectories hold source code, prompts and command output, and they can hold credentials. They must stay on this machine.

Data:

- Every Harbor process that our tools start gets `HARBOR_TELEMETRY=off`. In the benchmark repository, `sitecustomize.py` also sets it for each Python process with the repository on `PYTHONPATH`. A running round then stops telemetry at its next trial.
- Our Harbor commands refuse the Hub flags `--upload`, `--public`, `--share-org`, `--launch` and `--stream`.
- Vis redacts credentials in `trajectory.json`: known provider credentials and environment values whose names contain `KEY`, `TOKEN`, `SECRET`, `PASSWORD` or `CREDENTIAL`. The Harbor adapter then removes the exact secret values that it gave to the trial.
- Job directories get mode `0700`. `jobs/` and `runs/` stay out of git. A sealed result has no trajectory content.
- Claude Code in Harbor already sets `CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1`.
- Harbor 0.24 comes after the running baseline round is sealed. A round never changes its Harbor version.

Acceptance criteria: tests check the process environment, the refused flags, the redaction and the directory mode. A planted fake credential does not appear in `trajectory.json`.

Unknowns: none.

## 2. ATIF trajectories from Vis

Rationale: Harbor tools read ATIF. With ATIF, Vis trials appear in `harbor view`, `harbor analyze`, rewardkit trajectory criteria and `harbor-atif2otel`.

Data:

- The `result` frame of `--full-trace-json-stream` has each iteration: thinking, prose, code blocks, stdout, error, tool call id, times and the final answer. `tokens` and `cost` hold the totals.
- `vis-agent --trajectory PATH` writes ATIF v1.8 when the run ends. `src/com/blockether/vis/internal/loop/trajectory.clj` owns the mapping.
- The prompt is the first `user` step. Each iteration is one `agent` step, with `reasoning_content`, `model_name` and `reasoning_effort`.
- Each code block is one tool call `python_execution` with the argument `code`. Its stdout and error are the observation.
- The final answer is the message of the last agent step. Totals go to `final_metrics`.
- Vis facts go to `extra`: iteration, provider, block status, Activity counts, repairs, timeouts, session id and profile.
- Council children go to `subagent_trajectories` (phase 3).

Acceptance criteria: Lazytest tests cover the mapping and the redaction. A host run with `--trajectory` passes validation with Harbor's `Trajectory` model. `harbor view` shows its steps. The token totals equal the result frame.

Unknowns: the trace has no token counts for each iteration, so step metrics can stay empty.

## 3. One Vis agent, profiles and a current Linux build

Rationale: one adapter gives one behavior. Named profiles make each harness component a switch, and the profile is part of each result.

Data:

- `ctb/agents/vis.py` stays the only Vis adapter. `terminal_bench/` uses it.
- Current switches: `--toggles` (for example `council`, `draft_backend`, `subagents` and `reasoning_level`), `--extensions`, `--reasoning-effort` and `--db :memory` or `--persist`.
- `profiles.toml` names each profile: toggles, extensions, effort and a configuration overlay. The profile goes to ATIF `agent.extra` and to the run record.
- `native-release.yml` dry runs upload the Linux binary as a workflow artifact with a short retention. The benchmark pins that archive by sha256, run id and commit. This is not a release.

Acceptance criteria: one canary task each for `vis-solo` and `vis-team` on a build from `main`, with valid ATIF and the profile in it. The second adapter is removed.

Unknowns: none. The engine has toggles for reasoning, Council, subagents, drafts, introspection and shell commands. It has no switch for compaction, the context budget, fold hints, planning or the action space. Phase 8 adds them as engine toggles.

## 4. e2e scenarios as a Harbor dataset

Rationale: one runner for all evaluations. Harbor gives retries, job results, `view` and `regrade`.

Data:

- `scenario.json` keys: `want`, `wantnot`, `want_answer`, `want_answer_json`, `want_json_files`, `want_tools`, `want_forms`, `want_requested_route`, `want_folded_prefix`, `want_cache_read`, `want_cache_metrics`, `want_helper_reuse`, `measurement`, `files_from`, `fixture_generator`, `timeout_s` and `lang`.
- A Harbor environment for the host (`--env module:Class`) makes a new git repository for each trial, as `e2e/run.py` does now. It uses the source CLI, so no Linux build is necessary.
- A compiler makes one Harbor task from each `scenario.json`: `task.toml`, `instruction.md`, the files and `tests/`.
- The verifier reads the workspace and `trajectory.json`, and writes named rewards: correctness and each process guard. Rewardkit criteria are used where one fits.

Acceptance criteria: one Harbor run of all scenarios is graded by the old and the new grader, and they agree on 100% of the verdicts. Then `evals.run("scenarios", ...)` uses Harbor, and `e2e/run.py` is removed.

Unknowns: some e2e guards read Activity data that the trace may not hold. Such a guard gets its data in ATIF `extra` first.

## 5. One result store and one statistics module

Rationale: the same code and the same rules compare any two runs.

Data:

- A Harbor job directory has `config.json`, `result.json` and trials with `result.json`, `agent/trajectory.json` and `verifier/reward.json`.
- The SDK module `blockether.vis.evals` reads Harbor jobs and holds the statistics: cluster intervals, exact McNemar, Benjamini-Hochberg, power and cost. The `evals` extension and the benchmark repository import it.
- Every round has a seal: commit, pins, profiles, artifact sha256, task checksums and one row for each trial.
- Vis keeps an index of runs that it can build again from the files. It does not keep a copy.

Acceptance criteria: `evals.compare` reads a benchmark round and an e2e round with the same code. McNemar and Benjamini-Hochberg values equal reference values.

Unknowns: none.

## 6. Evaluations in Vis

Rationale: a person starts, watches and compares evaluations from chat, the TUI and the Companion app. A real failed session becomes a new scenario.

Data:

- `evals.run(dataset, agents, profiles, attempts)` starts a Harbor job in the background.
- `evals.watch(run_id)` follows a run in a live view: its progress, counters and one row for each finished trial. The view reads the run directory, so the run continues when a person stops the view.
- The TUI and the Companion app show this live view like every other live view.
- `evals.capture(session, turn)` makes a task draft from a session: instruction, files and rubric. A person approves the rubric.
- An automation runs the e2e dataset every night and reports a regression.

Acceptance criteria: a run starts from chat, its progress shows live and its result is sealed. `harbor check` accepts a captured task. Each new TUI fold has arrow-key tests.

Unknowns: none for live progress. Rejected: a Harbor plugin `VisLive` with its own gateway route. The run directory already holds every trial result, and a live view needs no new route, authentication or client code.

## 7. Trajectory judges

Rationale: a pass rate does not tell why a run fails.

Data:

- Rubrics label the failure stage and the action type of each tool call: understand, write, verify and other, as in the paper.
- `harbor analyze` or rewardkit judges read `trajectory.json`, with `samples` for agreement and `guard` against prompt injection.
- People label failures with `evals.label`. Cohen's kappa compares each judge with the people.

Acceptance criteria: each rubric has at least 30 human labels. A judge with kappa of 0.6 or more is used for review. Automatic causes need kappa of 0.8 or more.

Unknowns: the judge cost and the time for human labels.

## 8. Ablations and the improvement gate

Rationale: find which component helps which model, and at which cost.

Data:

- Axes:
  - context management (compaction off and on, thresholds and fold hints) for each context budget,
  - planning on and off,
  - all tools or the shell only,
  - solo or Council,
  - effort.
- New engine toggles give the axes that have no switch today: compaction, the context budget, fold hints, planning and the action space.
- The tasks are split into a working set and a held-out set.
- `evals.ablate` plans a run before it starts: cost, time and statistical power.
- A change needs a paired test on the held-out set and a person's approval. The test must show a gain, or a lower cost without a loss.
- An agent can propose a change from the paper and the diagnoses. The gate makes the decision.

Acceptance criteria: one complete ablation family on held-out tasks, with a sealed report.

Unknowns: the budget, the number of tasks for enough power and the Podman machine time.

## Plan state

- Phase 1: done. The benchmark commit `22223a5` stays local until the baseline round is sealed.
- Phase 2: done. A host run with `--trajectory` passed Harbor's `Trajectory` model, and `harbor view` showed its 3 steps. Its totals equaled the result frame. A planted fake credential did not appear in the file, and the file mode was `0600`.
- Phase 3: done. Vis `a93089b62` writes the steps of a persisted run. The native-release dry run `38185257847` built it, and `profiles.toml` pins that archive by sha256 (benchmark commit `bf1ae0b`, local). In round `canary-p3b`, `vis-solo` and `vis-team` each wrote valid ATIF v1.8 with 15 steps, mode `0600` and the profile in `agent.extra`. Both trials had reward 0: each one failed 1 of 2 task tests, with no infrastructure error. `terminal_bench/vis_agent.py` is removed.
- Phase 4: done. One Harbor run graded all 40 scenarios with the old and the new grader on the `anthropic-coding-plan/claude-opus-5-5` route. The verdicts agreed in 40 of 40 trials: 37 passed and 3 failed in both, with no trial exception. `evals.run("scenarios", ...)` starts `e2e/vis_harbor/run.py` as one Harbor job, and `e2e/run.py` is removed.
- Phase 5: done. Release v0.2.38 ships `blockether.vis.evals` in `vis-agent` 0.2.38 on PyPI. The McNemar and Benjamini-Hochberg tests equal the binomial and statsmodels reference values. `evals.compare` pairs scenario runs and benchmark rounds with the same code (4 tests in `.vis/extensions/test_evals.py`). With the real e2e runs `20261006-005150-scenarios-ec87` and `20261012-085548-scenarios-d4c4`, it paired 5 cases with no measurable difference. The benchmark repository reads and seals rounds with the SDK (commit `812b2d5`, local).
- Phase 6: in progress. `evals.run(dataset, ...)` replaces `run_scenarios` and `run_bench`. It takes the datasets `scenarios` and `terminal-bench`, and it refuses an option of the other dataset. `evals.capture(session_id, turn)` drafts a scenario from a session turn through `vis.read_session` (commits `513895774` and `f050802a6`). `evals.watch(run_id)` follows a run in a live view. On the real run `20261012-085548-scenarios-d4c4`, it showed 40 trials and "39 of 40 runs passed". Open: the nightly automation and `harbor check` of a captured task.
- Phases 7 and 8: not started.
- The baseline round (`ctb-baseline`) has no Vis trials. It runs until about 2026-10-13. After its seal, commit `results/baseline.json` in the benchmark repository.
