# Session inspection and Council A/B benchmark

These ten Harbor tasks use synthetic sessions. They cover distinct workflows, not a random sample. The fixtures replace the session content, IDs, routes, paths, and numerical values.

## Cases

Each folder is named `introspection-<case>`. Read `instruction.md` for the exact question. `tests/checks.json` holds the answer, not the model-visible task workspace.

| Case | Measures |
| --- | --- |
| `route-fallback` | Provider routing without printing the full session. |
| `recover-block` | Exact scoped stdout recovery; distractor output in the preceding iteration. |
| `previous-final` | Final result separated from interim assistant prose. |
| `council-attribution` | Council batch attribution from persisted transcript rather than user request. |
| `provider-wait` | Structured per-iteration progress versus a manual NDJSON scan. |
| `request-origin` | Event attribution and scope; compare start versus adjacent events. |
| `task-settlement` | Task lifecycle projection; distinguish a failed Task from a successful turn. |
| `engine-log-cause` | Log-only negative control: provider wait is nearby but not the worker cause. |
| `council-handoff` | Live child prompt and report recovery on the first attempt. |
| `council-resume` | Live follow-up prompt and whether one reused assistant retains the right task boundary. |

## Layout and isolation

- The cases are in `bench/introspection/tasks/introspection-<case>/`, not in `e2e/`: the standard agent has no seeded session and cannot pass them. Run them with `InspectionAgent`.
- `bench_agent:InspectionAgent` uses the normal Vis agent/Harbor host environment. It seeds a new private trial database before the first model request; fixture construction uses the product SQLite backend and real persisted form envelopes. The saved data is a different session from the measured agent, so seed tokens do not count. The same harness checkout seeds both arms. Only the exact synthetic IDs appear in the workspace.
- Fallback rows for wait, request origin, and Task settlement are scoped NDJSON with realistic distractors. The same journal is seeded in the trial home, and in the readable workspace `diagnostics` path. Both arms can use this fallback. The log case is a deliberately bounded worker-log negative control. The request-origin case may already be answerable from persisted Council attribution; it checks that the answer remains correct without a redundant raw read.
- Real Council verifier tasks exercise `council.run`, the child prompt, and same-child follow-up without any embedded answer. The Harbor grader requires exact JSON plus one owned child and the expected number of completed gateway Task attempts. These are the only cases that make additional model calls when run in Harbor.
- Neither the validator nor the runner reads the operator's database, events or logs. The `VIS_HOME` of a trial is separate. The runner writes only the provider key environment reference to trial config. Use a named API-key provider supported by both arms; do not copy config or credentials into these files. Custom gateways and OAuth are not configured by this runner.

## Free checks

Use the Vis SDK Python environment. Prepare each source checkout with `clojure -X:deps prep :aliases '[:vis]'`. Set `VIS` to the checkout that contains this benchmark. The validator runs Clojure but does not run Harbor or call a model.

```bash
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH="$VIS/packages/vis-agent/src" \
  "$VIS/packages/vis-agent/.venv/bin/python" "$VIS/bench/introspection/validate.py" "$VIS" --tmp-dir "$TMPDIR"
```

It reads the Harbor task layout and grader specs, checks good/wrong structured answers, reads all four DB-backed answers through the real model projection, and checks the raw journal/log oracles. For Council, it checks both verifiers against accepted and missing host evidence and saved child reports; live attempts require the paid run. It also checks the comparator against fabricated Harbor job artifacts and exercises the runner with a stub CLI, including token and request-accounting capture.

## Measure the payload and prompt size

Run the static benchmark with no model calls. Pass the absolute path of the checkout to measure.

```bash
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH="$VIS/packages/vis-agent/src" \
  "$VIS/packages/vis-agent/.venv/bin/python" "$VIS/bench/introspection/measure_static.py" "$BASELINE"
```

The JSON gives character and UTF-8 byte counts for the default session data, its Python display, and the three prompt sections. It also gives the rendered Council input for 20 iterations with one reply due. The two live Council cases use the task text and the same synthetic parent history. The report gives the Svar estimates for both inherited prefixes, or `null` when the checkout does not support the experiment. These counts are not provider tokens.

## Paid A/B

Run this only with approval for model calls. Use the same SDK, fixture seeder, provider, model, reasoning effort, extension settings, runner, and concurrency in both arms. Set `VIS_BENCH_API_KEY` privately in the process environment. Use absolute paths for the checkouts and the jobs directory. Run at least three attempts per task if budget permits and alternate arm order to reduce load and cache bias. Avoid a job-level `VIS_HOME` override on the Vis CLI; the runner sets the trial's home.

```bash
export HARBOR_TELEMETRY=off VIS_BENCH_SOURCE="$VIS"
BENCH="$VIS/bench/introspection"
export PYTHONPATH="$BENCH:$VIS/packages/vis-agent/src"
common=(--path "$BENCH/tasks"
        --agent bench_agent:InspectionAgent
        --env blockether.vis.evals.environment:HostEnvironment
        --model "$ROUTE" --n-attempts 3 --jobs-dir "$JOBS")
"$VIS/packages/vis-agent/.venv/bin/harbor" run "${common[@]}" --ak "source=$BASELINE" --job-name baseline
"$VIS/packages/vis-agent/.venv/bin/harbor" run "${common[@]}" --ak "source=$CANDIDATE" --job-name candidate
"$VIS/packages/vis-agent/.venv/bin/python" "$BENCH/compare.py" "$JOBS/baseline" "$JOBS/candidate"
```

For the handoff experiment, use the same candidate checkout for both arms and select only `introspection-council-handoff` and `introspection-council-resume`. Set `VIS_BENCH_HANDOFF=full` for the first arm and `VIS_BENCH_HANDOFF=bounded` for the second. The setting applies to newly created children.

Use checkout A and checkout B for prompt replacement; `--ak append_system=FILE` only measures an appended instruction. To attribute savings independently, compare baseline with (1) projection only, (2) prompt only, then (3) both.

## Read the comparison

- Correctness comes first. Review attempts, exact passes, missing verifications and routes before interpreting savings. The report shows token medians for both correct-only and all observed trials, with observed sample counts. Failures and absent accounting are not zero-token wins. Report both per-case and workload-stratified results; do not present these ten deliberately selected cases as a rate for the population.
- ATIF prompt/input includes provider-cached input; report cached and uncached separately. Output includes provider-reported completion tokens. The report splits leader and descendants and sums them as team totals. Iteration counts come from Vis when present, otherwise from metered agent steps. Council follow-up means two attempts but one child trajectory.
- `prompt-accounting.json` contains counts only: Vis's Svar estimate for each persisted request, tagged by prepared/logical projection, with labels such as core system, built-in tool rules, and session context. It is not a provider invoice. Its core/system labels do not isolate Council prose from neighboring instructions unless the engine attributes them separately. Use the first request as a prompt-size signal and the sum as workload cost. Do not sum this local estimate with provider input tokens. A new child also stores the full and bounded estimates of its inherited prefix on the model selected at creation. The estimates exclude the same delegated task and current child context in both arms.
- A Python return object is not itself model input. If the agent stores `read_session` in a variable and prints one field, a smaller return object may save host transfer and filtering, but not model tokens on that call. `tool_stdout_chars` reports observed saved stdout size; provider usage measures the prompts actually billed.
- Operation counts prefer recorded Activity when present and otherwise count Python call sites. Raw journal/log reads are AST-plus-filename **candidate blocks**, not syscall counts. Computed paths, aliases, dynamic loops and commands can under/overcount. Compare after spot-checking fallback cases; the comparator does not print any code, messages or logs. Trial artifacts themselves include synthetic model and tool text.
