# Vis-powered SWE-bench Lite

This directory wires Vis to the official [SWE-bench](https://github.com/princeton-nlp/SWE-bench) harness
so Vis can be scored on **SWE-bench Lite resolution rate** and autoresearch can use
that percentage as a primary metric.

## Layout

| File | Purpose |
|------|---------|
| `subset.py` | Materializes a deterministic stratified subset of SWE-bench Lite into `instances.json`. Run once; commit the result. |
| `instances.json` | **Frozen** task list for the inner loop. Never edit by hand. |
| `vis_agent.py` | Bridge: for one instance, checks out the repo, calls `vis run`, returns the unified diff. |
| `run_subset.sh` | Driver: solves every instance, stores per-instance Vis artifacts, runs the SWE-bench evaluator, writes `results/<iter>/summary.json`. |
| `metrics.py` | Reduces predictions + SWE-bench report → `summary.json` and `METRIC name=value` lines. |
| `requirements.txt` | Minimal Python dependencies for the harness. |
| `results/` | Per-iteration predictions, summaries, evaluator reports, and `artifacts/<instance>/vis.stdout.json` iteration traces. Gitignored. |

## One-time setup

```bash
# Python env (any 3.10+). Pinned for reproducibility.
python -m venv dev/benches/swe-bench/.venv
source dev/benches/swe-bench/.venv/bin/activate
pip install -r dev/benches/swe-bench/requirements.txt

# Materialize the frozen N=30 subset (deterministic seed)
python dev/benches/swe-bench/subset.py --split lite --n 30 --seed 42 \
  --out dev/benches/swe-bench/instances.json
git add dev/benches/swe-bench/instances.json
git commit -m "autoresearch: freeze SWE-bench Lite N=30 subset"
```

## Two-tier loop

- **Inner loop** (autoresearch iterations): `SWEBENCH_SUBSET=inner` → frozen N=30.
- **Validation** (manual, periodic): `SWEBENCH_SUBSET=lite` (or `full`) → all 300 Lite tasks.

Switch via env var in `autoresearch.sh` or invoke `run_subset.sh` directly:

```bash
SWEBENCH_SUBSET=lite ./dev/benches/swe-bench/run_subset.sh
```

For cheap harness checks without Docker evaluation:

```bash
SWEBENCH_SUBSET=lite SWEBENCH_MAX_INSTANCES=1 SWEBENCH_EVALUATE=0 \
  ./dev/benches/swe-bench/run_subset.sh
```

## Quota guardrail

`run_subset.sh` has a best-effort budget guard via `VIS_REQUEST_BUDGET`
(default 300 for inner, 2500 for lite/full). Autoresearch should log partial
runs as `crash` so an incomplete evaluation does not get falsely credited.
