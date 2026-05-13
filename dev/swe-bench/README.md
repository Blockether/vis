# SWE-bench harness for Vis autoresearch

This directory wires Vis to the official [SWE-bench](https://github.com/princeton-nlp/SWE-bench) harness
so autoresearch can use **% resolved on SWE-bench Lite** as a primary metric.

## Layout

| File | Purpose |
|------|---------|
| `subset.py` | Materializes a deterministic stratified subset of SWE-bench Lite into `instances.json`. Run once; commit the result. |
| `instances.json` | **Frozen** task list for the inner loop. Never edit by hand. |
| `vis_agent.py` | Bridge: for one instance, checks out the repo, calls `vis run`, returns the unified diff. |
| `run_subset.sh` | Driver: solves every instance, runs the SWE-bench evaluator, writes `results/<iter>.json` and a `summary.json` consumed by `autoresearch.sh`. |
| `metrics.py` | Reduces `summary.json` → `METRIC name=value` stdout lines. |
| `results/` | Per-iteration artifacts. Gitignored. |

## One-time setup

```bash
# Python env (any 3.10+). Pinned for reproducibility.
python -m venv dev/swe-bench/.venv
source dev/swe-bench/.venv/bin/activate
pip install 'swebench==2.1.*' 'datasets>=2.19' 'unidiff>=0.7'

# Materialize the frozen N=30 subset (deterministic seed)
python dev/swe-bench/subset.py --split lite --n 30 --seed 42 \
  --out dev/swe-bench/instances.json
git add dev/swe-bench/instances.json
git commit -m "autoresearch: freeze SWE-bench Lite N=30 subset"
```

## Two-tier loop

- **Inner loop** (autoresearch iterations): `SWEBENCH_SUBSET=inner` → N=30
- **Validation** (manual, periodic): `SWEBENCH_SUBSET=full` → N=300 (all Lite)

Switch via env var in `autoresearch.sh` or invoke `run_subset.sh` directly:

```bash
SWEBENCH_SUBSET=full ./dev/swe-bench/run_subset.sh
```

## Quota guardrail

`run_subset.sh` aborts the iteration when the cumulative request count exceeds
`VIS_REQUEST_BUDGET` (default 300 for inner, 2500 for full). Autoresearch logs
the partial run as `crash` so the iteration doesn't get falsely credited.
