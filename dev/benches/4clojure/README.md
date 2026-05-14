# Vis-powered 4Clojure bench

This bench asks Vis to solve frozen 4Clojure problems by writing one Clojure
form to `solution.edn`. The harness substitutes that form for `__`, runs the
problem tests on the JVM Clojure CLI, and reports **pass rate** for autoresearch.
It evaluates model-generated code locally, so run it only in a dev/sandboxed
context.

Artifacts for each run live under `dev/benches/4clojure/results/<ITER_TAG>/`:
`predictions.jsonl`, `summary.json`, and one JSON trace per task in `traces/`.

## Run

```bash
./dev/benches/4clojure/run_subset.sh
FOURCLOJURE_MAX_INSTANCES=4 ./dev/benches/4clojure/run_subset.sh
FOURCLOJURE_SOLVE=0 FOURCLOJURE_MAX_INSTANCES=1 ./dev/benches/4clojure/run_subset.sh
```

## Refresh frozen data

```bash
python3 dev/benches/4clojure/download_problems.py \
  --out dev/benches/4clojure/problems.json
python3 dev/benches/4clojure/subset.py --n 30 --seed 42 \
  --out dev/benches/4clojure/instances.json
```
