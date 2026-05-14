# Vis dev benches

`dev/benches/` holds model-facing benchmark harnesses for Vis autoresearch.

| Bench | Primary metric | Runner |
|-------|----------------|--------|
| `swe-bench` | SWE-bench Lite `resolved_pct` | `./dev/benches/swe-bench/run_subset.sh` |
| `4clojure` | 4Clojure `pass_pct` | `./dev/benches/4clojure/run_subset.sh` |

```bash
./dev/benches/run.sh swe-bench
./dev/benches/run.sh 4clojure
./dev/benches/test.sh
```
