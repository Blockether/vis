# Autoresearch Ideas

- Add Harbor/Terminal-Bench native agent adapters for Vis and opencode instead of the current host-agent-uses-docker-exec bridge. Cleaner logs and closer leaderboard compatibility.
- Add Terminal-Bench task metadata sync script that refreshes difficulty/category/timeouts from upstream `task.yaml` into `bench/opencode/tasks.jsonl`.
- Add Aider Polyglot via Terminal-Bench adapter for multi-language edit tasks after the Docker runner is stable.
- Add SWE-bench Lite/Verified adapter as slower validation: agents produce patches, official Docker harness grades them.
- Add per-task rotating schedule in `autoresearch.sh` so the loop cannot overfit to `TB-hello-world` or `TB-jsonl-aggregator`.
- Add parser tests using one captured `opencode run --format json` artifact and one `vis run --json --trace` artifact.
- TUI: replace `identityHashCode`-based render caches with content fingerprints. `wrap-text` keys on `[(System/identityHashCode text) max-width]` but `(str/trim s)` returns a NEW String when it actually trims - so any caller that does `(wrap-text (str/trim s) w)` pays the full wrap cost on every call. Surfaced during live-stream perf work in `render.clj` `format-iteration-entry-entries` stdout path (~47 ms per call for 6 KB stdout). Same fragility applies to `format-clojure-ansi`, `format-answer-with-thinking-data`, `bubble-height` — all key on `identityHashCode`. Use the bounded fingerprint helper `(text-fingerprint s)` already added to `render.clj`. Likely 5-10x further win on live ticks with long stdout/stderr blocks.
