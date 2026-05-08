# Autoresearch Ideas

- Add Harbor/Terminal-Bench native agent adapters for Vis and opencode instead of the current host-agent-uses-docker-exec bridge. Cleaner logs and closer leaderboard compatibility.
- Add Terminal-Bench task metadata sync script that refreshes difficulty/category/timeouts from upstream `task.yaml` into `bench/opencode/tasks.jsonl`.
- Add Aider Polyglot via Terminal-Bench adapter for multi-language edit tasks after the Docker runner is stable.
- Add SWE-bench Lite/Verified adapter as slower validation: agents produce patches, official Docker harness grades them.
- Add per-task rotating schedule in `autoresearch.sh` so the loop cannot overfit to `TB-hello-world` or `TB-jsonl-aggregator`.
- Add parser tests using one captured `opencode run --format json` artifact and one `vis run --json --trace` artifact.
