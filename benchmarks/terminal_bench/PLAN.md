# Terminal-Bench 4.0: Vis native on Z.ai Coding Plan

Phrase: Compare Vis as a complete agent on the public Terminal-Bench 4.0 dataset without changing its runtime.

Context: Build from a Git archive of remote main 5b8b6fd923736aed00e508590dc4543f976a5c38; unrelated working-tree edits cannot enter the native image. The macOS build passed, and Harbor’s published amd64 task images need a Linux-amd64 bundle. Keep runner data and credentials outside Git. Coding Plan subscription charges are not metered API billing.

1. Build and pin a Linux-amd64 native bundle.
   - Rationale: macOS and Linux arm64 binaries cannot execute inside Harbor’s published amd64 task containers.
   - Data: source SHA, bundle SHA, image stamp, platform and sidecar smoke test.
   - Acceptance: binary and Python sidecar execute in a representative task container.
   - Unknowns: none for the published CPU image used in the successful bundle smoke test.
2. Implement and test the Harbor installed-agent adapter.
   - Rationale: retain Harbor's real task environment and verifier.
   - Data: isolated per-trial home, Council and drafts off, only coding-plan credentials at exec time, trace JSONL and Harbor metrics.
   - Acceptance: configuration and metric tests pass, no credentials in job config or tracked files.
   - Unknowns: none; a completed Harbor trial validated credential delivery and the trace result, including gzip extraction in the second batch.
3. Run one or two tasks, inspect results, then the next two.
   - Rationale: catch integration failures before consuming plan quota.
   - Data: trial and verifier JSON, full trace, token usage, estimated cost, timing and errors.
   - Acceptance: each trial has a real completion and verifier result with interpretable usage.
   - Unknowns: plan quota and task runtimes.
4. Continue across the dataset when representative trials pass.
   - Rationale: produce a meaningful product-level benchmark, not a smoke test.
   - Data: immutable dataset version, attempts, failures, missing GPU tasks, comparison caveats.
   - Acceptance: all feasible tasks are accounted for; unrun tasks have concrete reasons.
   - Unknowns: GPU sandbox availability and additional cloud charges.

State: A clean archive of main `5b8b6fd923736aed00e508590dc4543f976a5c38` produced the Linux-amd64 native bundle; the binary and Python sidecar passed smoke tests in a published task image. Its checksum and environment are recorded in ignored `runs/provenance.json`. The adapter isolates each task, disables Council and drafts, and captures full trace JSONL with credential redaction before disk writes. The benchmark suite has 45 passing tests, clean Python formatting and lint, and no Vis runtime changes. Six attempts have verified results: React and interleaved Vigenère earned reward 1; Bun, foodstuff, FreeCAD impeller, and layout-config-recreation earned reward 0. The last report covers 16 attempts and is stale until the current jobs finish. Four other trials hit `max-tokens-exceeded`, CAD model ended after 77 iterations with an HTTP I/O error, music-harmony exhausted memory, the original photonic peer was canceled, and three setup failures remain separate; none are counted as scored failures.

Eleven complete gzip traces were converted to SHA-256-verified zstd archives. When the Podman VM stopped, two `bulk-002` tasks had already called the model; their incomplete traces and exceptions are preserved, not scored. The VM was restarted and `bulk-003` is now retrying just those two tasks using the explicit, tested `--retry-task` option. The 63 CPU tasks run in resumable batches of at most two with disk checks; three GPU-only tasks cannot run on the current Podman machine. All jobs, logs, traces, trial and verifier JSON, time and token metrics, metered-price estimates, and provenance live in ignored `jobs/` and `runs/`. Coding Plan billed costs are unknown, not the metered-price estimate. Legacy artifacts are private: old traces were archived without sanitization and must not be shared without review. Unrelated images, clean merged old drafts, and a VM trim reclaimed storage. Next: inspect the retry batch, continue the CPU queue while preserving attempts, refresh the summary, and account for the GPU tasks separately.
