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

State: The clean-main Linux-amd64 native build passed an ABI and Python smoke test in a published task image. Its source commit and bundle checksum are in ignored `runs/provenance.json`. The isolated adapter and resumable queue have 22 passing tests; Council and drafts are disabled. Two staged trials passed their verifiers (React and interleaved Vigenère); Bun, foodstuff, and FreeCAD impeller completed with reward 0. HTML, FreeCAD platform drawing, the solo photonic retry, and FreeCAD spring clip hit Vis model `max-tokens-exceeded` errors. CAD model completed 77 iterations before an HTTP transport I/O error without a provider status code. The original photonic trial was canceled when its music-harmony peer exhausted container memory. These attempts remain unscored rather than fabricated as failures; three setup failures remain separate. The last report had five verified trials out of 13 attempts with mean reward 0.4 and is stale until the active jobs finish.

The CAD-model and FreeCAD-impeller staged pair has finished. A separate resumable queue is running the remaining CPU tasks in batches of one or two, checking disk and preserving each job; its first batch is waiting for layout-config-recreation, which is still doing CPU-intensive browser work. Nine complete gzip traces have been converted to lossless, SHA-256-verified zstd archives; the two interrupted music and photonic traces remain incomplete gzip files. Full traces, timing, tokens, metered-price estimates, verifier results, and provenance live under ignored `jobs/` and `runs/`. Unused images, ten clean merged old worktrees, and a VM trim reclaimed host storage. Next: inspect finished batches, resume after any disk or model-error stop, summarize all CPU attempts, and list the three GPU-required tasks separately.
