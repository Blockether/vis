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

State: A clean archive of main `5b8b6fd923736aed00e508590dc4543f976a5c38` produced the Linux-amd64 native bundle; the binary and Python sidecar passed smoke tests in a published task image. Its checksum and environment are recorded in ignored `runs/provenance.json`. The adapter isolates each task, disables Council and drafts, and captures full trace JSONL with credential redaction before disk writes. The benchmark suite has 59 passing tests, clean Python formatting and lint, and no Vis runtime changes.

`runs/summary.json` covers 27 attempts: 20 CPU tasks are scored and 3 are solved (15%). React, interleaved Vigenère and protein-autointerp-disulfide earned reward 1. Bun, foodstuff, FreeCAD impeller, layout-config-recreation, medical-claims-processing and vpp-loss-divergence failed their tests; medical-claims-processing matched 114 of 116 claim lines and vpp-loss-divergence passed 4 of 5 tests, but rewards are all or nothing. layout-config-recreation2 reached the 8-hour agent timeout. Five tasks stopped with `max-tokens-exceeded` and five lost the provider stream mid-response with an HTTP I/O error; payments-pipeline-fix and vllm-deepseek-streaming lost theirs 80 ms apart after 86 s without answer text, a shared provider or network event. In each output-limit trial, one model call reasoned up to svar's 32,768-token cap for this model; Vis then retried it with a 16,384-token limit and stopped when the retry hit that cap too. Harbor still verifies trials that end with an agent error. Vis usage omits calls that never produced a parsed response, so the summary estimates their output from stream chunks: about 356k tokens beside 1.55M recorded. Music-harmony is unscored: the container killed Vis with exit 137, so Harbor ran no verifier. In music-harmony and the layout-config-recreation2 retry, the Python worker crashed mid-run; Vis retires a crashed worker until the next turn, a benchmark run is a single turn, and the model kept calling the dead tool for 181 and 384 iterations, about 6 h 15 min of the layout retry. Two Podman VM interruptions, one canceled peer and three setup failures are preserved but not scored. `uefi-bootkit` runs alone because it needs the whole 16 GiB memory budget; 41 more CPU tasks are pending, and three GPU-only tasks cannot run on the current Podman machine.

Agent timeouts are scored attempts: Harbor verifies them although Vis writes no final result, so the queue neither re-runs them nor stops on their missing Vis metrics. The queue keeps two tasks running while their memory fits the 16 GiB budget and starts the next one when a slot frees, because two-task batches left a slot idle for hours behind the slower trial; only two consecutive agent errors that each end within 10 minutes stop it. After 27 attempts, pulled task images filled the Podman VM (58 images, 54.6 GB) and its disk file took 78 GB on the host; the free-space guard then stopped new starts without saying so while a peer finished. The queue now removes each validated task's environment and verifier images, trims the VM disk so the host gets the blocks back, and prints why it stops starting tasks. Complete gzip traces are converted to SHA-256-verified zstd archives; incomplete ones are preserved. All jobs, logs, traces, trial and verifier JSON, time and token metrics, metered-price estimates, and provenance live in ignored `jobs/` and `runs/`. Coding Plan billed costs are unknown, not the metered-price estimate. Legacy artifacts are private: old traces were archived without sanitization and must not be shared without review. Next: continue the CPU queue, refresh the summary as tasks finish, decide whether the runner should end a trial once its Python worker is retired, and account for the GPU tasks separately.
