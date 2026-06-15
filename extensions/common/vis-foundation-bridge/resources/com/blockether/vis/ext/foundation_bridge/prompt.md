`br_*` Bridge verification tools keep verification work inside the Vis tool surface.
Use them instead of shelling out to `bb bridge` or editing Bridge-owned state by hand.

Main workflow:

- Use `br_init()` only when Bridge is not configured for this workspace. It is idempotent and returns the active profile path when setup already exists.
- Use `br_check()` first for status, convergence, or "what evidence is missing?" questions. It also accepts opts: `br_check({"changed_files": ["path", ...]})` scopes impact analysis to known edited files.
- Use `br_next(opts?)` after `br_check()` to get the immediate recommended action as a `br_*` operation. Prefer this suggestion over inventing shell commands.
- Use `br_list_evidence(opts?)` to inspect configured runnable evidence commands and their ids before running one.
- Use `br_run_evidence(id, opts?)` only when the configured command should actually run and write a Bridge receipt.
- Use `br_run_evidence(id, {"is_dry_run": True})` to preview the execution plan without running the command or writing a receipt.

Useful opts use Python dict spelling:

- `{"profile": "/abs/path/to/.bridge/profile.edn"}` overrides profile discovery.
- `{"policy": "/abs/path/to/.bridge/verification-policy.yaml"}` overrides policy discovery.
- `{"changed_files": ["src/foo.clj"]}` scopes `br_check()` and `br_next()` impact analysis.
- `{"subject": "core"}` narrows evidence execution when the command supports subjects.
- `{"out_dir": ".bridge/ephemeral/evidence"}` or `{"out": ".../receipt.yaml"}` controls receipt output for real evidence runs.
- `{"timeout_seconds": 300}` overrides command timeout.
- `{"is_dry_run": True}` previews `br_run_evidence` only; do not report it as completed evidence.

Reading results:

- When answering status questions, summarize the returned map instead of pasting it raw.
- Prefer `status`, `issue_count`, `counts`, `required_obligations`, `recommended_obligations`, `evidence_receipts`, and `next_action` when they are present.
- Call out open or failed obligations, stale artifacts, subject problems, and evidence receipts that are already present.
- Keep policy obligations and runnable evidence ids distinct: for example `unit-tests` is not necessarily the runnable command id `unit`.
- If `configured?` is false or status is `unconfigured`, call `br_init()` before trying `br_run_evidence`.
