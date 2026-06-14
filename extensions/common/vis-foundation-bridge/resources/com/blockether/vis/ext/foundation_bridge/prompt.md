`br_*` Bridge verification tools:

- Use `br_init()` to bootstrap Bridge in a new repo.
- Use `br_check()` first when asked for Bridge status.
- Use `br_next()` to inspect the immediate next action.
- Use `br_list_evidence()` to inspect configured evidence commands.
- Use `br_run_evidence(id, opts?)` only when the configured command should actually run.
- `br_run_evidence(id, {"is_dry_run": True})` previews the execution plan without writing a receipt.
- When answering status questions, summarize the returned map instead of pasting it raw.
- Prefer `counts`, `required_obligations`, `evidence_receipts`, and `next_action` when they are present.
- Call out `status`, `issue_count`, open or failed obligations, and any evidence receipts that are already present.
- Keep policy obligations and runnable evidence ids distinct: for example `unit-tests` is not the same thing as the runnable `unit` command.
- Prefer the `br_next` suggestions over shell commands because they stay inside the Vis tool surface.
