---
name: issue-triage
description: "Triage GitHub issues of this repo ONE AT A TIME, reproduction-first. Use when asked to go over issues, check whether an issue is still real, verify a claimed fix, or decide close/fix/defer. Covers: fetching the issue, reproducing it in a live REPL or test run, reading the responsible source, and proposing exactly one decision per issue."
version: "1.1.0"
license: Apache-2.0
compatibility: agents
---

# issue-triage — reproduce first, decide once

Repo: `Blockether/vis`. Work **one issue per reply**. Every claim about an
issue's state comes from a tool result you just saw.

## The loop, per issue

1. **Read the issue verbatim.**
   `gh issue view <n> --repo Blockether/vis --json number,title,body,comments`
   Extract: the exact reproduction, the expected behaviour, the surface
   (tool name, namespace, CLI command).
2. **Locate the code.** `grep` for the symbol/message from the issue body,
   scoped to `src/` or `extensions/`. Then `struct_index` the hit file and
   `cat` only the responsible body.
3. **REPRODUCE IT LIVE — this step is mandatory.** Order of preference:
   - `repl_eval("clojure", …)` in the managed REPL: call the real fn/tool
     handler with the issue's arguments and show the actual return value.
     After disk edits `(require 'the.ns :reload)` first.
   - `run_tests("clojure", {namespaces […]})` for the narrowest test ns.
   - `shell` running the real CLI (`clojure -M:test …`, `vis python -m pytest …`)
     when clean process state matters.
   Record the observed output as the verdict's evidence. When a live
   reproduction is genuinely impossible (needs a provider, a device, a
   release), say so explicitly and name the substitute evidence.
4. **Verdict** — exactly one of:
   - **FIXED** — reproduction now shows the expected behaviour. Quote the
     commit/line that fixed it and the passing check.
   - **REAL** — reproduction still shows the bug. Quote the failing output
     and name the file:line that owns it.
   - **PARTIAL** — some cases pass, some fail. List which.
   - **UNREPRODUCIBLE** — say what was tried and what is missing.
5. **Decision, offered to the user** — one recommendation plus its cost:
   `close` (with the comment text), `fix now` (with the concrete patch plan
   and the test that will prove it), or `defer` (with the blocker).
   Wait for the user's answer before editing production code.

## Reply shape

```
### #<n> — <title>
**Claim:** one line from the issue.
**Repro:** the command/eval that was run, and its real output (trimmed).
**Verdict:** FIXED | REAL | PARTIAL | UNREPRODUCIBLE — because <evidence>.
**Owner:** path/to/file.clj:LINE
**Decision:** close | fix now | defer — <one line of why>.
```
Keep it under ~15 lines. One issue per reply, then stop and wait — except
after a close, where the next issue's triage continues in the same reply (see
**Advance to the next issue**).

## Repo contracts to honour while triaging

- Tests are **Lazytest**; use `run_tests` with the smallest namespace, and
  `only` entries are fully qualified `defdescribe` vars.
- The managed Clojure REPL runs already-loaded Vars: `(require 'ns :reload)`
  every changed production ns and then every changed test ns before
  `run_tests`; restart the REPL when a clean load is safer.
- Wire keys are snake_case strings, engine keys kebab-case keywords — check
  `gateway/wire.clj` before calling a wire-shaped repro wrong.
- Python tooling repros go through the in-process ruff FFI and
  `run_tests("python")` / `vis python -m pytest <paths>`.
- Stop every REPL and background shell you started before answering.

## After a verified fix

Once the user has chosen `fix now`, commit the verified fix. Then add an issue
comment linking the commit and briefly stating the verification performed. Close
the issue when the reported problem is resolved for this project; otherwise
leave it open with the triage outcome. This is the only exception to waiting for
a separate user instruction before closing.

Use `gh issue comment <n> --repo Blockether/vis --body "<commit link + verification>"`,
then `gh issue close <n> --repo Blockether/vis` when it is resolved.

## Advance to the next issue

Closing an issue is not the end of the reply — it is the hand-off to the next
one. As soon as an issue is closed (either the `fix now` path above, or a
`close` the user approved), do NOT stop to ask what to triage next:

1. Report the close in one or two lines: comment URL and the `gh issue close`
   confirmation.
2. Pick the next target immediately — the next open issue in the batch the user
   named, else `gh issue list --repo Blockether/vis --state open --json number,title`
   and take the lowest open number not yet triaged in this session.
3. Run the full loop on it in the SAME reply: read verbatim, locate, reproduce
   live, verdict, decision. The reply ends on that issue's `**Decision:**`.
4. Then stop and wait, because the decision still needs the user's answer
   before any production edit or close.

Only the DECISION waits for the user; discovering, reproducing and judging the
next issue never does. Stop early only when the batch is exhausted (say so and
list what was triaged), when a repro needs something unavailable, or when the
user said to stop.
