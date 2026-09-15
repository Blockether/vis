---
name: ponytail
description: >
  Find the simplest correct solution for a coding task. Also covers
  over-engineering reviews, repository audits, and deliberate-shortcut reports.
  Use only when you explicitly request Ponytail or one of these workflows.
license: MIT
---

# Ponytail

Build the simplest correct solution that meets the request. Reduce concepts,
maintenance, and dependencies, not readability or required behavior.

Use `/skill:ponytail` with your task, or ask for a review, audit, or debt report.
This applies to the requested task only, not as an automatic or persistent mode.

## Choose the smallest sufficient solution

Understand the requested behavior, relevant code, and callers first. Then stop
at the first option that meets the actual requirements:

1. **Skip unnecessary work.** Do not build for hypothetical future needs.
2. **Reuse the codebase.** Look for an existing owner, helper, or pattern.
3. **Use the standard library or platform.** Prefer built-in behavior to custom
   code: CSS over layout JavaScript, database constraints over duplicate checks.
4. **Use an installed dependency** when it fits. Add a dependency only when its
   benefit outweighs the maintenance cost and simpler options fall short.
5. **Write the smallest readable implementation** that handles the real cases.

Fix bugs at their root cause, in the shared owner when appropriate, rather than
patching the same symptom in each caller. Check the affected paths; a small diff
in the wrong place is not a simpler solution.

## Keep the change honest

- Avoid speculative abstractions, configuration, scaffolding, and extension
  points. Delete unnecessary code instead of wrapping it in another layer.
- Prefer boring, readable code. Do not force one-liners, compress formatting,
  or spread an unrelated cleanup beyond the requested scope.
- Preserve correctness, security, trust-boundary validation, data-loss protection,
  accessibility, and explicitly requested behavior. Do not ship a reduced feature
  as if it fulfilled the request; explain a real trade-off or blocker.
- Use the project's existing test infrastructure. Reproduce bugs before fixing
  them, keep regression tests, and run affected tests and required format/lint
  checks. Simplicity is not permission to replace coverage with a demo.
- Repository rules and the user's scope take precedence. This skill grants no
  permission to commit, push, release, or take other external actions.

## Review or audit

Review the requested diff; for an audit, inspect the requested repository scope.
Report findings without applying fixes unless asked. Verify usage and contracts
before calling code or dependencies unnecessary.

Rank actionable findings by impact. For each, give `path:line`, what to remove
or simplify, the concrete replacement (or nothing), and why behavior is preserved.
Look for dead code, duplicated built-ins, unused dependencies, and speculative
layers. If nothing is worth changing, say so. Flag serious correctness or security
issues separately; never recommend removing safeguards just to shorten the code.

## Record real shortcuts

For an accepted simplification with a known limit, leave a source comment:
`ponytail: <limit>; revisit when <trigger>; <upgrade path>`.
Do not annotate every ordinary simple choice.

When asked for debt, collect these source comments within the requested scope,
excluding dependencies and generated output. Report `path:line`, limit, trigger,
and upgrade path; flag missing details and count the markers. Do not create a
ledger file unless asked.

## Report the result

Keep the summary short: what changed or was found, checks run, and any known
limits or omitted work. Give more detail when requested. Report only actual
counts or measurements; do not invent savings, baselines, or benchmark claims.
