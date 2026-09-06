# Session health in app metrics

Ship the approved view with measured session data, never demonstration values.

## Context
The Companion health component and stories already render the approved layout. The usage
endpoint currently returns lifetime totals only. Extend the existing request/usage pipeline;
do not reconstruct a prompt from files on disk, add polling to every list row, or deploy.

## 1. Capture and persist request health
- Rationale: health belongs to one actual request, not cumulative billing.
- Data: context renderer, loop request boundary, persisted iteration metadata.
- Acceptance criteria: request-aligned budget, limit and prompt estimates; guidance access
  distinct from loaded instructions; missing measurements stay absent; regression tests.
- Unknowns: smallest existing durable metadata slot and actual prompt assembly seam.

## 2. Connect the usage endpoint and Companion
- Rationale: reuse the on-demand metrics read and the approved UI.
- Data: SQLite usage query, gateway wire projection, SessionUsage, SessionStatsPanel.
- Acceptance criteria: historical and active sessions read measured facts; no fixtures in
  production; totals remain separate; tests cover absent/stale data and the client boundary.
- Unknowns: whether historical requests retained enough metadata for a partial snapshot.

## 3. Verify
- Rationale: prove behavior across the engine, persistence, gateway and UI boundary.
- Data: affected Lazytest/Vitest/Storybook tests, lint/reflection, formatting, app build.
- Acceptance criteria: affected checks pass; approved layout preserved; no deployment,
  gateway restart, commits or unrelated work changed.
- Unknowns: test/runtime availability.

## Plan state
1–3 complete locally. Request health is captured without prompt contents and persisted with
provider input; the existing usage endpoint feeds Companion on demand. Historical budgets
stay absent. Linked-root instruction read receipts are not available and remain unknown.

Verification: 799 affected backend cases passed across loop, SQLite, prompt and gateway
suites; after the final arithmetic-only fix, all 38 prompt cases passed again. Seven Clojure
files pass formatting and lint/reflection with no findings. Companion affected unit tests,
14 Storybook cases, typecheck and React compiler lint pass; production build passes.
Historical and unknown-read states were inspected at 393×852 with annotated screenshots.
No deployment, gateway restart, commit or push. Unrelated local changes were preserved.
