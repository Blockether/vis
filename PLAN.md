# Settings editing across the Companion and TUI

Make the scope, effect and saved value clear before you apply a change.

## Context

The gateway catalog and sparse overrides already own settings. This task extends
`config/scoped.clj`, `sandbox/scoped_policy.clj` and gateway settings routes rather
than introducing a second configuration store. The Companion uses
`apps/vis-companion/src/screens/settings/`; the TUI uses `dialogs.clj` and its client.

Current problems: access fields are JSON strings, TUI text editing is one-line,
ancestor overrides disable editing, forms lose drafts on refresh, and editing,
service operations and device preferences share inconsistent controls.

Rejected alternatives: duplicating catalogs in clients, rewriting authored YAML,
copying inherited settings, saving every dependent field separately, widening host
permissions in a local scope, and including credentials in presets or exports.

## 1. Contract, validation and atomic persistence

Rationale: both clients must show and save the same configuration safely.

Data: canonical gateway/config schemas, scoped resolver, atomic file writers and
SQLite settings storage.

Acceptance criteria: typed editors, inherited and own values, application timing,
versioned atomic batches, explicit values, field errors, scope isolation and
rejected policy expansion. Existing single-key callers retain their current API.

Unknowns: exact transaction and configuration writer boundaries are being checked.

## 2. Companion editing and navigation

Rationale: the phone and desktop need clear scope and task navigation.

Data: SettingsScreen, MachineSettings, ScopedSettingsDialog, shared typed editors,
gateway client and settings drafts.

Acceptance criteria: structured access forms, staged configuration, protected drafts,
conflict recovery, inline errors, shared search and categories, explicit sources,
local preferences and service actions kept separate, keyboard and back navigation.

Unknowns: existing interaction tests must be updated to the new authorized behavior.

## 3. TUI editing and navigation

Rationale: a compact list must not force complex configuration into one input line.

Data: settings catalog projection, list/details/editor views, virtual terminal tests.

Acceptance criteria: staged values, Ctrl+S, discard guard, typed and multiline editors,
inheritance preview, editable ancestor values, preserved selection/search, useful
small-terminal layouts and source/effect details.

Unknowns: verify both narrow and wide layouts with the existing virtual screen.

## 4. Resources, presets and documentation

Rationale: managing a service is different from setting its availability.

Data: MCP/providers/speech/notifications, profile import/export and configuration guide.

Acceptance criteria: task-oriented navigation, clear device/gateway ownership,
existing lifecycle progress/errors retained, safe presets/import/export, and updated
user guidance without credentials or private deployment information.

Unknowns: presets must remain explicit draft changes, never hidden remote actions.

## 5. Verification and publication

Rationale: complete the behavior across API, clients and the built application.

Data: Lazytest, Companion Vitest/Storybook, formatter/linter/reflection, native build
and affected native tests, final scoped Git diff.

Acceptance criteria: regressions reproduced before fixes, affected checks and builds
pass, unrelated changes preserved, task commits pushed to main and CI reported.
No product release, live restart or store submission is authorized by this task.

Unknowns: broader failures are classified against the scoped diff, never bypassed.

## Plan state

- All five phases are complete and verified; publication to `main` follows these checks.
- Base: bf769fa9b on `main` (initial HEAD b5c4538c9; peer commits landed meanwhile).
- Phase 1: `PATCH /v1/settings` applies a typed, revision-checked batch in one
  SQLite transaction. A stale revision returns 409, an invalid batch returns 400,
  and neither writes anything.
- Phase 2: the Companion machine and scoped dialogs share one draft, review and
  apply editor with typed fields, inherited values and settings profiles.
- Phase 3: the TUI Settings dialog uses the same draft, review and apply flow.
- Phase 4: `resources/vis-docs/settings.md` guide, updated configuration reference,
  and profile limits read from the gateway schema.
- Phase 5: backend and TUI Lazytest, clj format and lint, Companion typecheck, lint,
  unit and Storybook tests, `npm run build`, native build, `native_settings_test`,
  `native-tui-resize-test` and the docs page canon tests pass.
- Unrelated: the Storybook `IterationTrace` "Joined Activity" story fails at the base
  (it expects "2 checks"; `ActivityPanel` shows "verifications"). Reported in Council #7500.
- No product release, live restart or store submission is part of this task.
